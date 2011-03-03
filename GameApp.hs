module GameApp
    (GameApp(..),gameApp,
     Game,gameGetName,gameGetTime,
     gameGetState,gameGetStateAttr,gameSetState,gameUpdateState,
     gameMessageToAll,gameMessageToAllExcept,gameMessageToUser,
     App,UserId,chatGameApp)
where

import Control.Concurrent
    (MVar,newMVar,putMVar,readMVar,takeMVar,
     forkIO,threadDelay)
import Data.List(find)
import Data.Time(UTCTime,NominalDiffTime,diffUTCTime,getCurrentTime)

import Server(App(..),UserId,User,userId,userName,userWrite)

newtype Game ga a = Game (GameState ga -> (GameState ga,a))

instance Monad (Game ga) where
    (Game xf1) >>= g = Game (\ state1 -> let (state2,a) = xf1 state1
                                             Game xf2 = g a
                                         in  xf2 state2)
    f >> g = f >>= const g
    return a = Game (\ state -> (state,a))

class GameApp ga where
    gameAppName :: ga -> String
    gameAppAddPlayer :: UserId -> Game ga ()
    gameAppRemovePlayer :: UserId -> Game ga ()
    gameAppCommands :: [(String,UserId -> [String] -> Game ga ())]
    gameAppPollTime :: ga -> UTCTime -> Maybe UTCTime
    gameAppPoll :: Game ga ()

data GameState ga = GameState {
    gameStateGameApp :: ga,
    gameStateUsers :: [User],
    gameStateTime :: UTCTime,
    gameStateMessages :: [(UserId,[String])]
    }

gameAppGetGameState :: Game ga (GameState ga)
gameAppGetGameState = Game (\ gameState -> (gameState,gameState))

gameAppSetGameState :: GameState ga -> Game ga ()
gameAppSetGameState gameState = Game (const (gameState,()))

gameGetName :: UserId -> Game ga String
gameGetName uid = do
    gameState <- gameAppGetGameState
    return (maybe (show uid) userName
                  (find ((== uid) . userId) (gameStateUsers gameState)))

gameGetTime :: Game ga UTCTime
gameGetTime = do
    gameState <- gameAppGetGameState
    return (gameStateTime gameState)

gameMessageToUser :: UserId -> [String] -> Game ga ()
gameMessageToUser uid message = do
    gameState <- gameAppGetGameState
    let messageList = gameStateMessages gameState
    let userMessages = maybe message (++ message) (lookup uid messageList)
    gameAppSetGameState gameState {
                            gameStateMessages =
                                (uid,userMessages) : filter ((/= uid) . fst)
                                                            messageList
                            }

gameMessageToAll :: [String] -> Game ga ()
gameMessageToAll message = do
    gameState <- gameAppGetGameState
    let userIds = map userId (gameStateUsers gameState)
    sequence_ (map (flip gameMessageToUser message) userIds)

gameMessageToAllExcept :: [UserId] -> [String] -> Game ga ()
gameMessageToAllExcept excluded message = do
    gameState <- gameAppGetGameState
    let userIds = filter (not . (`elem` excluded))
                         (map userId (gameStateUsers gameState))
    sequence_ (map (flip gameMessageToUser message) userIds)

gameGetState :: Game ga ga
gameGetState = do
    gameState <- gameAppGetGameState
    return (gameStateGameApp gameState)

gameSetState :: ga -> Game ga ()
gameSetState ga = do
    gameState <- gameAppGetGameState
    gameAppSetGameState gameState { gameStateGameApp = ga }

gameUpdateState :: (ga -> ga) -> Game ga ()
gameUpdateState update = do
    gameState <- gameAppGetGameState
    gameAppSetGameState
        gameState { gameStateGameApp = update (gameStateGameApp gameState) }

gameGetStateAttr :: (ga -> a) -> Game ga a
gameGetStateAttr attr = do
    gameState <- gameGetState
    return (attr gameState)

gameApp :: GameApp ga => ga -> IO App
gameApp ga = do
    gameApp <- newMVar ga
    users <- newMVar []
    savedPollTime <- newMVar Nothing
    return App {
        appName = gameAppName ga,
        appHelp = map fst cmds,
        appAddUser = addUser gameApp users savedPollTime,
        appRemoveUser = removeUser gameApp users savedPollTime,
        appProcessLine = processLine gameApp users savedPollTime,
        appUserRenamed = userRenamed users
        }
  where
    cmds = gameAppCommands
    notify userList messages = do
        sequence_ (map (notifyUser messages) userList)
    notifyUser messages user =
        userWrite user (maybe [] id (lookup (userId user) messages))
    stateTransition ga userList utcTime (Game update) =
        let (GameState { gameStateGameApp = newGa,
                         gameStateMessages = messages },()) =
                update GameState {
                        gameStateGameApp = ga,
                        gameStateUsers = userList,
                        gameStateTime = utcTime,
                        gameStateMessages = []
                        }
        in  (newGa,messages)
    addUser gameApp users savedPollTime user = do
        ga <- takeMVar gameApp
        userList <- fmap (user:) (takeMVar users)
        utcTime <- getCurrentTime
        let (newGa,messages) = stateTransition ga userList utcTime
                                               (gameAppAddPlayer (userId user))
        updatePoller gameApp users savedPollTime newGa utcTime
        putMVar users userList
        putMVar gameApp newGa
        notify userList messages
    removeUser gameApp users savedPollTime uid = do
        ga <- takeMVar gameApp
        userList <- takeMVar users
        utcTime <- getCurrentTime
        let (newGa,messages) = stateTransition ga userList utcTime
                                               (gameAppRemovePlayer uid)
        updatePoller gameApp users savedPollTime newGa utcTime
        putMVar users (filter ((/= uid) . userId) userList)
        putMVar gameApp newGa
        notify userList messages
    processLine gameApp users savedPollTime user line = do
        ga <- takeMVar gameApp
        userList <- readMVar users
        utcTime <- getCurrentTime
        let (newGa,messages) =
                maybe (ga,zip (map userId userList)
                              (repeat [userName user ++ ":" ++ line]))
                      (stateTransition ga userList utcTime)
                      (findCommand (userId user) (words line))
        updatePoller gameApp users savedPollTime newGa utcTime
        putMVar gameApp newGa
        notify userList messages
    findCommand userId args
      | null args = Nothing
      | otherwise = fmap (($ tail args) . ($ userId)) (lookup (head args) cmds)
    userRenamed users user oldName = do
        userList <- fmap ((user:) . (filter ((userId user /=) . userId)))
                         (takeMVar users)
        putMVar users userList
        notify userList (zip (map userId userList )
                             (repeat [oldName ++ " is renamed to "
                                              ++ userName user]))
    updatePoller gameApp users savedPollTime newGa utcTime = do
        oldPollTime <- takeMVar savedPollTime
        let newPollTime =
                getNewPollTime oldPollTime (gameAppPollTime newGa utcTime)
        if newPollTime /= oldPollTime
            then maybe (return ()) (forkPoller savedPollTime) newPollTime
            else return ()
        putMVar savedPollTime newPollTime
      where
        getNewPollTime Nothing newPollTime = newPollTime
        getNewPollTime oldPollTime Nothing = oldPollTime
        getNewPollTime (Just oldTime) (Just newTime) =
            Just (min oldTime newTime)
        forkPoller savedPollTime pollTime =
            forkIO (schedulePoll savedPollTime pollTime) >> return ()
        schedulePoll savedPollTime pollTime = do
            if utcTime < pollTime
                then threadDelay $ fromIntegral $ round
                                 $ 1000000 * diffUTCTime pollTime utcTime
                else return ()
            ga <- takeMVar gameApp
            userList <- takeMVar users
            currentTime <- getCurrentTime
            savedTime <- takeMVar savedPollTime
            if savedTime == Just pollTime
                then do
                    let (updatedGa,messages) =
                            stateTransition ga userList currentTime gameAppPoll
                    putMVar savedPollTime Nothing
                    updatePoller gameApp users savedPollTime
                                 updatedGa currentTime
                    putMVar users userList
                    putMVar gameApp newGa
                    notify userList messages
                else do
                    putMVar savedPollTime savedTime
                    putMVar users userList
                    putMVar gameApp ga

chatGameApp :: IO App
chatGameApp = gameApp Chat

data Chat = Chat

instance GameApp Chat where
    gameAppName _ = "chat"
    gameAppAddPlayer uid = do
        name <- gameGetName uid
        gameMessageToAll [name ++ " has joined."]
    gameAppRemovePlayer uid = do
        name <- gameGetName uid
        gameMessageToAll [name ++ " has left."]
    gameAppCommands = []
    gameAppPollTime _ _ = Nothing
    gameAppPoll = return ()
