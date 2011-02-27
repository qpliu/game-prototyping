module Apps
    (GameApp(..),gameApp,
     App,UserId,
     chatApp,chatGameApp,echoApp)
where

import Control.Concurrent
    (MVar,modifyMVar_,newMVar,putMVar,readMVar,takeMVar,
     forkIO,killThread,threadDelay)
import Data.Map(Map,delete,elems,empty,insert,toList)
import qualified Data.Map
import Data.Time(UTCTime,NominalDiffTime,diffUTCTime,getCurrentTime)

import Server(App(..),UserId,User,serve,userId,userName,userWrite)

echoApp :: IO App
echoApp = return App {
    appName = "echo",
    appHelp = [],
    appAddUser = \ _ -> return (),
    appRemoveUser = \ _ -> return (),
    appProcessLine = \ user line -> userWrite user [line],
    appUserRenamed = \ _ _ -> return ()
    }

chatApp :: IO App
chatApp = do
    users <- newMVar empty
    return App {
        appName = "oldChat",
        appHelp = [],
        appAddUser = chatAddUser users,
        appRemoveUser = chatRemoveUser users,
        appProcessLine = chatProcessLine users,
        appUserRenamed = chatUserRenamed users
        }
  where
    broadcast users user message = do
        userList <- fmap elems (readMVar users)
        sequence_ (map (flip userWrite [userName user ++ message]) userList)
    chatAddUser users user = do
        modifyMVar_ users (return . insert (userId user) user)
        broadcast users user " has joined"
    chatRemoveUser users uid = do
        user <- fmap (Data.Map.lookup uid) (readMVar users)
        modifyMVar_ users (return . delete uid)
        maybe (return ()) (flip (broadcast users) " has left") user
    chatProcessLine users user line = broadcast users user (':':line)
    chatUserRenamed users user oldName = do
        modifyMVar_ users (return . insert (userId user) user)
        broadcast users user (" renamed from " ++ oldName)

class GameApp game where
    gameName :: game -> String

    -- gameAddPlayer
    -- parameters:
    --    game state
    --    UserId of player to be added
    --    get current user name from UserId
    --    current time
    -- result:
    --    (updated game state,public message,messages to specific players)
    gameAddPlayer :: game -> UserId -> (UserId -> String) -> UTCTime
                          -> (game,[String],[(UserId,[String])])

    -- gameRemovePlayer
    -- parameters:
    --    game state
    --    UserId of player leaving
    --    get current user name from UserId
    --    current time
    -- result:
    --    (updated game state,public message,messages to specific players)
    gameRemovePlayer :: game -> UserId -> (UserId -> String) -> UTCTime
                             -> (game,[String],[(UserId,[String])])

    -- gameCommand parameters:
    --    game state
    --    UserId of user taking action
    --    get current user name from UserId
    --    current time
    --    command arguments
    -- result:
    --    (updated game state,public message,messages to specific players)
    gameCommands :: [(String,game -> UserId -> (UserId -> String)
                                  -> UTCTime-> [String]
                                  -> (game,[String],[(UserId,[String])]))]

    -- gamePollTime
    -- parameters:
    --    game state
    --    current time
    -- result:
    --    time for next poll
    gamePollTime :: game -> UTCTime -> Maybe UTCTime

    -- gamePoll
    -- parameters:
    --    game state
    --    get current user name from UserId
    --    current time
    -- result:
    --    (updated game state,public message,messages to specific players)
    gamePoll :: game -> (UserId -> String) -> UTCTime
                     -> (game,[String],[(UserId,[String])])

gameApp :: GameApp game => game -> IO App
gameApp game = do
    gameState <- newMVar game
    users <- newMVar empty
    return App {
        appName = gameName game,
        appHelp = map fst gameAppCmds,
        appAddUser = gameAppAddUser users gameState,
        appRemoveUser = gameAppRemoveUser users gameState,
        appProcessLine = gameAppProcessLine users gameState,
        appUserRenamed = gameAppUserRenamed users
        }
  where
    notify users defaultMessage specificMessages = do
        userList <- fmap toList (readMVar users)
        sequence_ (map (notifyUser defaultMessage specificMessages) userList)
    notifyUser defaultMessage specificMessages (uid,user) =
        userWrite user (maybe defaultMessage id (lookup uid specificMessages))
    getNameMap users =
        fmap (\ userMap uid ->
                    maybe (show uid) userName (Data.Map.lookup uid userMap))
             (readMVar users)
    gameAppCmds = gameCommands
    gameAppAddUser users gameState user = do
        modifyMVar_ users (return . insert (userId user) user)
        nameMap <- getNameMap users
        utcTime <- getCurrentTime
        game <- takeMVar gameState
        let (newGame,defaultMessage,specificMessages) =
                gameAddPlayer game (userId user) nameMap utcTime
        updatePoll users gameState newGame utcTime
        putMVar gameState newGame
        notify users defaultMessage specificMessages
    gameAppRemoveUser users gameState uid = do
        nameMap <- getNameMap users
        utcTime <- getCurrentTime
        game <- takeMVar gameState
        let (newGame,defaultMessage,specificMessages) =
                gameRemovePlayer game uid nameMap utcTime
        updatePoll users gameState newGame utcTime
        putMVar gameState newGame
        notify users defaultMessage specificMessages
        modifyMVar_ users (return . delete uid)
    gameAppProcessLine users gameState user line = do
        utcTime <- getCurrentTime
        game <- takeMVar gameState
        (newGame,defaultMessage,specificMessages) <-
            maybe (return (game,[userName user ++ ":" ++ line],[]))
                  (runCommand users user utcTime (drop 1 (words line)) game)
                  (findCommand (words line))
        updatePoll users gameState newGame utcTime
        putMVar gameState newGame
        notify users defaultMessage specificMessages
    gameAppUserRenamed users user oldName = do
        modifyMVar_ users (return . insert (userId user) user)
        notify users [oldName ++ " is renamed to " ++ userName user]
                     [(userId user,[])]
    findCommand [] = Nothing
    findCommand (cmd:_) = lookup cmd gameAppCmds
    runCommand users user utcTime args game cmd = do
        nameMap <- getNameMap users
        return (cmd game (userId user) nameMap utcTime args)
    updatePoll users gameState game utcTime = do
        maybe (return ())
              ((>> return ()) . forkIO . schedulePoll users gameState utcTime)
              (gamePollTime game utcTime)
    schedulePoll users gameState startTime pollTime = do
        if startTime < pollTime
            then threadDelay $ fromIntegral $ round
                             $ 1000000 * diffUTCTime pollTime startTime
            else return ()
        nameMap <- getNameMap users
        utcTime <- getCurrentTime
        game <- takeMVar gameState
        let (newGame,defaultMessage,specificMessages) =
                gamePoll game nameMap utcTime
        updatePoll users gameState newGame utcTime
        putMVar gameState newGame
        notify users defaultMessage specificMessages

chatGameApp :: IO App
chatGameApp = gameApp ChatGame

data ChatGame = ChatGame

instance GameApp ChatGame where
    gameName _ = "chat"
    gameAddPlayer g uid getName _ = (g,[getName uid ++ " has joined"],[])
    gameRemovePlayer g uid getName _ = (g,[getName uid ++ " has left"],[])
    gameCommands = []
    gamePollTime _ _ = Nothing
    gamePoll g _ _ = (g,[],[])
