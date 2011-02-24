module Server
    (serve,
     App(..),
     UserId,User,userId,userName,userApp,userWrite,userClose)
where

import Control.Concurrent(MVar,modifyMVar_,newMVar,putMVar,readMVar,takeMVar)
import Data.Char(isSpace)
import Data.Map(Map,adjust,delete,empty,insert,size)
import qualified Data.Map

import Listener(listener,makeHandler)

newtype UserId = UserId String
    deriving (Eq,Ord)

instance Show UserId where
    show (UserId id) = id

data App = App {
    appName :: String,
    appHelp :: [String],
    appAddUser :: User -> IO (),
    appRemoveUser :: UserId -> IO (),
    appProcessLine :: User -> String -> IO (),
    appUserRenamed :: User -> String -> IO ()
    }

data User = User {
    userId :: UserId,
    userName :: String,
    userApp :: App,
    userWrite :: [String] -> IO (),
    userClose :: IO ()
    }

serve :: Int -> Int -> [App] -> IO ()
serve port maxClients apps = newMVar empty >>= server port maxClients apps

server :: Int -> Int -> [App] -> MVar (Map UserId User) -> IO ()
server port maxClients apps users = listener port handler
  where
    handler addr handle = do
        (write,close) <- makeHandler (processLine (UserId addr))
                                     (cleanup (UserId addr))
                                     handle
        userMap <- takeMVar users
        if size userMap < maxClients
            then do
                putMVar users (insert (UserId addr) User {
                                        userId = UserId addr,
                                        userName = addr,
                                        userApp = nullApp,
                                        userWrite = write,
                                        userClose = close }
                                      userMap)
                write (greeting apps)
            else do
                putMVar users userMap
                write ["Exceeded maximum number of users."]
                close
    cleanup uid = do
        sequence_ (map (flip appRemoveUser uid) apps)
        modifyMVar_ users (return . delete uid)
    processLine uid line = do
        user <- fmap (Data.Map.lookup uid) (readMVar users)
        maybe (return ()) (processUserLine line) user
    processUserLine line user =
        maybe (appProcessLine (userApp user) user line)
              (($ drop 1 (words line)) . ($ user))
              (findCommand (words line))
    findCommand [] = Nothing
    findCommand (cmd:_) = lookup cmd cmds

    greeting :: [App] -> [String]
    greeting apps = ["Welcome to the server", "/help for help"]

    cmds :: [(String,User -> [String] -> IO ())]
    cmds = [("/help",helpCmd),
            ("/quit",quitCmd),
            ("/apps",appsCmd),
            ("/app",appCmd),
            ("/name",nameCmd),
            ("/users",usersCmd)]

    helpCmd user args = userWrite user (map fst cmds ++ appHelp (userApp user))

    quitCmd user args = userClose user

    appsCmd user args = userWrite user [unwords (map appName apps)]

    appCmd user [app] =
        case lookup app (zip (map appName apps) apps) of
            Just newApp -> do
                appRemoveUser (userApp user) (userId user)
                updateUser user { userApp = newApp }
                appAddUser newApp user { userApp = newApp }
            _ -> userWrite user [appName (userApp user)]
    appCmd user _ = userWrite user [appName (userApp user)]

    nameCmd user [] = userWrite user [userName user]
    nameCmd user args = do
        updateUser user { userName = unwords args }
        appUserRenamed (userApp user)
                       user { userName = unwords args }
                       (userName user)

    usersCmd user args = do
        userMap <- readMVar users
        userWrite user (map displayUser (Data.Map.toList userMap))
      where
        displayUser (UserId addr,user) =
            addr ++ ":" ++ appName (userApp user) ++ ":" ++ userName user

    updateUser user = modifyMVar_ users (return . insert (userId user) user)

nullApp :: App
nullApp = App {
    appName = "(none)",
    appHelp = [],
    appAddUser = \ _ -> return (),
    appRemoveUser = \ _ -> return (),
    appProcessLine = \ _ _ -> return (),
    appUserRenamed = \ _ _ -> return ()
    }
