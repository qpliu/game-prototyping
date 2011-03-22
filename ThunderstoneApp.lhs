> module ThunderstoneApp(thunderstoneApp) where

> import Control.Monad(when,unless)
> import Data.List(find)
> import System.Random(getStdGen)

> import GameApp
>     (GameApp(..),gameApp,
>      Game,gameGetName,gameGetState,gameSetState,
>      gameMessageToAll,gameMessageToAllExcept,gameMessageToUser,
>      App,UserId)

> import ThunderstoneBase
>     (ThunderstoneState,
>      thunderstoneInit,thunderstoneStartGame,randomizedGame,
>      thunderstonePublicState,thunderstonePlayerState,
>      thunderstoneTakeAction,
>      PlayerId)

> thunderstoneApp :: IO App
> thunderstoneApp =
>     getStdGen >>= gameApp . thunderstoneGame . thunderstoneInit

> thunderstoneGame :: ThunderstoneState -> ThunderstoneGame
> thunderstoneGame state =
>     ThunderstoneGame {
>         tsPlayers = [],
>         tsBots = [],
>         tsState = state,
>         tsGameStarted = False
>         }

> data ThunderstoneGame = ThunderstoneGame {
>     tsPlayers :: [Player],
>     tsBots :: [Bot],
>     tsState :: ThunderstoneState,
>     tsGameStarted :: Bool
>     }

> data Player = Player {
>     userId :: UserId,
>     playerId :: PlayerId
>     }

> data Bot = Bot {
>     botName :: String,
>     botId :: PlayerId
>     }

> instance GameApp ThunderstoneGame where
>     gameAppName _ = "Thunderstone"
>     gameAppAddPlayer uid = do
>         name <- gameGetName uid
>         gameMessageToAllExcept [uid] [name ++ " arrives."]
>         gameMessageToUser uid
>             ["Welcome to Thunderstone",
>              "/play to join the game",
>              "/start to start the game",
>              "/game to see the game state",
>              "/do to list available actions or to take an action"]
>     gameAppRemovePlayer uid = do
>         name <- gameGetName uid
>         gameMessageToAll [name ++ " leaves."]
>         state <- gameGetState
>         when (userInGame state uid) (do
>             if tsGameStarted state
>               then do
>                 gameMessageToAll ["The game is canceled."]
>                 gameSetState state {
>                     tsPlayers = [],
>                     tsBots = [],
>                     tsGameStarted = False
>                     }
>               else
>                 gameSetState state {
>                     tsPlayers = filter ((/= uid) . userId) (tsPlayers state)
>                     })
>     gameAppCommands = [("/play",playCmd),
>                        ("/start",startCmd),
>                        ("/game",gameCmd),
>                        ("/g",gameCmd),
>                        ("/do",doCmd),
>                        ("/d",doCmd)]
>     gameAppPollTime _ _ = Nothing
>     gameAppPoll = return ()

> playCmd :: UserId -> [String] -> Game ThunderstoneGame ()
> playCmd uid _ = do
>     state <- gameGetState
>     doPlay state
>   where
>     doPlay state
>       | userInGame state uid =
>             gameMessageToUser uid ["You are already in the game."]
>       | tsGameStarted state =
>             gameMessageToUser uid
>                 ["You cannot join a game already in progress."]
>       | length (tsPlayers state) + length (tsBots state) >= 5 =
>             gameMessageToUser uid
>                 ["The game is full. (Maximum of 5 players and bots)."]
>       | otherwise = do
>             name <- gameGetName uid
>             gameMessageToAll [name ++ " is playing the game."]
>             state <- gameGetState
>             gameSetState state {
>                 tsPlayers = newPlayer : tsPlayers state
>                 }
>     newPlayer = Player {
>         userId = uid,
>         playerId = undefined
>         }

> startCmd :: UserId -> [String] -> Game ThunderstoneGame ()
> startCmd uid _ = do
>     state <- gameGetState
>     doStart state
>   where
>     doStart state
>       | not (userInGame state uid) =
>             gameMessageToUser uid
>                 ["You are not in the game.","/play to join the game."]
>       | tsGameStarted state =
>             gameMessageToUser uid ["The game has already been started."]
>       | otherwise = do
>             name <- gameGetName uid
>             gameMessageToAll [name ++ " starts the game."]
>             state <- gameGetState
>             gameSetState state {
>                 tsPlayers = setPlayerIds,
>                 tsBots = setBotIds,
>                 tsState = thunderstoneState,
>                 tsGameStarted = True
>                 }
>           where
>             (thunderstoneState,playerIds) =
>                 thunderstoneStartGame (tsState state)
>                                       numberOfPlayers randomizedGame
>             numberOfPlayers =
>                 length (tsPlayers state) + length (tsBots state)
>             setPlayerIds = zipWith setPlayerId playerIds (tsPlayers state)
>             setPlayerId playerId player = player { playerId = playerId }
>             setBotIds = zipWith setBotId
>                                 (drop (length (tsPlayers state)) playerIds)
>                                 (tsBots state)
>             setBotId botId bot = bot { botId = botId }

> gameCmd :: UserId -> [String] -> Game ThunderstoneGame ()
> gameCmd uid _ = do
>     state <- gameGetState
>     doGame state
>   where
>     doGame state
>       | not (tsGameStarted state) = do
>             names <- mapM (gameGetName . userId) (tsPlayers state)
>             if null names
>               then
>                 gameMessageToUser uid
>                     ["The game has not started and there are no players.",
>                      "/play to join the game.",
>                      "/start to start the game after all players join."]
>               else do
>                 gameMessageToUser uid
>                     (("The game has not started.  Players:") :
>                      [' ' : name | name <- names])
>                 let botNames = map botName (tsBots state)
>                 unless (null botNames)
>                     (gameMessageToUser uid
>                          ("Bots:" : [' ' : name | name <- botNames]))
>       | otherwise = do
>             playerNames <- getPlayerNames
>             gameMessageToUser uid
>                (showPublicState (thunderstonePublicState (tsState state))
>                                  playerNames)
>             maybe (return ())
>                   (gameMessageToUser uid
>                        . showPlayerState
>                        . thunderstonePlayerState (tsState state))
>                   (getPlayerId state uid)
>     showPublicState _ = undefined
>     showPlayerState _ = undefined

> doCmd :: UserId -> [String] -> Game ThunderstoneGame ()
> doCmd uid args = undefined


> userInGame :: ThunderstoneGame -> UserId -> Bool
> userInGame state uid = uid `elem` [userId player | player <- tsPlayers state]

> getPlayerId :: ThunderstoneGame -> UserId -> Maybe PlayerId
> getPlayerId state uid =
>     fmap playerId (find ((uid ==) . userId) (tsPlayers state))

> getPlayerNames :: Game ThunderstoneGame (PlayerId -> String)
> getPlayerNames = do
>     state <- gameGetState
>     playerNames <- mapM getPlayerName (tsPlayers state)
>     let botNames = map getBotName (tsBots state)
>     return (maybe "(unknown)" id . flip lookup (playerNames ++ botNames))
>   where
>     getPlayerName player = do
>         name <- gameGetName (userId player)
>         return (playerId player,name)
>     getBotName bot = (botId bot,botName bot)
