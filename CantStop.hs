module CantStop(cantStopApp) where

import Data.List(find,nub)
import Data.Maybe(catMaybes)
import Data.Time(UTCTime,addUTCTime,diffUTCTime)
import System.Random(StdGen,getStdGen,randomR)

import GameApp
    (GameApp(..),gameApp,
     Game,gameGetName,gameGetTime,
     gameGetState,gameGetStateAttr,gameSetState,gameUpdateState,
     gameMessageToAll,gameMessageToAllExcept,gameMessageToUser,
     App,UserId)

cantStopApp :: IO App
cantStopApp = getStdGen >>= gameApp . cantStop

cantStop :: StdGen -> CantStop
cantStop stdGen = CantStop {
    cantStopPlayers = [],
    cantStopStdGen = stdGen,
    cantStopTracks = []
    }

data CantStop = CantStop {
    cantStopPlayers :: [Player],
    cantStopStdGen :: StdGen,
    cantStopTracks :: [Track]
    }

data Player = Player {
    playerId :: UserId,
    playerLockoutCountdown :: [UTCTime],
    playerRoll :: [Int]
    }

data Track = Track {
    trackNumber :: Int,
    trackWinner :: Maybe UserId,
    trackPlayers :: [(UserId,(Int,Int))]
    }

getPlayer :: UserId -> CantStop -> Maybe Player
getPlayer uid game = find ((== uid) . playerId) (cantStopPlayers game)

inGame :: UserId -> CantStop -> Bool
inGame uid game = maybe False (const True) (getPlayer uid game)

gameStarted :: CantStop -> Bool
gameStarted game = not (null (cantStopTracks game))

gameReset :: CantStop -> CantStop
gameReset game = game { cantStopPlayers = [], cantStopTracks = [] }

gameStart :: UTCTime -> CantStop -> CantStop
gameStart utcTime game =
    foldl setCountdown game { cantStopTracks = initialTracks }
          (cantStopPlayers game)
  where
    setCountdown game player =
        setPlayerCountdown (playerId player) utcTime game

newPlayer :: UserId -> Player
newPlayer uid =
    Player {
        playerId = uid,
        playerLockoutCountdown = [],
        playerRoll = []
        }

playerLockedout :: UserId -> CantStop -> Bool
playerLockedout uid game =
    maybe False (not . null . playerLockoutCountdown) (getPlayer uid game)

addPlayer :: Player -> CantStop -> CantStop
addPlayer player game =
    game { cantStopPlayers = player : cantStopPlayers game }

updatePlayerCountdown :: UserId -> [UTCTime] -> CantStop -> CantStop
updatePlayerCountdown uid countdown game =
    game { cantStopPlayers = map updatePlayer (cantStopPlayers game) }
  where
    updatePlayer player
      | playerId player == uid = player { playerLockoutCountdown = countdown }
      | otherwise = player

setPlayerCountdown :: UserId -> UTCTime -> CantStop -> CantStop
setPlayerCountdown uid utcTime game =
    updatePlayerCountdown uid (map (flip addUTCTime utcTime) [1,2,3,4,5]) game

playerRolls :: UserId -> CantStop -> CantStop
playerRolls uid game =
    game {
        cantStopPlayers = map updatePlayer (cantStopPlayers game),
        cantStopStdGen = newStdGen
        }
  where
    (newStdGen,roll) = head $ drop 4 $ iterate roll1 (cantStopStdGen game,[])
    roll1 (stdGen,rolls) = let (nextRoll,nextGen) = randomR (1,6) stdGen
                           in  (nextGen,nextRoll:rolls)
    updatePlayer player
      | playerId player == uid = player { playerRoll = roll }
      | otherwise = player

getPlayerRoll :: UserId -> CantStop -> [Int]
getPlayerRoll uid game = maybe [] playerRoll (getPlayer uid game)

takeOptions :: UserId -> CantStop -> [Int]
takeOptions uid game
  | length takenTracks < 3 = options openTracks
  | otherwise = options openTakenTracks
  where
    roll = getPlayerRoll uid game
    takenTracks = filter (trackTaken uid) (cantStopTracks game)
    openTracks = filter (trackOpen uid) (cantStopTracks game)
    openTakenTracks = filter (trackOpen uid) takenTracks
    rolledOptions =
        [fst a + fst b | a <- zip roll [1..], b <- zip roll [1..], a /= b ]
    options availableTracks =
        filter (`elem` rolledOptions) (map trackNumber availableTracks)

clearRoll :: UserId -> CantStop -> CantStop
clearRoll uid game =
    game { cantStopPlayers = map updatePlayer (cantStopPlayers game) }
  where
    updatePlayer p | playerId p == uid = p { playerRoll = [] } | otherwise = p

initialTracks :: [Track]
initialTracks =
    [Track { trackNumber = n, trackWinner = Nothing, trackPlayers = [] }
     | n <- [2..12]]

trackLength :: Track -> Int
trackLength (Track { trackNumber = n }) = min (n + 1) (15 - n)

getTracks :: (Track -> Bool) -> CantStop -> [Track]
getTracks test game = filter test (cantStopTracks game)

updateTracks :: (Track -> Track) -> CantStop -> CantStop
updateTracks update game =
    game { cantStopTracks = map update (cantStopTracks game) }

trackNumbered :: [Int] -> Track -> Bool
trackNumbered numbers track = trackNumber track `elem` numbers

trackWon :: UserId -> Track -> Bool
trackWon uid track = maybe False (uid ==) (trackWinner track)

trackOpen :: UserId -> Track -> Bool
trackOpen uid track
  | trackWinner track /= Nothing = False
  | otherwise = maybe True ((< trackLength track) . snd)
                      (lookup uid (trackPlayers track))

trackProgress :: UserId -> Track -> Int
trackProgress uid track = maybe 0 snd (lookup uid (trackPlayers track))

trackStoppedAt :: UserId -> Track -> Int
trackStoppedAt uid track = maybe 0 fst (lookup uid (trackPlayers track))

trackTaken :: UserId -> Track -> Bool
trackTaken uid track
  | trackWinner track /= Nothing = False
  | otherwise = maybe False (uncurry (<)) (lookup uid (trackPlayers track))

trackTake :: UserId -> Int -> Track -> Track
trackTake uid number track
  | trackWinner track /= Nothing || number /= trackNumber track = track
  | not (uid `elem` map fst (trackPlayers track)) =
      track { trackPlayers = (uid,(0,1)) : (trackPlayers track) }
  | otherwise = track { trackPlayers = map takeT (trackPlayers track) }
  where
    takeT p@(uid2,(stopAt,taken))
      | uid2 == uid = (uid2,(stopAt,min (taken + 1) (trackLength track)))
      | otherwise = p

trackFail :: UserId -> Track -> Track
trackFail uid track
  | trackWinner track /= Nothing = track
  | otherwise = track { trackPlayers = map fail (trackPlayers track) }
  where
    fail p@(uid2,(stopAt,taken))
      | uid2 == uid = (uid2,(stopAt,stopAt))
      | otherwise = p

trackStop :: UserId -> Track -> Track
trackStop uid track
  | trackWinner track /= Nothing = track
  | maybe False ((>= trackLength track) . snd)
                (lookup uid (trackPlayers track)) =
        track { trackWinner = Just uid, trackPlayers = [] }
  | otherwise = track { trackPlayers = map stop (trackPlayers track) }
  where
    stop p@(uid2,(stopAt,taken))
      | uid2 == uid = (uid2,(taken,taken))
      | otherwise = p

trackRemovePlayer :: UserId -> Track -> Track
trackRemovePlayer uid track
  | trackWinner track == Just uid =
        track { trackWinner = Nothing, trackPlayers = [] }
  | otherwise =
        track { trackPlayers = filter ((uid /=) . fst) (trackPlayers track) }

instance GameApp CantStop where
    gameAppName _ = "CantStop"
    gameAppAddPlayer uid = do
        name <- gameGetName uid
        gameMessageToAllExcept [uid] [name ++ " arrives."]
        gameMessageToUser uid
            ["Welcome to Can't Stop.",
             "/play to join the game",
             "/start to start the game",
             "When playing /roll, /take number, /stop"]
    gameAppRemovePlayer uid = do
        name <- gameGetName uid
        cond [(gameGetStateAttr (not . inGame uid),
               gameMessageToAll [name ++ " leaves."]),
              (gameGetStateAttr (not . gameStarted),
               do gameUpdateState (removePlayer uid)
                  gameMessageToAll [name ++ " leaves."]),
              (gameGetStateAttr
                   (null . filter (trackWon uid) . cantStopTracks),
               do gameUpdateState (removePlayer uid)
                  gameMessageToAll [name ++ " leaves."]),
              (return True,
               do openTracks <-
                      gameGetStateAttr (filter (trackWon uid) . cantStopTracks)
                  gameUpdateState (removePlayer uid)
                  gameMessageToAll
                      [name ++ " leaves.  Tracks now open: "
                            ++ unwords (map (show . trackNumber) openTracks)])]
    gameAppCommands = [("/play",cmdPlay),("/game",cmdGame),("/start",cmdStart),
                       ("/roll",cmdRoll),("/take",cmdTake),("/stop",cmdStop)]
    gameAppPollTime game time =
        if null times then Nothing else Just (minimum times)
      where times = filter (> time) (concatMap playerLockoutCountdown
                                               (cantStopPlayers game))
    gameAppPoll = doPoll

cond :: Monad m => [(m Bool,m ())] -> m ()
cond [] = return ()
cond ((test,body):rest) = do
    result <- test
    if result
        then body
        else cond rest

removePlayer :: UserId -> CantStop -> CantStop
removePlayer uid game
  | null (cantStopPlayers newGame) = newGame { cantStopTracks = [] }
  | otherwise = newGame
  where
    newGame = game {
        cantStopPlayers = filter ((/= uid) . playerId) (cantStopPlayers game),
        cantStopTracks = map (trackRemovePlayer uid) (cantStopTracks game)
        }

cmdPlay :: UserId -> [String] -> Game CantStop ()
cmdPlay uid _ =
    cond [(gameGetStateAttr (inGame uid),
           gameMessageToUser uid ["You are already playing."]),
          (gameGetStateAttr (not . gameStarted),
           do name <- gameGetName uid
              gameMessageToAll [name ++ " is playing."]
              gameUpdateState (addPlayer (newPlayer uid))),
         (return True,
           do name <- gameGetName uid
              utcTime <- gameGetTime
              gameMessageToAll [name ++ " enters the game."]
              gameUpdateState (addPlayer (newPlayer uid))
              gameUpdateState (setPlayerCountdown uid utcTime))]

cmdGame :: UserId -> [String] -> Game CantStop ()
cmdGame uid _ =
    cond [(gameGetStateAttr (not . gameStarted),
           do players <- gameGetStateAttr cantStopPlayers
              if null players
                  then gameMessageToUser uid ["Nobody is playing."]
                  else do
                    playerNames <-
                        sequence (map (gameGetName . playerId) players)
                    gameMessageToUser uid
                        ("The game is not started.  Players:"
                         : map ("  " ++) playerNames)),
          (return True,
           do tracks <- gameGetStateAttr cantStopTracks
              trackInfo <- sequence (map showTrack tracks)
              gameMessageToUser uid (concat trackInfo))]
  where
    showTrack track = do
        trackPlayerInfo <- maybe (showTrackPlayers track)
                                 showTrackWinner (trackWinner track)
        return (("Track " ++ show (trackNumber track) ++ " ("
                          ++ show (trackLength track) ++ ")")
                : trackPlayerInfo)
    showTrackWinner winnerId = do
        winner <- gameGetName winnerId
        return ["  won by: " ++ winner]
    showTrackPlayers track =
        sequence (map showTrackPlayer (trackPlayers track))
    showTrackPlayer (pid,(stoppedAt,progress)) = do
        name <- gameGetName pid
        return (name ++ ": " ++ show stoppedAt
                     ++ if progress > stoppedAt
                            then " -> " ++ show progress
                            else "")

cmdStart :: UserId -> [String] -> Game CantStop ()
cmdStart uid _ =
    cond [(gameGetStateAttr (not . inGame uid),
           gameMessageToUser uid
               ["You are not in the game.","/play to join the game."]),
          (gameGetStateAttr gameStarted,
           gameMessageToUser uid ["The game has already been started."]),
          (return True,
           do name <- gameGetName uid
              utcTime <- gameGetTime
              gameMessageToAll [name ++ " starts the game.",
                                "Start playing after the countdown."]
              gameUpdateState (gameStart utcTime))]

cmdRoll :: UserId -> [String] -> Game CantStop ()
cmdRoll uid _ =
    cond [(gameGetStateAttr (not . inGame uid),
           gameMessageToUser uid
               ["You are not in the game.","/play to join the game."]),
          (gameGetStateAttr (not . gameStarted),
           gameMessageToUser uid
               ["The game has not been started.","/start to start the game."]),
          (gameGetStateAttr (playerLockedout uid),
           gameMessageToUser uid ["Try again after the countdown finishes."]),
          (gameGetStateAttr (not . null . getPlayerRoll uid),
           do roll <- gameGetStateAttr (getPlayerRoll uid)
              canTake <- gameGetStateAttr (takeOptions uid)
              gameMessageToUser uid
                  ["You already rolled: " ++ showNumbers roll,
                   "You can take: " ++ showNumbers canTake]),
          (return True,
           do gameUpdateState (playerRolls uid)
              roll <- gameGetStateAttr (getPlayerRoll uid)
              canTake <- gameGetStateAttr (takeOptions uid)
              if null canTake
                  then do
                      gameUpdateState (updateTracks (trackFail uid))
                      gameUpdateState (clearRoll uid)
                      gameMessageToUser uid
                           ["You roll: " ++ showNumbers roll
                                         ++ ", which fails."]
                  else gameMessageToUser uid
                           ["You roll: " ++ showNumbers roll,
                            "You can take: " ++ showNumbers canTake])]

cmdTake :: UserId -> [String] -> Game CantStop ()
cmdTake uid [number] =
    cond [(gameGetStateAttr (not . inGame uid),
           gameMessageToUser uid
               ["You are not in the game.","/play to join the game."]),
          (gameGetStateAttr (not . gameStarted),
           gameMessageToUser uid
               ["The game has not been started.","/start to start the game."]),
          (gameGetStateAttr (playerLockedout uid),
           gameMessageToUser uid ["Try again after the countdown finishes."]),
          (gameGetStateAttr (null . getPlayerRoll uid),
           gameMessageToUser uid ["You have not rolled.","/roll to roll."]),
          (gameGetStateAttr (not . (number `elem`)
                                 . map show . takeOptions uid),
           do roll <- gameGetStateAttr (getPlayerRoll uid)
              canTake <- gameGetStateAttr (takeOptions uid)
              gameMessageToUser uid
                  ["Invalid option.",
                   "You roll: " ++ showNumbers roll,
                   "You can take: " ++ showNumbers canTake]),
          (return True,
           do roll <- gameGetStateAttr (getPlayerRoll uid)
              canTake <- gameGetStateAttr (takeOptions uid)
              let taken2 = read number : filter (== (sum roll - read number))
                                                canTake
              alreadyTaken <- gameGetStateAttr
                                (map trackNumber . getTracks (trackTaken uid))
              let taken = if length (nub (alreadyTaken ++ taken2)) > 3
                              then take 1 taken2
                              else taken2
              gameUpdateState (clearRoll uid)
              sequence_ (map takeTrack taken)
              name <- gameGetName uid
              takenTracks <- gameGetStateAttr (getTracks (trackNumbered taken))
              gameMessageToAll (map (showTakenTrack name) takenTracks))]
  where
    takeTrack trackNumber =
        gameUpdateState (updateTracks (trackTake uid trackNumber))
    showTakenTrack name track =
        name ++ " takes " ++ show (trackNumber track)
             ++ ": " ++ show (trackStoppedAt uid track)
             ++ " -> " ++ show (trackProgress uid track)
             ++ "/" ++ show (trackLength track) ++ "."

cmdTake uid _ = gameMessageToUser uid ["/take NUMBER"]

cmdStop :: UserId -> [String] -> Game CantStop ()
cmdStop uid _ =
    cond [(gameGetStateAttr (not . inGame uid),
           gameMessageToUser uid
               ["You are not in the game.","/play to join the game."]),
          (gameGetStateAttr (not . gameStarted),
           gameMessageToUser uid
               ["The game has not been started.","/start to start the game."]),
          (gameGetStateAttr (playerLockedout uid),
           gameMessageToUser uid ["Try again after the countdown finishes."]),
          (gameGetStateAttr (not . null . getPlayerRoll uid),
           do roll <- gameGetStateAttr (getPlayerRoll uid)
              canTake <- gameGetStateAttr (takeOptions uid)
              gameMessageToUser uid
                  ["You need to take your tracks.",
                   "You have rolled: " ++ showNumbers roll,
                   "You can take: " ++ showNumbers canTake]),
          (return True,
           do name <- gameGetName uid
              utcTime <- gameGetTime
              takenTracks <- gameGetStateAttr (getTracks (trackTaken uid))
              gameUpdateState (updateTracks (trackStop uid))
              wonTracks <- gameGetStateAttr (getTracks (trackWon uid))
              gameMessageToAll (map (showTakenTrack name) takenTracks)
              if length wonTracks >= 3
                  then do
                      gameMessageToAll
                          [name ++ " wins with "
                                ++ showNumbers (map trackNumber wonTracks)
                                ++ "."]
                      gameUpdateState gameReset
                  else
                      gameUpdateState (setPlayerCountdown uid utcTime))]
  where
    showTakenTrack name track
      | trackProgress uid track >= trackLength track =
        name ++ " stops and wins " ++ show (trackNumber track) ++ "."
      | otherwise =
        name ++ " stops on " ++ show (trackNumber track)
             ++ ": " ++ show (trackStoppedAt uid track)
             ++ " -> " ++ show (trackProgress uid track)
             ++ "/" ++ show (trackLength track) ++ "."

doPoll :: Game CantStop ()
doPoll = do
    utcTime <- gameGetTime
    players <- gameGetStateAttr cantStopPlayers
    sequence_ (map (pollPlayer utcTime) players)
  where
    pollPlayer utcTime player
      | any (<= utcTime) (playerLockoutCountdown player) = do
            let newLockout = filter (> utcTime) (playerLockoutCountdown player)
            gameUpdateState
                (updatePlayerCountdown (playerId player) newLockout)
            gameMessageToUser (playerId player)
                [if null newLockout
                     then "Go!"
                     else show (length newLockout) ++ "..."]
      | otherwise = return ()

showNumbers :: [Int] -> String
showNumbers numbers = unwords (map show numbers)
