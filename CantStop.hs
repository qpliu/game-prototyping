module CantStop(cantStopApp) where

import Data.List(find)
import qualified Data.Map
import Data.Maybe(catMaybes)
import Data.Time(UTCTime,addUTCTime,diffUTCTime)
import System.Random(StdGen,getStdGen,randomR)

import Apps(GameApp(..),gameApp,App,UserId)

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

getPlayer :: CantStop -> UserId -> Maybe Player
getPlayer game uid = find ((== uid) . playerId) (cantStopPlayers game)

inGame :: CantStop -> UserId -> Bool
inGame game uid = maybe False (const True) (getPlayer game uid)

playerLockedout :: CantStop -> UserId -> Bool
playerLockedout game uid =
    maybe False (not . null . playerLockoutCountdown) (getPlayer game uid)

updatePlayers :: CantStop -> ([Player] -> [Player]) -> CantStop
updatePlayers game update =
    game { cantStopPlayers = update (cantStopPlayers game) }

initialTracks :: [Track]
initialTracks =
    [Track { trackNumber = n, trackWinner = Nothing, trackPlayers = [] }
     | n <- [2..12]]

trackLength :: Track -> Int
trackLength (Track { trackNumber = n }) = min (n + 1) (15 - n)

trackWon :: UserId -> Track -> Bool
trackWon uid track = maybe False (uid ==) (trackWinner track)

trackOpen :: UserId -> Track -> Bool
trackOpen uid track
  | trackWinner track /= Nothing = False
  | otherwise = maybe True ((< trackLength track) . snd)
                      (lookup uid (trackPlayers track))

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
      | uid2 == uid && taken < trackLength track = (uid2,(stopAt,taken + 1))
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
    gameName _ = "CantStop"
    gameAddPlayer game uid getName time =
        (game,
         [getName uid ++ " has arrived."],
         [(uid,["Welcome to Can't Stop",
                "/play to start playing",
                "/start to start the game",
                "When playing: /roll, /take number, /stop"])])
    gameRemovePlayer game uid getName time =
        (removePlayer game uid,[getName uid ++ " has left."],[])
    gameCommands =
        [("/play",cantStopPlay),
         ("/game",cantStopGameState),
         ("/start",cantStopStart),
         ("/roll",cantStopRoll),
         ("/take",cantStopTake),
         ("/stop",cantStopStop)]
    gamePollTime game time =
        let times = filter (> time) (concatMap playerLockoutCountdown
                                               (cantStopPlayers game))
        in  if null times then Nothing else Just (minimum times)
    gamePoll game getName time = doCountdown getName time (game,[],[])

removePlayer :: CantStop -> UserId -> CantStop
removePlayer game uid
  | null (cantStopPlayers newGame) = newGame { cantStopTracks = [] }
  | otherwise = newGame
  where
    newGame = updatePlayers game {
                        cantStopTracks =
                            map (trackRemovePlayer uid) (cantStopTracks game)
                        }
                    (filter ((/= uid) . playerId))

doCountdown :: (UserId -> String) -> UTCTime
                       -> (CantStop,[String],[(UserId,[String])])
                       -> (CantStop,[String],[(UserId,[String])])
doCountdown getName time (game,defaultMessage,specificMessages) =
    (game { cantStopPlayers = updatedPlayers },
     defaultMessage,catMaybes updatedSpecificMessages)
  where
    (updatedPlayers,updatedSpecificMessages) =
        unzip (map updatePlayer (cantStopPlayers game))
    updatePlayer player
      | any (<= time) (playerLockoutCountdown player) =
            (player {
                playerLockoutCountdown =
                    filter (> time) (playerLockoutCountdown player)
                },
             addCountdownToMessage
                 (playerId player)
                 (length (filter (> time) (playerLockoutCountdown player))))
      | otherwise = (player,fmap ((,) (playerId player))
                            (lookup (playerId player) specificMessages))
    addCountdownToMessage uid 0 =
        Just (uid,getUserMessage uid ++ showState game getName ++ ["Go!"])
    addCountdownToMessage uid n =
        Just (uid,getUserMessage uid ++ [show n ++ "..."])
    getUserMessage uid = maybe defaultMessage id (lookup uid specificMessages)

cantStopPlay :: CantStop -> UserId -> (UserId -> String) -> UTCTime -> [String]
                         -> (CantStop,[String],[(UserId,[String])])
cantStopPlay game uid getName time args
  | inGame game uid = (game,[],[])
  | length (cantStopPlayers game) >= 4 =
        (game,[],[(uid,["The game is full."])])
  | null (cantStopTracks game) =
        (updatePlayers game (Player {
                                playerId = uid,
                                playerLockoutCountdown = [],
                                playerRoll = []
                                }
                              :),
         [getName uid ++ " is playing."],[(uid,["You are in the game."])])
  | otherwise =
        (updatePlayers game (Player {
                                playerId = uid,
                                playerLockoutCountdown =
                                    map (flip addUTCTime time) [1,2,3,4,5],
                                playerRoll = []
                                }
                             :),
         [getName uid ++ " joins the game."],
         [(uid,["You are in the game.  /roll after the countdown."])])

cantStopGameState :: CantStop -> UserId -> (UserId -> String)
                              -> UTCTime -> [String]
                              -> (CantStop,[String],[(UserId,[String])])
cantStopGameState game uid getName time args =
    (game,[],[(uid,showState game getName)])

cantStopStart :: CantStop -> UserId -> (UserId -> String)
                          -> UTCTime -> [String]
                          -> (CantStop,[String],[(UserId,[String])])
cantStopStart game uid getName time args
  | not (inGame game uid) = (game,[],[(uid,["You are not playing."])])
  | not (null (cantStopTracks game)) =
        (game,[],[(uid,["The game has already started."])])
  | otherwise =
        (updatePlayers game { cantStopTracks = initialTracks }
                       (map setCountdown),
         [getName uid
          ++ " starts the game.  Play starts after the countdown."],
         [])
  where
    setCountdown player =
        player {
            playerLockoutCountdown = map (flip addUTCTime time) [1,2,3,4,5]
        }

cantStopRoll :: CantStop -> UserId -> (UserId -> String) -> UTCTime -> [String]
                         -> (CantStop,[String],[(UserId,[String])])
cantStopRoll game uid getName time args
  | not (inGame game uid) = (game,[],[(uid,["You are not playing."])])
  | null (cantStopTracks game) =
        (game,[],[(uid,["The game has not started.  /start to start it."])])
  | playerLockedout game uid =
        (game,[],[(uid,["Wait for the countdown to finish."])])
  | not (null oldRoll) =
        (game,[],
         [(uid,["You already rolled " ++ showNumbers oldRoll,
                "You can take "
                    ++ showNumbers (takeOptions game uid oldRoll)])])
  | null newOptions =
        (game {
            cantStopStdGen = newStdGen,
            cantStopTracks = map (trackFail uid) (cantStopTracks game)
         },
         [getName uid ++ " rolls " ++ showNumbers newRoll ++ ", which fails"],
         [(uid,["You roll " ++ showNumbers newRoll ++ ", which fails"])])
  | otherwise =
        (updatePlayers game { cantStopStdGen = newStdGen } (map addNewRoll),
         [getName uid ++ " rolls " ++ showNumbers newRoll],
         [(uid,["You roll " ++ showNumbers newRoll,
                "You can take " ++ showNumbers newOptions])])
  where
    Just player = getPlayer game uid
    oldRoll = playerRoll player
    (newStdGen,newRoll) = doRoll (cantStopStdGen game)
    newOptions = takeOptions game uid newRoll
    addNewRoll player2
      | uid /= playerId player2 = player2
      | otherwise = player2 { playerRoll = newRoll }

cantStopTake :: CantStop -> UserId -> (UserId -> String) -> UTCTime -> [String]
                         -> (CantStop,[String],[(UserId,[String])])
cantStopTake game uid getName time args
  | not (inGame game uid) = (game,[],[(uid,["You are not playing."])])
  | null (cantStopTracks game) =
        (game,[],[(uid,["The game has not started.  /start to start it."])])
  | playerLockedout game uid =
        (game,[],[(uid,["Wait for the countdown to finish."])])
  | null roll =
        (game,[],[(uid,["You have not rolled.  /roll to roll."])])
  | null args && (length options == 1 || sum options == sum roll) =
        cantStopTake game uid getName time [show (head options)]
  | length args /= 1 || not (concat args `elem` map show options) =
        (game,[],[(uid,["You can take " ++ showNumbers options])])
  | not (otherTake `elem` options)
    || (length taken == 3 && not (otherTake `elem` taken))
    || (length taken == 2 && not (take `elem` taken)
                          && not (otherTake `elem` taken)) =
        (doTake take $ updatePlayers game (map clearRoll),
         [getName uid ++ " takes " ++ show take],
         [(uid,["You take " ++ show take])])
  | otherwise =
        (doTake otherTake $ doTake take $ updatePlayers game (map clearRoll),
         [getName uid ++ " takes " ++ show take ++ " and " ++ show otherTake],
         [(uid,["You take " ++ show take ++ " and " ++ show otherTake])])
  where
    Just player = getPlayer game uid
    roll = playerRoll player
    options = takeOptions game uid roll
    take = read (head args)
    otherTake = sum roll - take
    taken = map trackNumber $ filter (trackTaken uid) (cantStopTracks game)
    doTake track game =
        game {
            cantStopTracks = map (trackTake uid track) (cantStopTracks game)
        }
    clearRoll p@(Player { playerId = uid2 })
      | uid2 == uid = p { playerRoll = [] }
      | otherwise = p

cantStopStop :: CantStop -> UserId -> (UserId -> String) -> UTCTime -> [String]
                         -> (CantStop,[String],[(UserId,[String])])
cantStopStop game uid getName time args
  | not (inGame game uid) = (game,[],[(uid,["You are not playing."])])
  | null (cantStopTracks game) =
        (game,[],[(uid,["The game has not started.  /start to start it."])])
  | playerLockedout game uid =
        (game,[],[(uid,["Wait for the countdown to finish."])])
  | not $ null (playerRoll player) =
        (game,[],[(uid,["You have to take your roll.  /take to take."])])
  | null $ filter (trackTaken uid) $ cantStopTracks game =
        (game,[],[(uid,["You have no progress to stop at.  /roll to roll."])])
  | length wonTracks >= 3 =
        (game { cantStopPlayers = [], cantStopTracks = [] },
         [getName uid ++ " wins with " ++ showNumbers wonTracks],[])
  | otherwise =
        (updatePlayers newGame (map setCountdown),
         [getName uid ++ " stops."],[])
  where
    Just player = getPlayer game uid
    newGame =
        game { cantStopTracks = map (trackStop uid) (cantStopTracks game) }
    wonTracks =
        map trackNumber (filter (trackWon uid) (cantStopTracks newGame))
    setCountdown p@Player { playerId = uid2 }
      | uid == uid2 = p {
            playerLockoutCountdown = map (flip addUTCTime time) [1,2,3,4,5]
            }
      | otherwise = p

showNumbers :: [Int] -> String
showNumbers numbers = unwords (map show numbers)

doRoll :: StdGen -> (StdGen,[Int])
doRoll stdGen = head $ drop 4 $ iterate roll1 (stdGen,[])
  where
    roll1 (stdGen,rolls) = let (nextRoll,nextGen) = randomR (1,6) stdGen
                           in  (nextGen,nextRoll:rolls)

takeOptions :: CantStop -> UserId -> [Int] -> [Int]
takeOptions game uid dice
  | length takenTracks < 3 = options openTracks
  | otherwise = options openTakenTracks
  where
    takenTracks = filter (trackTaken uid) (cantStopTracks game)
    openTracks = filter (trackOpen uid) (cantStopTracks game)
    openTakenTracks = filter (trackOpen uid) takenTracks
    rolledOptions =
        [fst a + fst b | a <- zip dice [1..], b <- zip dice [1..], a /= b ]
    options availableTracks =
        filter (`elem` rolledOptions) (map trackNumber availableTracks)

showState :: CantStop -> (UserId -> String) -> [String]
showState game getName = concatMap showTrack (cantStopTracks game)
  where
    showTrack track = ["Track " ++ show (trackNumber track) ++ ": length: "
                                ++ show (trackLength track)]
                      ++ maybe (map showTrackPlayer (trackPlayers track))
                               ((:[]) . ("won by "++) . getName)
                               (trackWinner track)
    showTrackPlayer (uid,(stopAt,taken)) =
        " " ++ getName uid
            ++ " stopped at: " ++ show stopAt
            ++ if taken > stopAt then " progress to: " ++ show taken else ""
