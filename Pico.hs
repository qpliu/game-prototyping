module Pico(picoApp) where

import Data.List(find,partition)
import Data.Time(UTCTime)
import System.Random(StdGen,getStdGen)

import GameApp
    (GameApp(..),gameApp,
     Game,gameGetName,gameGetTime,gameGetState,gameSetState,gameUpdateState,
     gameMessageToAll,gameMessageToAllExcept,gameMessageToUser,
     App,UserId)
import Shuffle(shuffle)

picoApp :: IO App
picoApp = getStdGen >>= gameApp . picoGame

picoGame :: StdGen -> PicoGame
picoGame stdGen = PicoGame {
    picoPlayers = [],
    picoStdGen = stdGen
    }

data PicoGame = PicoGame {
    picoPlayers :: [Player],
    picoStdGen :: StdGen
    }

data Player = Player {
    playerId :: UserId,
    playerHand :: [Card],
    playerWins :: [Card],
    playerPlayed :: Maybe Card,
    playerScore :: Int
    }

newtype Card = Card Int
    deriving Eq

instance Show Card where
    show (Card n) = show n

cards :: [Card]
cards = map Card (16:[4..13])

score :: [Card] -> Int
score cards = sum (map scoreCard cards)
  where
    scoreCard (Card n)
      | n < 5 = 1
      | n < 8 = 2
      | n < 14 = 3
      | otherwise = 4

defeats :: Card -> Card -> Bool
defeats (Card i) (Card j) = (i > j && i <= 2*j) || 2*i < j

instance GameApp PicoGame where
    gameAppName _ = "Pico"
    gameAppAddPlayer uid = do
        name <- gameGetName uid
        gameMessageToAllExcept [uid] [name ++ " arrives."]
        gameMessageToUser uid
            ["Welcome to Pico",
             "/play to join the game",
             "/play CARD when playing to play a card",
             "/game to see the current state of the game"]
        gameCmd uid []
    gameAppRemovePlayer uid = do
        name <- gameGetName uid
        cond [(inGame uid,
               do gameUpdateState resetGame
                  cond [(gameStarted,
                         gameMessageToAll
                             [name ++ " leaves, ending the current game."]),
                        (return True, gameMessageToAll [name ++ " leaves."])]),
              (return True, gameMessageToAll [name ++ " leaves."])]
    gameAppCommands = [("/play",playCmd),("/game",gameCmd)]
    gameAppPollTime _ = Nothing
    gameAppPoll = return ()

cond :: Monad m => [(m Bool,m ())] -> m ()
cond [] = return ()
cond ((test,body):rest) = do
    result <- test
    if result
        then body
        else cond rest

isNot :: Monad m => m Bool -> m Bool
isNot test = do
    result <- test
    return (not result)

inGame :: UserId -> Game PicoGame Bool
inGame uid = do
    game <- gameGetState
    return (uid `elem` map playerId (picoPlayers game))

gameStarted :: Game PicoGame Bool
gameStarted = do
    game <- gameGetState
    return (length (picoPlayers game) >= 2)

afterSwap :: Game PicoGame Bool
afterSwap = do
    game <- gameGetState
    return (sum (map playerScore (picoPlayers game)) > 0)

handFinished :: Game PicoGame Bool
handFinished = do
    game <- gameGetState
    return (any (< 2) (map (length . playerHand) (picoPlayers game)))

hasPlayed :: UserId -> Game PicoGame Bool
hasPlayed uid = do
    game <- gameGetState
    maybe (return False)
          (return . (/= Nothing) . playerPlayed)
          (find ((uid ==) . playerId) (picoPlayers game))

doPlay :: UserId -> String -> Game PicoGame Bool
doPlay uid card = do
    game <- gameGetState
    maybe (return False)
          (update game)
          (find ((uid ==) . playerId) (picoPlayers game))
  where
    update game player = do
        let played = find ((card ==) . show) (playerHand player)
        let hand = filter ((card /=) . show) (playerHand player)
        if card `elem` (map show (playerHand player))
            then do
                updateGame game player {
                                    playerHand = hand,
                                    playerPlayed = played
                                    }
                return True
            else return False
    updateGame game@PicoGame { picoPlayers = players } player =
        gameSetState game { picoPlayers = map (updatePlayer player) players }
    updatePlayer player oldPlayer =
        if uid == playerId oldPlayer then player else oldPlayer

allPlayersPlayed :: Game PicoGame Bool
allPlayersPlayed = do
    game <- gameGetState
    return (not (Nothing `elem` (map playerPlayed (picoPlayers game))))

resetGame :: PicoGame -> PicoGame
resetGame game = game { picoPlayers = [] }

dealHand :: PicoGame -> PicoGame
dealHand game =
    game {
        picoPlayers = zipWith initPlayer (picoPlayers game) [deck1,deck2],
        picoStdGen = stdGen
        }
 where
    (stdGen,_:deck) = shuffle (picoStdGen game) cards
    (deck1,deck2) = splitAt (length deck `div` 2) deck
    initPlayer player hand =
        player { playerHand = hand, playerWins = [], playerPlayed = Nothing }

swapHands :: PicoGame -> PicoGame
swapHands game@PicoGame { picoPlayers = (player1:player2:_) } =
    game {
        picoPlayers =
            [player1 {
                 playerHand = playerHand player2 ++ playerWins player2,
                 playerWins = [],
                 playerPlayed = Nothing,
                 playerScore = score (playerWins player1)
             }, player2 {
                 playerHand = playerHand player1 ++ playerWins player1,
                 playerWins = [],
                 playerPlayed = Nothing,
                 playerScore = score (playerWins player2)
             }]
        }

addPlayer :: UserId -> PicoGame -> PicoGame
addPlayer uid game =
    game { picoPlayers = newPlayer : picoPlayers game }
  where
    newPlayer = Player {
                    playerId = uid,
                    playerHand = [],
                    playerWins = [],
                    playerPlayed = Nothing,
                    playerScore = 0
                    }

playCmd :: UserId -> [String] -> Game PicoGame ()
playCmd uid [] = do
    name <- gameGetName uid
    cond [(inGame uid,
           gameMessageToUser uid ["You are already playing."]),
          (gameStarted,
           gameMessageToUser uid ["The game is full."]),
          (return True,
           do gameUpdateState (addPlayer uid)
              cond [(gameStarted,
                     do gameUpdateState dealHand
                        game <- gameGetState
                        let playerIds = map playerId (picoPlayers game)
                        gameMessageToAll
                            [name ++ " starts playing.  The game starts."]
                        sequence_ (map (flip gameCmd []) playerIds)),
                    (return True,
                     gameMessageToAll
                            [name ++ " starts playing, and is waiting for "
                                  ++ "an opponent to start the game."])])]
playCmd uid [card] = do
    cond [(isNot (inGame uid),
           gameMessageToUser uid ["You are not in the game."]),
          (isNot gameStarted,
           gameMessageToUser uid ["The game has not started."]),
          (hasPlayed uid,
           gameMessageToUser uid ["You have already played a card."]),
          (isNot (doPlay uid card),
           gameMessageToUser uid ["That is not a valid play."]),
          (return True,
           do name <- gameGetName uid
              gameMessageToAll [name ++ " plays a card."]
              cond [(allPlayersPlayed,
                     do game <- gameGetState
                        resolvePlayedCards game
                        cond [(handFinished,
                               do game <- gameGetState
                                  let players = picoPlayers game
                                  lines <- sequence (map showPublic players)
                                  gameMessageToAll (concat lines)
                                  cond [(afterSwap,
                                         do gameMessageToAll ["Game over."]
                                            gameUpdateState resetGame),
                                        (return True,
                                         do gameMessageToAll
                                                ["Swapping hands."]
                                            gameUpdateState swapHands
                                            let playerIds =
                                                    map playerId players
                                            sequence_ (map (flip gameCmd [])
                                                           playerIds))])])])]
  where
    resolvePlayedCards
        game@PicoGame {
            picoPlayers = (player1@Player { playerPlayed = Just card1 }
                           :player2@Player { playerPlayed = Just card2 }:_)
            }
      | card1 `defeats` card2 = do
            gameSetState game {
                picoPlayers =
                    [player1 {
                         playerWins = card1 : playerWins player1,
                         playerPlayed = Nothing
                         },
                     player2 {
                         playerHand = card2 : playerHand player2,
                         playerPlayed = Nothing
                         }]
                }
            name1 <- gameGetName (playerId player1)
            name2 <- gameGetName (playerId player2)
            gameMessageToAll [name1 ++ " wins with " ++ show card1 ++ ".",
                              name2 ++ " loses with " ++ show card2 ++ "."]
      | otherwise = do
            gameSetState game {
                picoPlayers =
                    [player1 {
                         playerHand = card1 : playerHand player1,
                         playerPlayed = Nothing
                         },
                     player2 {
                         playerWins = card2 : playerWins player2,
                         playerPlayed = Nothing
                         }]
                }
            name1 <- gameGetName (playerId player1)
            name2 <- gameGetName (playerId player2)
            gameMessageToAll [name2 ++ " wins with " ++ show card2 ++ ".",
                              name1 ++ " loses with " ++ show card1 ++ "."]

playCmd uid _ = gameMessageToUser uid ["That is not a valid play."]

gameCmd :: UserId -> [String] -> Game PicoGame ()
gameCmd uid _ = do
    game <- gameGetState
    let players = picoPlayers game
    cond [(inGame uid,
           cond [(gameStarted,
                  sequence_ (map ((>>= gameMessageToUser uid) . showPublic)
                                 (filter ((/= uid) . playerId) players)
                             ++ map (gameMessageToUser uid . showPrivate)
                                    (filter ((== uid) . playerId) players))),
                 (return True,
                  gameMessageToUser uid
                                    ["You are waiting for another player."])]),
          (return True,
           cond [(gameStarted,
                  sequence_ (map ((>>= gameMessageToUser uid) . showPublic)
                                 players)),
                 (return (null players),
                  gameMessageToUser uid
                      ["Nobody is playing.", "/play to start a game."]),
                 (return True,
                  do (name:_) <- sequence
                                     (map (gameGetName . playerId) players)
                     gameMessageToUser uid
                         [name ++ " is waiting for an opponent.",
                          "/play to play with " ++ name ++ "."])])]

showPublic :: Player -> Game PicoGame [String]
showPublic player = do
    name <- gameGetName (playerId player)
    return
        [name ++ ": hand size: " ++ show (length (playerHand player))
         ++ " wins: " ++ unwords (map show (playerWins player))
         ++ " score: "
         ++ show (score (playerWins player) + playerScore player)
         ++ maybe " choosing a card to play"
                  (const " has played a card") (playerPlayed player)]

showPrivate :: Player -> [String]
showPrivate player =
    ["Your hand: " ++ unwords (map show (playerHand player))
     ++ " wins: " ++ unwords (map show (playerWins player))
     ++ " score: " ++ show (score (playerWins player) + playerScore player),
     maybe "/play to play a card"
           (("You are playing " ++) . (++ ".") . show) (playerPlayed player)]
