module Zero(zeroApp) where

import Data.List(find,nub,sortBy)
import Data.Time(UTCTime)
import System.Random(StdGen,getStdGen)

import GameApp
    (GameApp(..),gameApp,
     Game,gameGetName,gameGetTime,
     gameGetState,gameGetStateAttr,gameSetState,gameUpdateState,
     gameMessageToAll,gameMessageToAllExcept,gameMessageToUser,
     App,UserId)
import Shuffle(shuffle)

zeroApp :: IO App
zeroApp = getStdGen >>= gameApp . zeroGame

zeroGame :: StdGen -> Zero
zeroGame stdGen = Zero {
    zeroPlayers = [],
    zeroCurrentPlayer = 0,
    zeroKnock = False,
    zeroCards = [],
    zeroRound = 0,
    zeroStdGen = stdGen
    }

data Zero = Zero {
    zeroPlayers :: [Player],
    zeroCurrentPlayer :: Int,
    zeroKnock :: Bool,
    zeroCards :: [Card],
    zeroRound :: Int,
    zeroStdGen :: StdGen
    }

data Player = Player {
    playerId :: UserId,
    playerCards :: [Card],
    playerKnock :: Bool,
    playerScore :: Int
    }

data Color = Red | Yellow | Green | Blue | Violet | Gray | Black
    deriving (Bounded,Enum,Eq,Ord,Show)

data Card = Card {
    cardColor :: Color,
    cardNumber :: Int
    }
    deriving Eq

instance Show Card where
    show Card { cardColor = color, cardNumber = number } =
        show color ++ show number

allCards :: [Card]
allCards = [Card { cardColor = color, cardNumber = number }
            | color <- [minBound..maxBound], number <- [1..8]]

scoreHand :: [Card] -> Int
scoreHand cards = sum $ nub $ map cardNumber $ filter notZero cards
  where
    notZero Card { cardColor = color, cardNumber = number } =
        length (filter ((== color) . cardColor) cards) < 5
        && length (filter ((== number) . cardNumber) cards) < 5

deal :: Zero -> Zero
deal game = game {
                zeroPlayers = reverse players,
                zeroKnock = False,
                zeroCards = take 5 shuffledCards,
                zeroStdGen = stdGen
                }
  where
    (stdGen,shuffledCards) = shuffle (zeroStdGen game) allCards
    (players,_) = foldl dealHand ([],drop 5 shuffledCards) (zeroPlayers game)
    dealHand (players,cards) player =
        (player { playerCards = take 9 cards, playerKnock = False } : players,
         drop 9 cards)

finishRound :: Zero -> Zero
finishRound game =
    game {
        zeroPlayers = map addScore (zeroPlayers game),
        zeroCurrentPlayer = zeroRound game,
        zeroKnock = False,
        zeroCards = [],
        zeroRound = zeroRound game + 1
        }
  where
    addScore player =
        player {
            playerCards = [],
            playerKnock = False,
            playerScore = playerScore player + scoreHand (playerCards player)
            }

isLastRound :: Zero -> Bool
isLastRound game = zeroRound game >= length (zeroPlayers game)

resetGame :: Zero -> Zero
resetGame game =
    game { zeroPlayers = [], zeroKnock = False, zeroCards = [], zeroRound = 0 }

removePlayer :: UserId -> Zero -> Zero
removePlayer uid game =
    game { zeroPlayers = filter ((/= uid) . playerId) (zeroPlayers game) }

addPlayer :: UserId -> Zero -> Zero
addPlayer uid game =
    game { zeroPlayers = newPlayer : zeroPlayers game }
  where
    newPlayer = Player {
                    playerId = uid,
                    playerCards = [],
                    playerKnock = False,
                    playerScore = 0
                    }

inGame :: UserId -> Zero -> Bool
inGame uid game = uid `elem` map playerId (zeroPlayers game)

isCurrentPlayer :: UserId -> Zero -> Bool
isCurrentPlayer uid game = uid == playerId (currentPlayer game)

currentPlayer :: Zero -> Player
currentPlayer game = zeroPlayers game !! zeroCurrentPlayer game

updatePlayer :: Player -> Zero -> Zero
updatePlayer player game =
    game { zeroPlayers = map update (zeroPlayers game) }
  where
    update p | playerId p == playerId player = player | otherwise = p

nextTurn :: Zero -> Zero
nextTurn game =
    game {
        zeroCurrentPlayer =
            (zeroCurrentPlayer game + 1) `mod` length (zeroPlayers game)
        }

gameStarted :: Zero -> Bool
gameStarted game = not (null (zeroCards game))

gameFull :: Zero -> Bool
gameFull game = length (zeroPlayers game) >= 5

gameCanStart :: Zero -> Bool
gameCanStart game = length (zeroPlayers game) >= 3

instance GameApp Zero where
    gameAppName game = "Zero"
    gameAppAddPlayer uid = do
        name <- gameGetName uid
        gameMessageToAllExcept [uid] [name ++ " arrives."]
        gameMessageToUser uid
            ["Welcome to Zero.",
             "/play to join the game",
             "/start to start the game",
             "/game to see the game state",
             "When playing, /knock or /swap, and /hand to see your hand."]
        cmdGame uid []
    gameAppRemovePlayer uid = do
        name <- gameGetName uid
        cond [(gameGetStateAttr (not . inGame uid),
               gameMessageToAll [name ++ " leaves."]),
              (return True,
               cond [(gameGetStateAttr gameStarted,
                      do gameUpdateState resetGame
                         gameMessageToAll
                             [name ++ " leaves, ending the game."]),
                     (return True,
                      do gameUpdateState (removePlayer uid)
                         gameMessageToAll [name ++ " leaves."])])]
    gameAppCommands = [("/play",cmdPlay),("/game",cmdGame),("/start",cmdStart),
                       ("/knock",cmdKnock),("/swap",cmdSwap),("/hand",cmdHand)]
    gameAppPollTime _ _ = Nothing
    gameAppPoll = return ()

cond :: Monad m => [(m Bool,m ())] -> m ()
cond [] = return ()
cond ((test,body):rest) = do
    result <- test
    if result
        then body
        else cond rest

cmdPlay :: UserId -> [String] -> Game Zero ()
cmdPlay uid _ =
    cond [(gameGetStateAttr (inGame uid),
           gameMessageToUser uid ["You are already playing."]),
          (gameGetStateAttr gameStarted,
           gameMessageToUser uid ["The game has already started."]),
          (gameGetStateAttr gameFull,
           gameMessageToUser uid ["The game is full."]),
          (return True,
           do gameUpdateState (addPlayer uid)
              name <- gameGetName uid
              gameMessageToAll [name ++ " is playing."])]

cmdGame :: UserId -> [String] -> Game Zero ()
cmdGame uid _ =
    cond [(gameGetStateAttr (not . gameStarted),
           do players <- gameGetStateAttr zeroPlayers
              names <- sequence (map (gameGetName . playerId) players)
              gameMessageToUser uid
                  ["The game has not started.  Players: " ++ unwords names]),
          (return True,
           do game <- gameGetState
              gameMessageToUser uid
                  ["Round " ++ show (zeroRound game) ++ " of "
                           ++ show (length (zeroPlayers game))]
              showScores (gameMessageToUser uid)
              showTable (gameMessageToUser uid)
              (cond [(gameGetStateAttr (inGame uid),cmdHand uid [])]))]

showScores :: ([String] -> Game Zero ()) -> Game Zero ()
showScores showMessage = do
    players <- gameGetStateAttr zeroPlayers
    sequence_ (map showScore players)
  where
    showScore player = do
        name <- gameGetName (playerId player)
        showMessage [name ++ ": " ++ show (playerScore player)]

showTable :: ([String] -> Game Zero ()) -> Game Zero ()
showTable showMessage = do
    cards <- gameGetStateAttr zeroCards
    showMessage ["Table: " ++ unwords (map show cards)]

showHand :: UserId -> Game Zero ()
showHand uid = do
    Just Player { playerCards = cards } <-
        gameGetStateAttr (find ((== uid) . playerId) . zeroPlayers)
    gameMessageToUser uid
        ["Your hand (" ++  show (scoreHand cards) ++ "): "
                       ++ unwords (map show cards)]

cmdStart :: UserId -> [String] -> Game Zero ()
cmdStart uid _ =
    cond [(gameGetStateAttr (not . inGame uid),
           gameMessageToUser uid ["You are not in the game."]),
          (gameGetStateAttr (not . gameCanStart),
           gameMessageToUser uid ["There aren't enough players to start."]),
          (return True,
           do name <- gameGetName uid
              gameMessageToAll [name ++ " starts the game."]
              gameUpdateState
                  (\ g -> deal g { zeroCurrentPlayer = 0, zeroRound = 1 })
              startTurn)]

startTurn :: Game Zero ()
startTurn = do
    showTable gameMessageToAll
    playerIds <- gameGetStateAttr (map playerId . zeroPlayers)
    sequence_ (map showHand playerIds)
    currentPlayerId <- gameGetStateAttr (playerId . currentPlayer)
    gameMessageToUser currentPlayerId ["It is your turn."]

cmdKnock :: UserId -> [String] -> Game Zero ()
cmdKnock uid _ =
    cond [(gameGetStateAttr (not . inGame uid),
           gameMessageToUser uid ["You are not in the game."]),
          (gameGetStateAttr (not . gameCanStart),
           gameMessageToUser uid ["There aren't enough players to start."]),
          (gameGetStateAttr (not . isCurrentPlayer uid),
           gameMessageToUser uid ["It is not your turn."]),
          (gameGetStateAttr (not . zeroKnock),
           do name <- gameGetName uid
              gameMessageToAll [name ++ " knocks.  (First knock.)"]
              gameUpdateState (\ g -> g { zeroKnock = True })
              gameUpdateState nextTurn
              startTurn),
          (return True,
           do name <- gameGetName uid
              gameMessageToAll [name ++ " knocks.  (Last knock.)"]
              player <- gameGetStateAttr currentPlayer
              gameUpdateState (updatePlayer player { playerKnock = True })
              gameUpdateState nextTurn
              cond [(gameGetStateAttr (not . playerKnock . currentPlayer),
                     startTurn),
                    (gameGetStateAttr (not . isLastRound),
                     do gameUpdateState (deal . finishRound)
                        gameMessageToAll ["End of round."]
                        showScores gameMessageToAll
                        startTurn),
                    (return True,
                     do gameUpdateState finishRound
                        gameMessageToAll ["Game over."]
                        showScores gameMessageToAll
                        gameUpdateState resetGame)])]

cmdSwap :: UserId -> [String] -> Game Zero ()
cmdSwap uid [card1,card2] =
    cond [(gameGetStateAttr (not . inGame uid),
           gameMessageToUser uid ["You are not in the game."]),
          (gameGetStateAttr (not . gameCanStart),
           gameMessageToUser uid ["There aren't enough players to start."]),
          (gameGetStateAttr (not . isCurrentPlayer uid),
           gameMessageToUser uid ["It is not your turn."]),
          (gameGetStateAttr
               (not . elem card1 . map show . playerCards . currentPlayer),
           gameMessageToUser uid [card1 ++ " is not in your hand."]),
          (gameGetStateAttr (not . elem card2 . map show . zeroCards),
           gameMessageToUser uid [card2 ++ " is not on the table."]),
          (return True,
           do game <- gameGetState
              player <- gameGetStateAttr currentPlayer
              let [c1] = filter ((== card1) . show) (playerCards player)
              let [c2] = filter ((== card2) . show) (zeroCards game)
              let hand = map (swap c1 c2) (playerCards player)
              let table = map (swap c2 c1) (zeroCards game)
              gameSetState game { zeroCards = table }
              gameUpdateState (updatePlayer player { playerCards = hand })
              name <- gameGetName uid
              if scoreHand hand == 0
                  then do
                      gameMessageToAll
                          [name ++ " swaps " ++ card1 ++ " for " ++ card2
                                ++ ", scoring zero."]
                      if isLastRound game
                          then do
                              gameUpdateState finishRound
                              gameMessageToAll ["Game over."]
                              showScores gameMessageToAll
                              gameUpdateState resetGame
                          else do
                              gameUpdateState (deal . finishRound)
                              gameMessageToAll ["End of round."]
                              showScores gameMessageToAll
                              startTurn
                  else do
                      gameMessageToAll
                          [name ++ " swaps " ++ card1 ++ " for " ++ card2
                                ++ "."]
                      gameUpdateState nextTurn
                      startTurn)]
  where
    swap old new item | item == old = new | otherwise = item
cmdSwap uid _ = gameMessageToUser uid ["/swap card-from-hand card-from-table"]

cmdHand :: UserId -> [String] -> Game Zero ()
cmdHand uid _ =
    cond [(gameGetStateAttr (not . inGame uid),
           gameMessageToUser uid ["You are not playing."]),
          (gameGetStateAttr (not . gameStarted),
           gameMessageToUser uid ["The game has not been started.",
                                  "/start to start it."]),
          (return True,showHand uid)]
