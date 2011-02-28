module Zero(zeroApp) where

import Data.List(find,nub,sortBy)
import Data.Time(UTCTime)
import System.Random(StdGen,getStdGen)

import Apps(GameApp(..),gameApp,App,UserId)
import Shuffle(shuffle)

zeroApp :: IO App
zeroApp = getStdGen >>= gameApp . zeroGame

zeroGame :: StdGen -> Zero
zeroGame stdGen = Zero {
    zeroPlayers = [],
    zeroCurrentPlayer = 0,
    zeroKnock = False,
    zeroCards = [],
    zeroHands = 0,
    zeroStdGen = stdGen
    }

data Zero = Zero {
    zeroPlayers :: [Player],
    zeroCurrentPlayer :: Int,
    zeroKnock :: Bool,
    zeroCards :: [Card],
    zeroHands :: Int,
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

resetGame :: Zero -> Zero
resetGame game =
    game { zeroPlayers = [], zeroKnock = False, zeroCards = [], zeroHands = 0 }

instance GameApp Zero where
    gameName game = "Zero"
    gameAddPlayer game uid getName time =
        (game,[getName uid ++ " arrives."],
         [(uid,["Welcome to Zero.","/play to join the game",
                "/game to see the game",
                "/start to start the game",
                "When playing, /knock or /swap, and /hand to see your hand"])])
    gameRemovePlayer game uid getName time
      | not (uid `elem` (map playerId (zeroPlayers game))) =
        (game,[getName uid ++ " leaves."],[])
      | null (zeroCards game) =
        (game {zeroPlayers = filter ((/= uid) . playerId) (zeroPlayers game)},
         [getName uid ++ " leaves."],[])
      | otherwise =
        (resetGame game,[getName uid ++ " leaves, ending the game."],[])
    gameCommands = [("/game",cmdGame),
                    ("/play",cmdPlay),
                    ("/start",cmdStart),
                    ("/hand",cmdHand),
                    ("/knock",cmdKnock),
                    ("/swap",cmdSwap)]
    gamePollTime game time = Nothing
    gamePoll game getName time = (game,[],[])

cmdGame :: Zero -> UserId -> (UserId -> String) -> UTCTime -> [String]
                -> (Zero,[String],[(UserId,[String])])
cmdGame game uid getName _ _ =
    (game,[],[(uid,showGame game (Just uid) getName)])

cmdPlay :: Zero -> UserId -> (UserId -> String) -> UTCTime -> [String]
                -> (Zero,[String],[(UserId,[String])])
cmdPlay game uid getName time args
  | uid `elem` (map playerId (zeroPlayers game)) =
        (game,[],[(uid,["You are already in the game."])])
  | not $ null (zeroCards game) =
        (game,[],[(uid,["The game has already started."])])
  | length (zeroPlayers game) >= 5 =
        (game,[],[(uid,["The game is already full."])])
  | otherwise =
        (game {
            zeroPlayers = Player {
                            playerId = uid,
                            playerCards = [],
                            playerKnock = False,
                            playerScore = 0
                            } : zeroPlayers game
            },
         [getName uid ++ " is playing."],[])

cmdStart :: Zero -> UserId -> (UserId -> String) -> UTCTime -> [String]
                 -> (Zero,[String],[(UserId,[String])])
cmdStart game uid getName time args
  | not $ null (zeroCards game) =
        (game,[],[(uid,["The game has already started."])])
  | not $ uid `elem` (map playerId (zeroPlayers game)) =
        (game,[],[(uid,["You are not playing."])])
  | length (zeroPlayers game) < 3 =
        (game,[],[(uid,["The game requires at least 3 players."])])
  | otherwise =
        (newGame,showGame newGame Nothing getName,
         map (showPlayerNewGame . playerId) (zeroPlayers game))
  where
    newGame = deal game { zeroCurrentPlayer = 0, zeroHands = 1 }
    showPlayerNewGame uid = (uid,showGame newGame (Just uid) getName)

cmdHand :: Zero -> UserId -> (UserId -> String) -> UTCTime -> [String]
                 -> (Zero,[String],[(UserId,[String])])
cmdHand game uid _ _ _ = (game,[],[(uid,[response])])
  where
    response = maybe "You are not playing."
                     (("Your hand: " ++) . unwords . map show . playerCards)
                     (find ((uid ==) . playerId) (zeroPlayers game))

cmdKnock :: Zero -> UserId -> (UserId -> String) -> UTCTime -> [String]
                -> (Zero,[String],[(UserId,[String])])
cmdKnock game uid getName time args
  | not $ uid `elem` (map playerId (zeroPlayers game)) =
        (game,[],[(uid,["You are not playing."])])
  | null (zeroCards game) =
        (game,[],[(uid,["The game has not started.  /start to start it."])])
  | uid /= playerId (zeroPlayers game !! zeroCurrentPlayer game) =
        (game,[],[(uid,["It's not your turn."])])
  | not (zeroKnock game) =
        finishTurn game { zeroKnock = True } getName
                   (getName uid ++ " knocks.")
  | otherwise =
        finishTurn game { zeroPlayers = map updatePlayer (zeroPlayers game) }
                   getName (getName uid ++ " knocks.")
  where
    updatePlayer player@Player { playerId = uid2 }
      | uid == uid2 = player { playerKnock = True }
      | otherwise = player

cmdSwap :: Zero -> UserId -> (UserId -> String) -> UTCTime -> [String]
                -> (Zero,[String],[(UserId,[String])])
cmdSwap game uid getName time args
  | not $ uid `elem` (map playerId (zeroPlayers game)) =
        (game,[],[(uid,["You are not playing."])])
  | null (zeroCards game) =
        (game,[],[(uid,["The game has not started.  /start to start it."])])
  | uid /= playerId currentPlayer =
        (game,[],[(uid,["It's not your turn."])])
  | length args /= 2 || maybeHandCard == Nothing || maybeTableCard == Nothing =
        (game,[],[(uid,["/swap card-from-hand card-from-table"])])
  | otherwise =
        finishTurn game {
            zeroPlayers = map updatePlayer (zeroPlayers game),
            zeroCards = map (updateCards tableCard handCard) (zeroCards game)
            }
            getName
            (getName uid ++ " swaps " ++ show tableCard
                         ++ " with " ++ show handCard)
  where
    currentPlayer = zeroPlayers game !! zeroCurrentPlayer game
    updateCards oldCard newCard card
      | oldCard == card = newCard
      | otherwise = card
    updatePlayer player@Player { playerId = uid2, playerCards = cards }
      | uid == uid2 =
            player { playerCards = map (updateCards handCard tableCard) cards }
      | otherwise = player
    maybeHandCard = find ((args !! 0 ==) . show) (playerCards currentPlayer)
    maybeTableCard = find ((args !! 1 ==) . show) (zeroCards game)
    Just handCard = maybeHandCard
    Just tableCard = maybeTableCard

showGame :: Zero -> Maybe UserId -> (UserId -> String) -> [String]
showGame game maybeUid getName
  | zeroHands game == 0 =
        ["The game has not started.  Players: "
         ++ unwords (map (getName . playerId) (zeroPlayers game))]
  | otherwise =
    ["Hand " ++ show (zeroHands game) ++ " of "
             ++ show (length (zeroPlayers game)),
     "Scores:"]
    ++ showScores game getName
    ++ showTable game getName
    ++ maybe [] showHand
             (find ((maybeUid ==) . Just . playerId) (zeroPlayers game))
  where
    showHand Player { playerCards = cards } =
        ["Your hand (" ++ show (scoreHand cards) ++ "): "
                       ++ unwords (map show cards)]

showScores :: Zero -> (UserId -> String) -> [String]
showScores game getName =
    map showScore (sortBy compareScore (zeroPlayers game))
  where
    compareScore p1 p2 = compare (playerScore p1) (playerScore p2)
    showScore p =
        " " ++ getName (playerId p) ++ ": " ++ show (playerScore p)

showTable :: Zero -> (UserId -> String) -> [String]
showTable game getName =
    ["Table: " ++ unwords (map show (zeroCards game)),
     "Current player: "
     ++ (getName $ playerId $ zeroPlayers game !! zeroCurrentPlayer game)]

finishTurn :: Zero -> (UserId -> String) -> String
                   -> (Zero,[String],[(UserId,[String])])
finishTurn game getName message
  | scoreHand (playerCards currentPlayer) == 0 =
        finishHand game getName
                   [message,getName (playerId currentPlayer) ++ " gets zero."]
  | zeroKnock game && playerKnock nextPlayer =
        finishHand game getName [message]
  | otherwise = (newGame,[message],map getMessage (zeroPlayers game))
  where
    currentPlayer = zeroPlayers game !! zeroCurrentPlayer game
    nextPlayerIndex
        = (zeroCurrentPlayer game + 1) `mod` length (zeroPlayers game)
    nextPlayer = zeroPlayers game !! nextPlayerIndex
    newGame = game { zeroCurrentPlayer = nextPlayerIndex }
    getMessage player =
        (playerId player,
         (message : showTable newGame getName)
         ++ ["Your hand (" ++ show (scoreHand (playerCards player)) ++ "): "
                           ++ unwords (map show (playerCards player))])

finishHand :: Zero -> (UserId -> String) -> [String]
                   -> (Zero,[String],[(UserId,[String])])
finishHand game getName messages
  | zeroHands game >= length (zeroPlayers game) =
        (game {
             zeroPlayers = [],
             zeroCurrentPlayer = 0,
             zeroKnock = False,
             zeroCards = [],
             zeroHands = 0
             },messages ++ ["Game over:"] ++ showScores newGame getName,[])
  | otherwise =
        (newGame,
         messages ++ ["Hand over:"] ++ showGame newGame Nothing getName,
         map (showPlayerNewGame . playerId) (zeroPlayers game))
  where
    newGame = deal game {
        zeroPlayers = map updateScore (zeroPlayers game),
        zeroCurrentPlayer = zeroHands game,
        zeroHands = zeroHands game + 1
        }
    updateScore player =
        player {
            playerScore = playerScore player + scoreHand (playerCards player)
            }
    showPlayerNewGame uid =
        (uid,messages ++ "Hand over:" : showGame newGame (Just uid) getName)
