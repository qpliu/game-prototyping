module PicoApp(picoApp) where

import Data.List(partition)
import Data.Time(UTCTime)
import System.Random(StdGen,getStdGen)

import Apps(GameApp(..),gameApp,App,UserId)
import Shuffle(shuffle)

picoApp :: IO App
picoApp = getStdGen >>= gameApp . picoGame

picoGame :: StdGen -> PicoGame
picoGame stdGen = PicoGame {
    picoPlayers = [],
    picoStdGen = stdGen
    }

newtype Card = Card Int
    deriving Eq

instance Show Card where
    show (Card n) = show n

cards :: [Card]
cards = map Card (16:[4..13])

score :: [Card] -> Int
score cards = sum (map (\ (Card n) -> n) cards)

defeats :: Card -> Card -> Bool
defeats (Card i) (Card j) = (i > j && i <= 2*j) || 2*i < j

data PicoGame = PicoGame {
    picoPlayers :: [(UserId,PicoPlayer)],
    picoStdGen :: StdGen
    }

data PicoPlayer = PicoPlayer {
    picoHand :: [Card],
    picoWins :: [Card],
    picoPlayed :: Maybe Card
    }

instance GameApp PicoGame where
    gameName _ = "Pico"
    gameAddPlayer game uid getName time =
        (game,[getName uid ++ " has arrived."],[])
    gameRemovePlayer game uid getName time =
        if uid `elem` map fst (picoPlayers game)
            then (game { picoPlayers = [] },
                  [getName uid ++ " has left, ending the game."],[])
            else (game,[getName uid ++ " has left."],[])
    gameCommands =
        [("/play",picoPlay),
         ("/game",picoGameState)]
    gamePollTime game time = Nothing
    gamePoll game time = (game,[],[])

picoPlay :: PicoGame -> UserId -> (UserId -> String) -> UTCTime -> [String]
                     -> (PicoGame,[String],[(UserId,[String])])
picoPlay game uid getName time args
  | null (picoPlayers game) =
        (game { picoPlayers = [(uid,undefined)] },
         [getName uid ++ " has started playing.","/play to join the game."],
         [(uid,["You are the first player in the game."])])
  | length (picoPlayers game) == 1 && uid `elem` (map fst (picoPlayers game)) =
        (game,[],[(uid,["You are the first player in the game."])])
  | length (picoPlayers game) == 1 = initGame
  | not (uid `elem` (map fst (picoPlayers game))) =
        (game,[],[(uid,["The game is full."])])
  | otherwise = playMove (partition ((== uid) . fst) (picoPlayers game))
  where
    initGame =
        let (stdGen,_:deck) = shuffle (picoStdGen game) cards
            (deck1,deck2) = splitAt (length deck `div` 2) deck
            [(uid2,_)] = picoPlayers game
            player1 = PicoPlayer {
                        picoHand = deck1,
                        picoWins = [],
                        picoPlayed = Nothing
                        }
            player2 = PicoPlayer {
                        picoHand = deck2,
                        picoWins = [],
                        picoPlayed = Nothing
                        }
        in  (game {
                picoPlayers = [(uid,player1),(uid2,player2)],
                picoStdGen = stdGen
                },
             [getName uid ++ " and " ++ getName uid2 ++ " started a game."],
             [(uid,picoPlayerState uid getName (uid,player1)),
              (uid2,[getName uid ++ " joined the game."]
                    ++ picoPlayerState uid2 getName (uid2,player2))])
    playedCard = Card (read (unwords args))
    playMove ((uid1,player1):_,(uid2,player2):_)
      | Nothing /= picoPlayed player1 =
            (game,[],[(uid,["You already played this turn."])])
      | not ((unwords args) `elem` (map show (picoHand player1))) =
            (game,[],[(uid,["Invalid play."])])
      | Nothing == picoPlayed player2 =
            (game {
                picoPlayers = [(uid1,player1 {
                                    picoHand = filter (/= playedCard)
                                                      (picoHand player1),
                                    picoPlayed = Just playedCard
                                    }),
                               (uid2,player2)]
                },
             [getName uid ++ " plays a card."],
             [(uid,["You play " ++ show playedCard])])
      | otherwise =
            let Just otherCard = picoPlayed player2
            in  if playedCard `defeats` otherCard
                    then finishTurn uid1 (win player1 playedCard) playedCard
                                    uid2 (lose player2 otherCard) otherCard
                    else finishTurn uid2 (win player2 otherCard) otherCard
                                    uid1 (lose player1 playedCard) playedCard
    finishTurn uidwin playerwin cardwin uidlose playerlose cardlose
      | length (picoHand playerwin) <= 1 =
            (game { picoPlayers = [] },
             [getName uidwin ++ " wins the hand with " ++ show cardwin,
              getName uidlose ++ " loses the hand with " ++ show cardlose,
              "Game over.",
              getName uidwin ++ ": " ++ show (score (picoWins playerwin)),
              getName uidlose ++ ": " ++ show (score (picoWins playerlose))],
             [])
      | otherwise =
            (game { picoPlayers = [(uidwin,playerwin),(uidlose,playerlose)] },
             [getName uidwin ++ " wins the hand with " ++ show cardwin,
              getName uidlose ++ " loses the hand with " ++ show cardlose],
             [])
    win player card =
        player {
            picoHand = filter (/= card) (picoHand player),
            picoWins = card : picoWins player,
            picoPlayed = Nothing
            }
    lose player card =
        player {
            picoHand = card : filter (/= card) (picoHand player),
            picoPlayed = Nothing
            }

picoGameState :: PicoGame -> UserId -> (UserId -> String)
                          -> UTCTime -> [String]
                          -> (PicoGame,[String],[(UserId,[String])])
picoGameState game uid getName time args
  | null (picoPlayers game) =
        (game,[],[(uid,["Nobody is playing.",
                        "/play to be the first player in the game."])])
  | map fst (picoPlayers game) == [uid] =
        (game,[],[(uid,["You are the only player in the game."])])
  | length (picoPlayers game) == 1 =
        (game,[],[(uid,[(getName $ fst $ head $ picoPlayers game)
                        ++ " is the only player in the game.",
                        "/play to be the second player in the game."])])
  | otherwise =
        (game,[],[(uid,concatMap (picoPlayerState uid getName)
                                 (picoPlayers game))])

picoPlayerState :: UserId -> (UserId -> String) -> (UserId,PicoPlayer)
                          -> [String]
picoPlayerState uid1 getName (uid2,player)
  | uid1 == uid2 =
        ["You: hand: " ++ unwords (map show (picoHand player))
             ++ " won: " ++ unwords (map show (picoWins player))
             ++ " score: " ++ show (score (picoWins player))]
        ++ maybe [] ((:[]) . ("You are playing " ++) . show)
                 (picoPlayed player)
  | otherwise =
        [getName uid2 ++ ": hand size: " ++ show (length (picoHand player))
             ++ " won: " ++ unwords (map show (picoWins player))
             ++ " score: " ++ show (score (picoWins player)),
         getName uid2 ++ maybe " is choosing a card to play"
                               (const " has played a card")
                               (picoPlayed player)]
