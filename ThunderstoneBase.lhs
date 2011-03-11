Prototype the Thunderstone base game.  Avoid the complexities of the
expansions to work out how to implement the basic mechanisms.

> module ThunderstoneBase

The interface:

Data types:

ThunderstoneState and PlayerId are opaque.

>     (ThunderstoneState,PlayerId,
>      Card(..),PlayerState(..),PlayerAction(..),ThunderstoneEvent(..),
>      GameSetup,

Initialization:

>      thunderstoneInit,

Game setup:

>      firstGame,randomGame,specifiedGame,
>      thunderstoneStartGame,

Game state:

>      thunderstoneGameOver,
>      thunderstoneDungeonHall,
>      thunderstoneVillageHeroes,
>      thunderstoneVillageResources,
>      thunderstonePlayerIds,
>      thunderstonePlayerHand,
>      thunderstonePlayerScore,
>      thunderstonePlayerState,

Game mechanics:

>      thunderstonePlayerOptions,
>      thunderstonePerformAction)

> where

> import Control.Monad(mapM,mapM_,replicateM,unless,when)
> import Data.List(nub,sortBy,(\\))
> import Data.Maybe(catMaybes,listToMaybe)
> import System.Random(StdGen)

> import qualified Shuffle
> import StateTransformer
>     (StateTransformer,getState,runStateTransformer,setState)
> import ThunderstoneCards

==============================================================================
Exported functions:

Initialize the state with the random number generator.

> thunderstoneInit :: StdGen -> ThunderstoneState
> thunderstoneInit stdGen = ThunderstoneState {
>     thunderstoneStdGen = stdGen,
>     thunderstoneCurrentPlayer = undefined,
>     thunderstonePlayers = [],
>     thunderstoneDungeon = undefined,
>     thunderstoneHeroes = undefined,
>     thunderstoneVillage = undefined
>     }

Start a game, specifying the number of players and choosing which
cards are in the game, which can be either a specific set of cards,
or randomly chosen cards.

> thunderstoneStartGame :: ThunderstoneState -> Int -> GameSetup
>                       -> (ThunderstoneState,[PlayerId])
> thunderstoneStartGame state numberOfPlayers gameSetup =
>     runStateTransformer (setup numberOfPlayers gameSetup) state

Various queries about the state of the game.

> thunderstoneGameOver :: ThunderstoneState -> Bool
> thunderstoneGameOver state = thunderstoneGetState state isGameOver

> thunderstoneDungeonHall :: ThunderstoneState -> (Card,Card,Card)
> thunderstoneDungeonHall state = undefined

> thunderstoneVillageHeroes :: ThunderstoneState -> [(HeroType,[HeroCard])]
> thunderstoneVillageHeroes state = undefined

> thunderstoneVillageResources :: ThunderstoneState -> [(VillageCard,Int)]
> thunderstoneVillageResources state = undefined

> thunderstonePlayerIds :: ThunderstoneState -> [PlayerId]
> thunderstonePlayerIds state = thunderstoneGetState state getPlayerIds

> thunderstonePlayerHand :: ThunderstoneState -> PlayerId -> [Card]
> thunderstonePlayerHand state playerId =
>     thunderstoneGetState state (getHand playerId)

> thunderstonePlayerScore :: ThunderstoneState -> PlayerId -> Int
> thunderstonePlayerScore state playerId =
>     thunderstoneGetState state (getScore playerId)

> thunderstonePlayerState :: ThunderstoneState -> PlayerId -> PlayerState
> thunderstonePlayerState state playerId =
>     thunderstoneGetState state (getPlayerState playerId)

> thunderstonePlayerOptions :: ThunderstoneState -> PlayerId -> [PlayerAction]
> thunderstonePlayerOptions state playerId =
>     thunderstoneGetState state (getPlayerOptions playerId)

A player performs an action.  If the action is legal, the updated state
and a list of events are returned.

> thunderstonePerformAction :: ThunderstoneState -> PlayerId -> PlayerAction
>                           -> Maybe (ThunderstoneState,[ThunderstoneEvent])
> thunderstonePerformAction = undefined

> firstGame :: GameSetup
> firstGame = return (monsters,heroes,village)
>   where
>     monsters = [MonsterEnchanted, MonsterOoze, MonsterUndeadDoom]
>     heroes = [HeroElf, HeroLorigg, HeroRegian, HeroThyrian]
>     village = [BattleFury, Fireball, FlamingSword, LightstoneGem,
>                MagicalAura, ShortSword, Spear, TownGuard]

Use the Randomizer cards to choose three random monsters, four
random heros, and eight village cards. 

For a longer game, try four or more monsters.

> randomGame :: GameSetup
> randomGame = do
>     monsters <- shuffle [MonsterAbyssal .. MonsterUndeadSpirit]
>     heroes <- shuffle [HeroAmazon .. HeroThyrian]
>     village <- shuffle [ArcaneEnergies .. Warhammer]
>     return (take 3 monsters, take 4 heroes, take 8 village)

Specify the cards in the game.  If not enough are specified, randomly
choose from the remaining cards.  If too many are specified, randomly
choose among the specified cards.

> specifiedGame :: [MonsterType] -> [HeroType] -> [VillageCard] -> GameSetup
> specifiedGame monsters heroes village = do
>     randomMonsters <- random monsters [MonsterAbyssal .. MonsterUndeadSpirit]
>     randomHeroes <- random heroes [HeroAmazon .. HeroThyrian]
>     randomVillage <- random village [ArcaneEnergies .. Warhammer]
>     return (take 3 randomMonsters, take 4 randomHeroes, take 8 randomVillage)
>   where
>     random specified others = do
>         shuffledSpecified <- shuffle (nub specified)
>         shuffledOthers <- shuffle (others \\ shuffledSpecified)
>         return (shuffledSpecified ++ shuffledOthers)

==============================================================================

> data Card =
>     MonsterCard MonsterCard
>   | HeroCard HeroCard
>   | VillageCard VillageCard
>   | DiseaseCard
>   | ThunderstoneCard ThunderstoneCard
>   deriving Eq

> instance Show Card where
>     show (MonsterCard card) = cardName $ monsterDetails card
>     show (HeroCard card) = cardName $ heroDetails card
>     show (VillageCard card) = cardName $ villageDetails card
>     show DiseaseCard = "Disease"
>     show (ThunderstoneCard card) = cardName $ thunderstoneDetails card

Game setup:

> type GameSetup = Thunderstone ([MonsterType],[HeroType],[VillageCard])

> setup :: Int -> GameSetup -> Thunderstone [PlayerId]
> setup numberOfPlayers gameSetup = do
>     (monsters,heroes,village) <- gameSetup
>     dungeon <- buildDungeon monsters
>     state <- getState
>     setState state {
>         thunderstoneCurrentPlayer = 0,
>         thunderstonePlayers = replicate numberOfPlayers newPlayer,
>         thunderstoneDungeon = dungeon,
>         thunderstoneHeroes = map heroStack (HeroMilitia:heroes),
>         thunderstoneVillage = map villageStack ([Dagger .. Torch] ++ village)
>         }
>     playerIds <- getPlayerIds
>     mapM_ drawStartingHand playerIds
>     currentPlayerId <- getCurrentPlayerId
>     setPlayerState currentPlayerId StartingTurn
>     return playerIds

>   where

>     newPlayer :: Player
>     newPlayer = Player {
>         playerHand = [],
>         playerDeck = [],
>         playerDiscard = [],
>         playerXP = 0,
>         playerState = Waiting
>         }

>     drawStartingHand :: PlayerId -> Thunderstone ()
>     drawStartingHand playerId = do
>         militia <- multiple 6 $ drawHero HeroMilitia
>         dagger <- multiple 2 $ drawResource Dagger
>         ironRations <- multiple 2 $ drawResource IronRations
>         torch <- multiple 2 $ drawResource Torch
>         discard playerId (map HeroCard militia)
>         discard playerId (map VillageCard (dagger ++ ironRations ++ torch))
>         hand <- multiple 6 $ drawCard playerId
>         setHand playerId hand

There are ten cards for each class.  Take all 30 Monster cards that match
the three selected classes and shuffle them together.  This becomes the
Dungeon Deck.  Count off ten Monster cards (without revealing them) and
shuffle them together with the special Thunderstone card.  Place these
eleven cards at the bottom of the Dungeon Deck.

>     buildDungeon :: [MonsterType] -> Thunderstone [Card]
>     buildDungeon monsters = do
>         dungeon <- shuffle $ map MonsterCard cards
>         shuffleToBottom 10 [ThunderstoneCard StoneOfMystery] dungeon
>       where
>         cards = concatMap (cardsOfType monsterDetails) monsters

Place both level 3 Hero cards in the stack.  Next, place all four level
2 Hero cards on top of the stack.  Finally, place all six level 1 Hero
cards on top of those.  This will create a stack of Hero cards with all
level 3 cards on the bottom, the level 2 cards in the middle, and the
level 1 cards on top.

>     heroStack :: HeroType -> (HeroType,[HeroCard])
>     heroStack hero = (hero,sortBy level (cardsOfType heroDetails hero))
>       where
>         level a b = compare (heroLevel $ cardStats $ heroDetails a)
>                             (heroLevel $ cardStats $ heroDetails b)

There are 15 of each basic card and 8 of each village card.

>     villageStack :: VillageCard -> (VillageCard,Int)
>     villageStack card = (card,length (cardsOfType villageDetails card))

Game state:

> type Thunderstone a = StateTransformer ThunderstoneState a

> newtype PlayerId = PlayerId Int

> data ThunderstoneState = ThunderstoneState {
>     thunderstoneStdGen :: StdGen,
>     thunderstoneCurrentPlayer :: Int,
>     thunderstonePlayers :: [Player],
>     thunderstoneDungeon :: [Card],
>     thunderstoneHeroes :: [(HeroType,[HeroCard])],
>     thunderstoneVillage :: [(VillageCard,Int)]
>     }

> data Player = Player {
>     playerHand :: [Card],
>     playerDeck :: [Card],
>     playerDiscard :: [Card],
>     playerXP :: Int,
>     playerState :: PlayerState
>     }

> data PlayerState =
>     StartingTurn

>   | UsingVillageEffects {
>         villageEffectsUnusedCards :: [Card],
>         villageEffectsUsedCards :: [Card],
>         villageEffectsNumberOfBuys :: Int,
>         villageEffectsGold :: Int
>         }
>   | Purchasing {
>         purchasingNumberOfBuys :: Int,
>         purchasingGold :: Int
>         }
>   | LevelingUpHeroes

>   | UsingDungeonEffects
>   | AttackingMonster
>   | TakingSpoils

>   | Waiting
>   deriving (Eq)

Nonactive players may need to take actions due to card effects.

The active player must either Visit the Village or Enter the Dungeon or Rest.

Visit the Village
1. Reveal your hand
2. Use Village Effects
3. Produce gold
4. Purchase card(s)
5. Level up Hero cards
6. End turn

Enter the Dungeon
1. Reveal your hand
2. Use DungeonEffects
3. Select Monster and rank to attack
4. Resolve the battle
   a. Calculate Attack Value
   b. Resolve Battle Effects
   c. Undefeated Monster to bottom of Dungeon Deck
   d. Defeated Monster and Disease in discard pile
   e. Receive spoils
   f. Resolve Breach Effects
5. End turn

Rest
1. Destroy one card
2. End turn

> data PlayerAction =
>     VisitVillage
>   | UseVillageEffects Card
>   | PurchaseHero HeroType
>   | PurchaseResource VillageCard
>   | LevelUpHero Card
>   | EndTurn

>   | EnterDungeon
>   | EquipHero Card Card
>   | AttackMonster DungeonRank

>   | Rest (Maybe Card)

> newtype DungeonRank = DungeonRank Int

> data ThunderstoneEvent =
>     PlayerEvent PlayerId PlayerAction
>   | GameOverEvent [(PlayerId,Int)]

Plus breach effects and other things that players should be notified of.


Game mechanics

> isGameOver :: Thunderstone Bool
> isGameOver = undefined

> getScores :: Thunderstone [(PlayerId,Int)]
> getScores = do
>     playerIds <- getPlayerIds
>     scores <- mapM getScore playerIds
>     return (zip playerIds scores)

> getScore :: PlayerId -> Thunderstone Int
> getScore playerId = do
>     player <- getPlayer playerId
>     return $ sum $ map cardScore
>            $ playerHand player ++ playerDeck player ++ playerDiscard player

> cardScore :: Card -> Int
> cardScore (MonsterCard card) = cardVictoryPoints $ monsterDetails card
> cardScore (HeroCard card) = cardVictoryPoints $ heroDetails card
> cardScore (VillageCard card) = cardVictoryPoints $ villageDetails card
> cardScore DiseaseCard = 0
> cardScore (ThunderstoneCard card) =
>     cardVictoryPoints $ thunderstoneDetails card

> getPlayerOptions :: PlayerId -> Thunderstone [PlayerAction]
> getPlayerOptions playerId = do
>     gameOver <- isGameOver
>     if gameOver
>       then return []
>       else do
>         playerState <- getPlayerState playerId
>         optionsWhen playerState
>   where
>     optionsWhen StartingTurn = do
>         hand <- getHand playerId
>         return ([VisitVillage, EnterDungeon, Rest Nothing]
>                 ++ map (Rest . Just) hand)
>     optionsWhen UsingVillageEffects {} = undefined
>     optionsWhen Purchasing {} = undefined
>     optionsWhen LevelingUpHeroes = undefined
>     optionsWhen UsingDungeonEffects = undefined
>     optionsWhen AttackingMonster = undefined
>     optionsWhen TakingSpoils = undefined
>     optionsWhen Waiting = return []

> takeAction :: PlayerId -> PlayerAction -> Thunderstone Bool
> takeAction playerId playerAction = do
>     gameOver <- isGameOver
>     if gameOver
>       then return False
>       else do
>         playerState <- getPlayerState playerId
>         status <- performAction playerState playerAction
>         gameOver <- isGameOver
>         unless gameOver startNextTurnIfTurnFinished
>         return status

>   where

>     performAction :: PlayerState -> PlayerAction -> Thunderstone Bool
>     performAction StartingTurn VisitVillage = do
>         hand <- getHand playerId
>         setPlayerState playerId
>                        UsingVillageEffects {
>                            villageEffectsUnusedCards = hand,
>                            villageEffectsUsedCards = [],
>                            villageEffectsNumberOfBuys = 0,
>                            villageEffectsGold = 0
>                            }
>         return True
>     performAction StartingTurn EnterDungeon = do
>         setPlayerState playerId
>                        UsingDungeonEffects
>         return True
>     performAction StartingTurn (Rest (Just card)) = do
>         hand <- getHand playerId
>         if card `elem` hand
>           then do
>             setHand playerId (remove1 card hand)
>             endTurn
>             return True
>           else return False
>     performAction StartingTurn (Rest Nothing) = do
>         endTurn
>         return True
>     performAction StartingTurn _ = return False

... more actions ...

End your turn by discarding all cards face up on your discard pile,
and draw six new cards to form a new hand.

>     endTurn :: Thunderstone ()
>     endTurn = do
>         hand <- getHand playerId
>         discard playerId hand
>         hand <- multiple 6 $ drawCard playerId
>         setHand playerId hand

>     startNextTurnIfTurnFinished :: Thunderstone ()
>     startNextTurnIfTurnFinished = do
>         turnFinished <- isTurnFinished
>         when turnFinished (do
>             state <- getState
>             setState state {
>                 thunderstoneCurrentPlayer =
>                     thunderstoneCurrentPlayer state + 1
>                         `mod` length (thunderstonePlayers state)
>                 }
>             currentPlayerId <- getCurrentPlayerId
>             setPlayerState currentPlayerId StartingTurn)

>     isTurnFinished :: Thunderstone Bool
>     isTurnFinished = do
>         playerIds <- getPlayerIds
>         playerStates <- mapM getPlayerState playerIds
>         return (all (== Waiting) playerStates)

Low level game mechanics

> discard :: PlayerId -> [Card] -> Thunderstone ()
> discard playerId cards = do
>     player <- getPlayer playerId
>     setPlayer playerId
>               player { playerDiscard = cards ++ playerDiscard player }

The discard deck is only shuffled when no cards remain in your deck and
you need to draw cards.  All cards in the discard pile are shuffled
together.

> drawCard :: PlayerId -> Thunderstone (Maybe Card)
> drawCard playerId = do
>     player <- getPlayer playerId
>     if null (playerDeck player)
>       then
>         if null (playerDiscard player)
>           then
>             return Nothing
>           else do
>             deck <- shuffle (playerDiscard player)
>             setPlayer playerId
>                       player { playerDeck = deck, playerDiscard = [] }
>             drawCard playerId
>       else do
>         setPlayer playerId
>                   player { playerDeck = tail (playerDeck player) }
>         return (Just (head (playerDeck player)))

Low level game state transformations.

> shuffle :: [a] -> Thunderstone [a]
> shuffle deck = do
>     state <- getState
>     let (stdGen,shuffled) = Shuffle.shuffle (thunderstoneStdGen state) deck
>     setState state { thunderstoneStdGen = stdGen }
>     return shuffled

> shuffleToBottom :: Int -> [a] -> [a] -> Thunderstone [a]
> shuffleToBottom count cards deck = do
>     bottom <- shuffle (cards ++ take count deck)
>     return (drop count deck ++ bottom)

> getCurrentPlayerId :: Thunderstone PlayerId
> getCurrentPlayerId = do
>     state <- getState
>     return (PlayerId (thunderstoneCurrentPlayer state))

> getPlayerIds :: Thunderstone [PlayerId]
> getPlayerIds = do
>     state <- getState
>     return (map PlayerId [0..length (thunderstonePlayers state) - 1])

> getPlayer :: PlayerId -> Thunderstone Player
> getPlayer (PlayerId playerNumber) = do
>     state <- getState
>     return (thunderstonePlayers state !! playerNumber)

> setPlayer :: PlayerId -> Player -> Thunderstone ()
> setPlayer (PlayerId playerNumber) player = do
>     state <- getState
>     let players = thunderstonePlayers state
>     setState state {
>         thunderstonePlayers = take playerNumber players
>                            ++ player : drop (playerNumber + 1) players
>         }

> getHand :: PlayerId -> Thunderstone [Card]
> getHand playerId = fmap playerHand (getPlayer playerId)

> setHand :: PlayerId -> [Card] -> Thunderstone ()
> setHand playerId cards = do
>     player <- getPlayer playerId
>     setPlayer playerId player { playerHand = cards }

> getPlayerState :: PlayerId -> Thunderstone PlayerState
> getPlayerState playerId = fmap playerState (getPlayer playerId)

> setPlayerState:: PlayerId -> PlayerState -> Thunderstone ()
> setPlayerState playerId state = do
>      player <- getPlayer playerId
>      setPlayer playerId player { playerState = state }

> getVillage :: Thunderstone [(VillageCard,Int)]
> getVillage = do
>     state <- getState
>     return (thunderstoneVillage state)

More briefly: fmap thunderstoneVillage getState

> getResourceCount :: VillageCard -> Thunderstone Int
> getResourceCount card = do
>     resources <- getVillage
>     return (maybe 0 id (lookup card resources))

More briefly: fmap (maybe 0 id . lookup card) getResources

> setResourceCount :: VillageCard -> Int -> Thunderstone ()
> setResourceCount card count = do
>     state <- getState
>     let village = thunderstoneVillage state
>     setState state {
>         thunderstoneVillage = map setCount (thunderstoneVillage state)
>         }
>   where
>     setCount resource@(resourceCard,_)
>       | resourceCard == card = (resourceCard,count)
>       | otherwise = resource

> drawResource :: VillageCard -> Thunderstone (Maybe VillageCard)
> drawResource card = do
>     count <- getResourceCount card
>     if count > 0
>       then do
>         setResourceCount card (count - 1)
>         return (Just card)
>       else
>         return Nothing

> topHero :: HeroType -> Thunderstone (Maybe HeroCard)
> topHero heroType = do
>     state <- getState
>     let maybeStack = lookup heroType (thunderstoneHeroes state)
>     return (maybe Nothing listToMaybe maybeStack)

More briefly:
fmap (maybe Nothing listToMaybe . lookup heroType . thunderstoneHeros) getState

> drawHero :: HeroType -> Thunderstone (Maybe HeroCard)
> drawHero heroType = do
>     maybeHero <- topHero heroType
>     unless (maybeHero == Nothing) (do
>         state <- getState
>         setState state {
>             thunderstoneHeroes = map updateHeroes (thunderstoneHeroes state)
>             })
>     return maybeHero
>   where
>     updateHeroes heroes@(hero,stack)
>       | hero == heroType = (hero,drop 1 stack)
>       | otherwise = heroes

When you level up, destroy the Hero card in your hand and pay the level
cost of the Hero card in Experience Points.  Then, search the stack of
matching Hero cards in the Village and find the card showing the next
higher level for the Hero type you destroyed.  Place this card on the
top of your discard pile.  Level 1 Heroes level up to 2, and Level 2
level up to 3.  However, you may not level the same Hero card twice in
one turn, i.e. from Level 1 to 3, and you may never skip a Level.

> drawHeroUpgrade :: HeroCard -> Thunderstone (Maybe HeroCard)
> drawHeroUpgrade heroCard = do
>     state <- getState
>     let stack = lookup heroType (thunderstoneHeroes state)
>     if heroCard `containedIn` stack
>       then do
>         setState state {
>             thunderstoneHeroes = map updateHeroes (thunderstoneHeroes state)
>             }
>         return (Just heroCard)
>       else return Nothing
>   where
>     heroType = cardType $ heroDetails heroCard
>     containedIn card Nothing = False
>     containedIn card (Just stack) = card `elem` stack
>     updateHeroes heroes@(hero,stack)
>       | hero == heroType = (hero,remove1 heroCard stack)
>       | otherwise = heroes

> multiple :: (Functor m, Monad m) => Int -> m (Maybe a) -> m [a]
> multiple count action = fmap catMaybes (replicateM count action)

> remove1 :: Eq a => a -> [a] -> [a]
> remove1 item items = before ++ drop 1 after
>   where
>     (before,after) = span (/= item) items

> thunderstoneGetState :: ThunderstoneState -> Thunderstone a -> a
> thunderstoneGetState state getThunderstoneState =
>     snd $ runStateTransformer getThunderstoneState state
