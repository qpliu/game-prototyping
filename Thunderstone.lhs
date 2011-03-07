> module Thunderstone
> where

> import Control.Monad(mapM,mapM_,replicateM,unless,when)
> import Data.List(find,findIndices,nub,subsequences,(\\))
> import Data.Maybe(catMaybes,listToMaybe)
> import System.Random(StdGen)

> import qualified Shuffle
> import StateTransformer(StateTransformer,getState,setState)

Data type that represents all the Thunderstone playing cards.

> data Card =
>     Thunderstone
>   | Monster MonsterCard
>   | Basic BasicType
>   | Hero HeroType HeroLevel
>   | Village VillageType
>   | Disease
>   deriving (Eq,Show)

Data types that represent card types, as well as the Randomizer cards.

> data MonsterType =
>     Abyssal
>   | Doomknight
>   | Dragon
>   | Enchanted
>   | Humanoid
>   | Ooze
>   | UndeadDoom
>   | UndeadSpirit
>   deriving (Bounded,Enum,Eq,Show)

> data BasicType =
>     Militia
>   | Dagger
>   | IronRations
>   | Torch
>   deriving (Bounded,Enum,Eq,Show)

> data HeroType =
>     Amazon
>   | Chalice
>   | Dwarf
>   | Elf
>   | Feayn
>   | Lorigg
>   | Outlands
>   | Redblade
>   | Regian
>   | Selurin
>   | Thyrian
>   deriving (Bounded,Enum,Eq,Show)

> data VillageType =
>     ArcaneEnergies
>   | Banish
>   | Barkeep
>   | BattleFury
>   | Feast
>   | Fireball
>   | FlamingSword
>   | Goodberries
>   | Hatchet
>   | Lantern
>   | LightstoneGem
>   | MagicalAura
>   | Pawnbroker
>   | Polearm
>   | ShortSword
>   | Spear
>   | TownGuard
>   | Trainer
>   | Warhammer
>   deriving (Bounded,Enum,Eq,Show)

Hero cards have three levels.

> newtype HeroLevel = HeroLevel Int
>   deriving (Eq,Show)

The specific monsters for each monster type.

> data MonsterCard =
>     ArchdukeOfPain
>   | BlackSlime
>   | BlinkDog
>   | BloodskullOrc
>   | DeadboneTroll
>   | EbonFume
>   | FirebrandCyclops
>   | Ghost
>   | GrayOoze
>   | GreenBlob
>   | GrayskinLizard
>   | GriknackGoblin
>   | Griffon
>   | Grudgebeast
>   | Haunt
>   | Knightmare
>   | LordOfDeath
>   | LordMortis
>   | Mythlurian
>   | Nixie
>   | NoxiousSlag
>   | Pegasus
>   | Prince
>   | RedJelly
>   | Revenant
>   | Skaladak
>   | Spectre
>   | Sphinx
>   | Succubus
>   | Suffering
>   | Tormentor
>   | TyxrTheOld
>   | Unchained
>   | UyrilUnending
>   | Wraith
>   deriving (Eq,Show)

> monsterCards :: MonsterType -> [MonsterCard]
> monsterCards Abyssal = []
> monsterCards Doomknight = []
> monsterCards Dragon = []
> monsterCards Enchanted = []
> monsterCards Humanoid = []
> monsterCards Ooze = []
> monsterCards UndeadDoom = []
> monsterCards UndeadSpirit = []

Game setup:

For the first game, a specific set of cards is chosen.
Otherwise, the Randomizer cards are used to select a random set of cards.

> type GameSetup = Thunderstone ([MonsterType],[HeroType],[VillageType])

Cards for the first game:

> firstGame :: GameSetup
> firstGame = return (monsters,heroes,village)
>   where
>     monsters = [Enchanted, Ooze, UndeadDoom]
>     heroes = [Elf, Lorigg, Regian, Thyrian]
>     village = [BattleFury, Fireball, FlamingSword, LightstoneGem,
>                MagicalAura, ShortSword, Spear, TownGuard]

Use the Randomizer cards to choose three random monsters, four
random heros, and eight village cards. 

For a longer game, try four or more monsters.

> randomizer :: Int -> GameSetup
> randomizer monsterCount = do
>     monsters <- randomize monsterCount
>     heroes <- randomize 4
>     village <- randomize 8
>     return (monsters,heroes,village)
>   where
>     randomize count = do
>         shuffled <- shuffle [minBound..maxBound]
>         return (take count shuffled)

For playing with specific cards, with random cards added if not enough
cards are specified.

> specificCards :: Int -> [MonsterType] -> [HeroType] -> [VillageType]
>               -> GameSetup
> specificCards monsterCount monsters heroes village = do
>     monsters' <- randomize monsterCount monsters
>     heroes' <- randomize 4 heroes
>     village' <- randomize 8 village
>     return (monsters',heroes',village')
>   where
>     randomize count specified = do
>         included <- shuffle (nub specified)
>         randoms <- shuffle ([minBound..maxBound] \\ included)
>         return (take count (included ++ randoms))

Setup:
1. Populate the Dungeon
2. Populate the Village
   a. Basic deck
   b. Village resources
   c. Heros
3. Create Party Deck
   a. Draw Starting Party Deck
   b. Shuffle and draw starting hand

> setup :: Int -> GameSetup -> Thunderstone [PlayerId]
> setup numberOfPlayers gameSetup = do
>     (monsters,heroes,village) <- gameSetup
>     dungeon <- dungeonDeck monsters
>     state <- getState
>     setState state {
>         thunderstoneCurrentPlayer = 0,
>         thunderstonePlayers = replicate numberOfPlayers newPlayer,
>         thunderstoneDungeon = dungeon,
>         thunderstoneHeroes = map heroStack heroes,
>         thunderstoneResources = map basicStack [minBound..maxBound]
>                              ++ map villageStack village
>         }
>     playerIds <- getPlayerIds
>     mapM_ setupPlayer playerIds
>     currentPlayerId <- getCurrentPlayerId
>     setPlayerState currentPlayerId StartingTurn
>     return playerIds
>   where
>     newPlayer = Player {
>         playerHand = [],
>         playerDeck = [],
>         playerDiscard = [],
>         playerXP = 0,
>         playerState = Waiting
>         }

Draw Starting Party Deck:
Each player draws six Militia, two Daggers, two Iron Rations, and two Torches.

Shuffle and draw the top six cards from your Party Deck to form
your starting hand.

>     setupPlayer playerId = do
>         militia <- multiple 6 $ drawResource (Basic Militia)
>         dagger <- multiple 2 $ drawResource (Basic Dagger)
>         ironRations <- multiple 2 $ drawResource (Basic IronRations)
>         torch <- multiple 2 $ drawResource (Basic Torch)
>         discard playerId (militia ++ dagger ++ ironRations ++ torch)
>         hand <- multiple 6 $ drawCard playerId
>         setHand playerId hand

There are ten cards for each class.  Take all 30 Monster cards that match
the three selected classes and shuffle them together.  This becomes the
Dungeon Deck.  Count off ten Monster cards (without revealing them) and
shuffle them together with the special Thunderstone card.  Place these
eleven cards at the bottom of the Dungeon Deck.

>     dungeonDeck :: [MonsterType] -> Thunderstone [Card]
>     dungeonDeck monsters = do
>         cards <- shuffle (map Monster (concatMap monsterCards monsters))
>         bottomOfTheDeck <- shuffle (Thunderstone : take 10 cards)
>         return (drop 10 cards ++ bottomOfTheDeck)

Hero stack:

Place both level 3 Hero cards in the stack.  Next, place all four level
2 Hero cards on top of the stack.  Finally, place all six level 1 Hero
cards on top of those.  This will create a stack of Hero cards with all
level 3 cards on the bottom, the level 2 cards in the middle, and the
level 1 cards on top.

>     heroStack :: HeroType -> (HeroType,[Card])
>     heroStack hero = (hero, replicate 6 (Hero hero (HeroLevel 1))
>                          ++ replicate 4 (Hero hero (HeroLevel 2))
>                          ++ replicate 2 (Hero hero (HeroLevel 3)))

Village stack:

There are eight of each village card.

>     villageStack :: VillageType -> (Card,Int)
>     villageStack village = (Village village,8)

Basic stack:

There are eighteen(?) of each basic card.
(Or should there be more militia?)
(The rules say there are 90 basic cards, including Disease.)

>     basicStack :: BasicType -> (Card,Int)
>     basicStack basic = (Basic basic,18)

Game state:

> type Thunderstone a = StateTransformer ThunderstoneState a

> newtype PlayerId = PlayerId Int

> data ThunderstoneState = ThunderstoneState {
>     thunderstoneStdGen :: StdGen,
>     thunderstoneCurrentPlayer :: Int,
>     thunderstonePlayers :: [Player],
>     thunderstoneDungeon :: [Card],
>     thunderstoneHeroes :: [(HeroType,[Card])],
>     thunderstoneResources :: [(Card,Int)]
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
>   | PurchaseBasic BasicType
>   | PurchaseResource VillageType
>   | LevelUpHero Card
>   | EndTurn

>   | EnterDungeon
>   | EquipHero Card Card
>   | AttackMonster DungeonRank

>   | Rest (Maybe Card)

> newtype DungeonRank = DungeonRank Int

Game mechanics

> isGameOver :: Thunderstone Bool
> isGameOver = undefined

> getScores :: Thunderstone [(PlayerId,Int)]
> getScores = undefined

> takeAction :: PlayerId -> PlayerAction -> Thunderstone Bool
> takeAction playerId playerAction = do
>     gameOver <- isGameOver
>     if gameOver
>       then return False
>       else do
>         playerState <- getPlayerState playerId
>         status <- performAction playerState playerAction
>         gameOver <- isGameOver
>         unless gameOver maybeFinishTurn
>         return status

>   where

>     performAction :: PlayerState -> PlayerAction -> Thunderstone Bool
>     performAction StartingTurn VisitVillage = do
>         setPlayerState playerId
>                        UsingVillageEffects {
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

>     endTurn = do
>         hand <- getHand playerId
>         discard playerId hand
>         handSize <- getHandSize playerId
>         hand <- multiple handSize $ drawCard playerId
>         setHand playerId hand

>     maybeFinishTurn :: Thunderstone ()
>     maybeFinishTurn = do
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

> getCurrentPlayerId :: Thunderstone PlayerId
> getCurrentPlayerId = do
>     state <- getState
>     return (PlayerId (thunderstoneCurrentPlayer state))

> getPlayerIds :: Thunderstone [PlayerId]
> getPlayerIds = do
>     state <- getState
>     return (map PlayerId [0..length (thunderstonePlayers state) - 1])

Low level game state transformations.

> shuffle :: [a] -> Thunderstone [a]
> shuffle cards = do
>     state <- getState
>     let (stdGen,shuffledCards) =
>             Shuffle.shuffle (thunderstoneStdGen state) cards
>     setState state { thunderstoneStdGen = stdGen }
>     return shuffledCards

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

> getResources :: Thunderstone [(Card,Int)]
> getResources = do
>     state <- getState
>     return (thunderstoneResources state)

More briefly: fmap thunderstoneResources getState

> getResourceCount :: Card -> Thunderstone Int
> getResourceCount card = do
>     resources <- getResources
>     maybe (return 0) return (lookup card resources)

More briefly: fmap (maybe 0 id . lookup card) getResources

> setResourceCount :: Card -> Int -> Thunderstone ()
> setResourceCount card count = do
>     state <- getState
>     let resources = thunderstoneResources state
>     setState state {
>         thunderstoneResources = map setCount (thunderstoneResources state)
>         }
>   where
>     setCount resource@(resourceCard,_)
>       | resourceCard == card = (resourceCard,count)
>       | otherwise = resource

> drawResource :: Card -> Thunderstone (Maybe Card)
> drawResource card = do
>     count <- getResourceCount card
>     if count > 0
>       then do
>         setResourceCount card (count - 1)
>         return (Just card)
>       else
>         return Nothing

> topHero :: HeroType -> Thunderstone (Maybe Card)
> topHero heroType = do
>     state <- getState
>     let maybeStack = lookup heroType (thunderstoneHeroes state)
>     return (maybe Nothing listToMaybe maybeStack)

More briefly:
fmap (maybe Nothing listToMaybe . lookup heroType . thunderstoneHeros) getState

> drawHero :: HeroType -> Thunderstone (Maybe Card)
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

> drawHeroWithLevel :: HeroType -> HeroLevel -> Thunderstone (Maybe Card)
> drawHeroWithLevel heroType heroLevel = do
>     state <- getState
>     let maybeStack = lookup heroType (thunderstoneHeroes state)
>     maybe (return Nothing) drawCard maybeStack
>   where
>     card = Hero heroType heroLevel
>     drawCard stack
>       | card `elem` stack = do
>           state <- getState
>           setState state {
>               thunderstoneHeroes =
>                   map updateHeroes (thunderstoneHeroes state)
>               }
>           return (Just card)
>       | otherwise = return Nothing
>     updateHeroes heroes@(hero,stack)
>       | hero == heroType = (hero,remove1 card stack)
>       | otherwise = heroes

> getHandSize :: PlayerId -> Thunderstone Int
> getHandSize playerId = return 6

> multiple :: (Functor m, Monad m) => Int -> m (Maybe a) -> m [a]
> multiple count action = fmap catMaybes (replicateM count action)

> remove1 :: Eq a => a -> [a] -> [a]
> remove1 item items = before ++ drop 1 after
>   where
>     (before,after) = span (/= item) items

> select :: Show a => [a] -> String -> Maybe a
> select items name =
>     maybe selectBySubstring Just (find ((== name) . show) items)
>   where
>     selectBySubstring
>       | length matchingIndices == 1 = Just (items !! head matchingIndices)
>       | otherwise = Nothing
>     matchingIndices = findIndices ((name `elem`) . subsequences . show) items
