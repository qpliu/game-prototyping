> module Thunderstone
> where

> import Control.Monad(mapM_,replicateM,unless)
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

> type Chooser = Thunderstone ([MonsterType],[HeroType],[VillageType])

Cards for the first game:

> firstGame :: Chooser
> firstGame = return (monsters,heros,village)
>   where
>     monsters = [Enchanted, Ooze, UndeadDoom]
>     heros = [Elf, Lorigg, Regian, Thyrian]
>     village = [BattleFury, Fireball, FlamingSword, LightstoneGem,
>                MagicalAura, ShortSword, Spear, TownGuard]

Use the Randomizer cards to choose three random monsters, four
random heros, and eight village cards. 

For a longer game, try four or more monsters.

> randomizer :: Int -> Chooser
> randomizer monsterCount = do
>     monsters <- randomize monsterCount
>     heros <- randomize 4
>     village <- randomize 8
>     return (monsters,heros,village)
>   where
>     randomize count = do
>         shuffled <- shuffle [minBound..maxBound]
>         return (take count shuffled)


Setup:
1. Populate the Dungeon
2. Populate the Village
   a. Basic deck
   b. Village resources
   c. Heros
3. Create Party Deck
   a. Draw Starting Party Deck
   b. Shuffle and draw starting hand

> setup :: Int -> Chooser -> Thunderstone [PlayerId]
> setup playerCount chooser = do
>     (monsters,heroes,village) <- chooser
>     dungeon <- dungeonDeck monsters
>     state <- getState
>     setState state {
>         thunderstoneCurrentPlayer = 0,
>         thunderstonePlayers = replicate playerCount newPlayer,
>         thunderstoneDungeon = dungeon,
>         thunderstoneHeroes = map heroStack heroes,
>         thunderstoneResources = map basicStack [minBound..maxBound]
>                              ++ map villageStack village
>         }
>     mapM_ (setupPlayer . PlayerId) [0..playerCount-1]
>     return (map PlayerId [0..playerCount-1])
>   where
>     newPlayer = Player {
>         playerHand = [],
>         playerDeck = [],
>         playerDiscard = [],
>         playerXP = 0,
>         playerActions = []
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
>     playerActions :: [PlayerAction]
>     }

PlayerActions are actions the player needs to take before the turn ends.

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
>     StartTurn

>   | UseVillageEffects
>   | Purchase Int Int
>   | LevelUpHeros

>   | UseDungeonEffects
>   | AttackMonster

>   | DestroyCard

>   | EndTurn

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
>       | hero == heroType = (hero,updateStack stack)
>       | otherwise = heroes
>     updateStack stack =
>         let (top,bottom) = span (/= card) stack
>         in  top ++ drop 1 bottom

> multiple :: (Functor m, Monad m) => Int -> m (Maybe a) -> m [a]
> multiple count action = fmap catMaybes (replicateM count action)
