> module Thunderstone
> where

> import Data.Maybe(catMaybes)
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

> setup :: Int -> Chooser -> Thunderstone ()
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
>     sequence_ (map setupPlayer [0..playerCount-1])
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

Militia is resource 0.
Dagger is resource 1.
IronRation is resource 2.
Torch is resource 3.

Shuffle and draw the top six cards from your Party Deck to form
your starting hand.

>     setupPlayer playerNumber = do
>         militia <- sequence (replicate 6 (drawResource 0))
>         dagger <- sequence (replicate 2 (drawResource 1))
>         ironRation <- sequence (replicate 2 (drawResource 2))
>         torch <- sequence (replicate 2 (drawResource 3))
>         cards <- shuffle (militia ++ dagger ++ ironRation ++ torch)
>         discard playerNumber cards
>         hand <- drawCards playerNumber 6
>         setHand playerNumber hand

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

>     heroStack :: HeroType -> [Card]
>     heroStack hero = replicate 6 (Hero hero (HeroLevel 1))
>                   ++ replicate 4 (Hero hero (HeroLevel 2))
>                   ++ replicate 2 (Hero hero (HeroLevel 3))

Village stack:

There are eight of each village card.

>     villageStack :: VillageType -> [Card]
>     villageStack village = replicate 8 (Village village)

Basic stack:

There are eighteen(?) of each basic card.
(Or should there be more militia?)
(The rules say there are 90 basic cards, including Disease.)

>     basicStack :: BasicType -> [Card]
>     basicStack basic = replicate 18 (Basic basic)

Game state:

> type Thunderstone a = StateTransformer ThunderstoneState a

> data ThunderstoneState = ThunderstoneState {
>     thunderstoneStdGen :: StdGen,
>     thunderstoneCurrentPlayer :: Int,
>     thunderstonePlayers :: [Player],
>     thunderstoneDungeon :: [Card],
>     thunderstoneHeroes :: [[Card]],
>     thunderstoneResources :: [[Card]]
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

> getPlayer :: Int -> Thunderstone Player
> getPlayer playerNumber = do
>     state <- getState
>     return (thunderstonePlayers state !! playerNumber)

> setPlayer :: Int -> Player -> Thunderstone ()
> setPlayer playerNumber player = do
>     state <- getState
>     let players = thunderstonePlayers state
>     setState state {
>         thunderstonePlayers = take playerNumber players
>                            ++ player : drop (playerNumber + 1) players
>         }

> getHand :: Int -> Thunderstone [Card]
> getHand playerNumber = fmap playerHand (getPlayer playerNumber)

> setHand :: Int -> [Card] -> Thunderstone ()
> setHand playerNumber cards = do
>     player <- getPlayer playerNumber
>     setPlayer playerNumber player { playerHand = cards }

> discard :: Int -> [Card] -> Thunderstone ()
> discard playerNumber cards = do
>     player <- getPlayer playerNumber
>     setPlayer playerNumber
>               player { playerDiscard = cards ++ playerDiscard player }

The discard deck is only shuffled when no cards remain in your deck and
you need to draw cards.  All cards in the discard pile are shuffled
together.

> drawCard :: Int -> Thunderstone (Maybe Card)
> drawCard playerNumber = do
>     player <- getPlayer playerNumber
>     if null (playerDeck player)
>       then
>         if null (playerDiscard player)
>           then
>             return Nothing
>           else do
>             deck <- shuffle (playerDiscard player)
>             setPlayer playerNumber
>                       player { playerDeck = deck, playerDiscard = [] }
>             drawCard playerNumber
>       else do
>         setPlayer playerNumber
>                   player { playerDeck = tail (playerDeck player) }
>         return (Just (head (playerDeck player)))

> drawCards :: Int -> Int -> Thunderstone [Card]
> drawCards playerNumber cardCount =
>     fmap catMaybes (sequence (replicate cardCount (drawCard playerNumber)))

> getResources :: Thunderstone [[Card]]
> getResources = fmap thunderstoneResources getState

> getResource :: Int -> Thunderstone [Card]
> getResource resourceNumber =
>     fmap ((!! resourceNumber) . thunderstoneResources) getState

> setResource :: Int -> [Card] -> Thunderstone ()
> setResource resourceNumber cards = do
>     state <- getState
>     let resources = thunderstoneResources state
>     setState state {
>         thunderstoneResources = take resourceNumber resources
>                              ++ cards : drop (resourceNumber + 1) resources
>         }

> drawResource :: Int -> Thunderstone Card
> drawResource resourceNumber = do
>     cards <- getResource resourceNumber
>     setResource resourceNumber (tail cards)
>     return (head cards)
