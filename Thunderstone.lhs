> module Thunderstone
> where

> import Control.Monad(foldM,mapM,mapM_,replicateM,unless,when)
> import Data.Either(lefts,rights)
> import Data.List(find,findIndices,nub,subsequences,(\\))
> import Data.Maybe(catMaybes,listToMaybe)
> import System.Random(StdGen)

> import qualified Shuffle
> import StateTransformer(StateTransformer,getState,setState)

Data type that represents all the Thunderstone playing cards.

> data Card =
>     Dungeon DungeonCard
>   | Basic BasicType
>   | Hero HeroCard
>   | Village VillageType
>   | Disease DiseaseType
>   deriving (Eq,Show)

Data types that represent card types, as well as the Randomizer cards.

> data ThunderstoneType =
>     StoneOfMystery

Wrath expansion:

>   | StoneOfAgony

Doomgate expansion:

>   | StoneOfAvarice

Dragonspire expansion:

>   deriving (Eq,Show)

> data MonsterType =
>     Abyssal
>   | DoomknightHumanoid
>   | Dragon
>   | Enchanted
>   | Humanoid
>   | Ooze
>   | UndeadDoom
>   | UndeadSpirit

Wrath expansion:

>   | ElementalNature
>   | ElementalPain
>   | Golem
>   | Horde

Doomgate expansion:

>   | AbyssalThunderspawn
>   | CultistHumanoid
>   | EvilDruidHumanoid
>   | TheSwarm
>   | UndeadStormwraith

Dragonspire expansion:

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

Promotional:

>   | Clan
>   | Harruli

Wrath expansion:

>   | Blind
>   | Diin
>   | Divine
>   | Gangland
>   | Gohlen
>   | Runespawn
>   | Toryn

Doomgate expansion:

>   | Deep
>   | Drunari
>   | Sidhe
>   | Slynn
>   | Tempest
>   | Tholis
>   | Verdan

Dragonspire expansion:

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

Wrath expansion:

>   | Ambrosia
>   | AmuletOfPower
>   | Blacksmith
>   | Claymore
>   | CreepingDeath
>   | CursedMace
>   | ForesightElixir
>   | IllusoryBlade
>   | MagiStaff
>   | MagicMissile
>   | Sage
>   | ShortBow
>   | TaxCollector

Doomgate expansion:

>   | BlessedHammer
>   | BorderGuard
>   | Cyclone
>   | DivineStaff
>   | DoomgateSquire
>   | FlaskOfOil
>   | FortuneTeller
>   | Glowberries
>   | GreedBlade
>   | PiousChaplain
>   | SoulJar
>   | SpiritBlast
>   | SpiritHunter

Dragonspire expansion:

>   deriving (Bounded,Enum,Eq,Show)

> data DiseaseType =
>     DungeonRot

Doomgate expansion:

>   | BalefulPlague
>   | Fatigue
>   | Leprosy
>   | Malaise
>   | ThundersCurse

>   deriving (Bounded,Enum,Eq,Show)

> data DungeonFeatureType =
>     PickTwo
>   | Guardian

Wrath expansion:

>   | DeathTraps
>   | DireTraps

Doomgate expansion:

>   | AmuletTreasures
>   | UlbricksTreasures
>   deriving (Eq,Show)

Dragonspire expansion:


Hero cards have three levels.

> data HeroCard = HeroCard HeroType HeroLevel
>   deriving (Eq,Show)

> newtype HeroLevel = HeroLevel Int
>   deriving (Eq,Show)

> data HeroParameters = HeroParameters {
>     heroName :: String,
>     heroGoldValue :: Int,
>     heroStrength :: Int,
>     heroPurchaseCost :: Int,
>     heroClass :: [HeroClass],
>     heroLight :: Int,
>     heroXPCost :: Int,
>     heroVictoryPoints :: Int
>     }

> data HeroClass =
>     Archer
>   | Cleric
>   | Fighter
>   | Thief
>   | Wizard
>   deriving (Eq,Show)

> heroParameters :: HeroCard -> HeroParameters

> heroParameters (HeroCard Amazon (HeroLevel 1)) = HeroParameters {
>     heroName = "Amazon Archer",
>     heroGoldValue = 0,
>     heroStrength = 4,
>     heroPurchaseCost = 6,
>     heroClass = [Fighter, Archer],
>     heroLight = 0,
>     heroXPCost = 2,
>     heroVictoryPoints = 0
>     }

Amazon Archer effects:
  ATTACK +1
  DUNGEON: ATTACK +2 at Rank 2 or 3
    This Hero's Dungeon Effect is an Attack Bonus in addition to the
    Amazon's normal Attack.

> heroParameters (HeroCard Amazon (HeroLevel 2)) = HeroParameters {
>     heroName = "Amazon Huntress",
>     heroGoldValue = 0,
>     heroStrength = 5,
>     heroPurchaseCost = 9,
>     heroClass = [Fighter, Archer],
>     heroLight = 0,
>     heroXPCost = 3,
>     heroVictoryPoints = 0
>     }

Amazon Huntress effects:
  ATTACK +2
  DUNGEON: ATTACK +3 at Rank 2 or 3
    This Hero's Dungeon Effect is an Attack Bonus in addition to the
    Amazon's normal Attack.

> heroParameters (HeroCard Amazon (HeroLevel 3)) = HeroParameters {
>     heroName = "Amazon Queen",
>     heroGoldValue = 0,
>     heroStrength = 6,
>     heroPurchaseCost = 11,
>     heroClass = [Fighter, Archer],
>     heroLight = 0,
>     heroXPCost = 0,
>     heroVictoryPoints = 0
>     }

Amazon Queen effects:
  ATTACK +2
  DUNGEON: ATTACK +4 at Rank 2 or 3
    This Hero's Dungeon Effect is an Attack Bonus in addition to the
    Amazon's normal Attack.

The specific dungeon cards.

> data DungeonCard =

>     Thunderstone ThunderstoneType

>   | ArchdukeOfPain
>   | Grudgebeast
>   | Succcubus
>   | TheUnchained
>   | Tormentor

>   | Darkness
>   | Judgement
>   | Knightmare
>   | LordMortis
>   | ThePrince

>   | EbonFume
>   | UyrilUnending
>   | Mythlurian
>   | TyxrTheOld
>   | Skaladak

>   | BlinkDog
>   | Griffon
>   | Nixie
>   | Pegasus
>   | Sphinx

>   | BloodskullOrc
>   | DeadboneTroll
>   | FirebrandCyclops
>   | GrayskinLizard
>   | GriknackGoblin

>   | BlackSlime
>   | GrayOooze
>   | GreenBlob
>   | NoxiousSlag
>   | RedJelly

>   | Famine
>   | Harbinger
>   | LordOfDeath
>   | Suffering

>   | Ghost
>   | Haunt
>   | Revenant
>   | Spectre
>   | Wraith

Wrath expansion:

>   | AirWrath
>   | EarthWrath
>   | FireWrath
>   | WaterWrath

>   | BloodTorment
>   | LavaTorment
>   | ShadowTorment
>   | SmokeTorment
>   | SteamTorment

>   | BronzeGolem
>   | ClayGolem
>   | Colossus
>   | IronGolem
>   | StoneGolem

>   | HordeHumanoid Int

>   | DarkChampion

Doomgate expansion:

>   | Swarm Int

>   | UnholyGuardian

Dragonspire expansion:

>   deriving (Eq,Show)

> monsterCards :: MonsterType -> [DungeonCard]
> monsterCards Abyssal = []
> monsterCards DoomknightHumanoid = []
> monsterCards Dragon = []
> monsterCards Enchanted = []
> monsterCards Humanoid = []
> monsterCards Ooze = []
> monsterCards UndeadDoom = []
> monsterCards UndeadSpirit = []

> dungeonFeatureCards :: DungeonFeatureType -> [DungeonCard]
> dungeonFeatureCards = undefined

Game setup:

For the first game, a specific set of cards is chosen.
Otherwise, the Randomizer cards are used to select a random set of cards.

> type GameSetup = Thunderstone ([DungeonCard],[HeroType],[VillageType])

Cards for the first game:

> firstGame :: GameSetup
> firstGame = do
>     dungeon <- dungeonSetup 1 thunderstones 3 monsters
>                             0 dungeonFeatures guardians
>     return (dungeon,heroes,village)
>   where
>     thunderstones = [StoneOfMystery]
>     monsters = [Enchanted, Ooze, UndeadDoom]
>     heroes = [Elf, Lorigg, Regian, Thyrian]
>     village = [BattleFury, Fireball, FlamingSword, LightstoneGem,
>                MagicalAura, ShortSword, Spear, TownGuard]
>     dungeonFeatures = []
>     guardians = []

Use the Randomizer cards to choose three random monsters, four
random heros, and eight village cards. 

For a longer game, try four or more monsters.

> basicGame :: GameSetup
> basicGame = do
>     dungeon <- dungeonSetup 1 thunderstones 3 monsters
>                             0 dungeonFeatures guardians
>     heroTypes <- shuffle heroes
>     villageTypes <- shuffle village
>     return (dungeon,take 4 heroTypes,take 8 villageTypes)
>   where
>     thunderstones = [StoneOfMystery]
>     monsters = [Abyssal .. UndeadSpirit]
>     heroes = [Amazon .. Thyrian]
>     village = [ArcaneEnergies .. Warhammer]
>     dungeonFeatures = []
>     guardians = []

Cards for the first Wrath of the Elements game:

> firstWrathGame :: GameSetup
> firstWrathGame = do
>     dungeon <- dungeonSetup 1 thunderstones 3 monsters
>                             2 dungeonFeatures guardians
>     return (dungeon,heroes,village)
>   where
>     thunderstones = [StoneOfAgony]
>     monsters = [Horde, ElementalNature, UndeadDoom]
>     heroes = [Chalice, Feayn, Diin, Toryn]
>     village = [CursedMace, ForesightElixir, Lantern, MagiStaff, MagicMissile,
>                Sage, ShortBow, TownGuard]
>     dungeonFeatures = [DireTraps, Guardian]
>     guardians = [DarkChampion]

Wrath of Elements plus basic set:

> wrathGame :: GameSetup
> wrathGame = do
>     dungeon <- dungeonSetup 1 thunderstones 3 monsters
>                             0 dungeonFeatures guardians
>     heroTypes <- shuffle heroes
>     villageTypes <- shuffle village
>     return (dungeon,take 4 heroTypes,take 8 villageTypes)
>   where
>     thunderstones = [StoneOfMystery,StoneOfAgony]
>     monsters = [Abyssal .. UndeadSpirit] ++ [ElementalNature .. Horde]
>     heroes = [Amazon .. Thyrian] ++ [Blind .. Toryn]
>     village = [ArcaneEnergies .. Warhammer] ++ [Ambrosia .. TaxCollector]
>     dungeonFeatures = [PickTwo, Guardian, Guardian, DeathTraps, DireTraps]
>     guardians = [DarkChampion]

Cards for the first Doomgate Legion game:

> firstDoomgateGame :: GameSetup
> firstDoomgateGame = do
>     dungeon <- dungeonSetup 1 thunderstones 3 monsters
>                             2 dungeonFeatures guardians
>     return (dungeon,heroes,village)
>   where
>     thunderstones = [StoneOfAvarice]
>     monsters = [AbyssalThunderspawn, CultistHumanoid, EvilDruidHumanoid]
>     heroes = [Chalice, Feayn, Diin, Toryn]
>     village = [CursedMace, ForesightElixir, Lantern, MagiStaff, MagicMissile,
>                Sage, ShortBow, TownGuard]
>     dungeonFeatures = [AmuletTreasures, Guardian]
>     guardians = [UnholyGuardian]

Doomgate Legion plus basic set:

> doomgateGame :: GameSetup
> doomgateGame = undefined

Cards for the first Dragonspire game:

> firstDragonspireGame :: GameSetup
> firstDragonspireGame = undefined

Dragonspire plus basic set:

> dragonspireGame :: GameSetup
> dragonspireGame = undefined

There are ten cards for each class.  Take all 30 Monster cards that match
the three selected classes and shuffle them together.  This becomes the
Dungeon Deck.  Count off ten Monster cards (without revealing them) and
shuffle them together with the special Thunderstone card.  Place these
eleven cards at the bottom of the Dungeon Deck.

A few cards sow a large question mark and say "Dungeon Feature" at the
top.  These will add special features like traps to the dungeon, and are
mixed in with the Monster Randomizers.

Turn over cards from the stack of Monster Randomizers one at a time until
you turn over a total of three different Monster cards, plus any number
of Dungeon Feature Randomizers.

If you draw any Dungeon Feature Randomizers, shuffle the "Dungeon Feature"
cards.  Take one card from the Dungeon Feature pile for each special
Randomizer you drew.

> dungeonSetup :: Int -> [ThunderstoneType] -> Int -> [MonsterType]
>              -> Int -> [DungeonFeatureType] -> [DungeonCard]
>              -> Thunderstone [DungeonCard]
> dungeonSetup numberOfThunderstones thunderstones
>              numberOfMonsterTypes monsterTypes
>              minimumNumberOfDungeonFeatures dungeonFeatureTypes
>              guardians = do
>     randomizers <- shuffle (map Left monsterTypes
>                             ++ map Right dungeonFeatureTypes)
>     let drawnRandomizers =
>             head $ (++ [randomizers]) -- if bad input, use everything
>                  $ dropWhile insufficient
>                  $ map (flip drop randomizers)
>                        [numberOfMonsterTypes .. length randomizers]
>     let dungeonFeatures =
>             concatMap dungeonFeatureCards (rights drawnRandomizers)
>     let monsters = concatMap monsterCards (lefts drawnRandomizers)
>     deck <- shuffle (dungeonFeatures ++ monsters)
>     let deckWithProgressiveMonsters = setProgressiveMonsters deck
>     let numberOfGuardians =
>             length (filter (== Guardian) (rights drawnRandomizers))
>     deckWithGuardians <- foldM shuffleInGuardian deckWithProgressiveMonsters
>                                (take numberOfGuardians guardians)
>     shuffledThunderstones <- shuffle thunderstones
>     shuffleToBottom 10 deckWithGuardians
>         (take numberOfThunderstones (map Thunderstone shuffledThunderstones))
>   where
>     insufficient cards =
>       length (lefts cards) < numberOfMonsterTypes
>       || length (rights cards) < minimumNumberOfDungeonFeatures
>                                  + length (filter (== PickTwo)
>                                                   (rights cards))
>     shuffleInGuardian deck guardian =
>         shuffleToBottom 10 deck [guardian]
>     setProgressiveMonsters deck =
>         setProgressiveCards (map progressTheHorde [3..12])
>       $ setProgressiveCards (map progressTheSwarm [4..13]) deck
>     progressTheHorde n (HordeHumanoid _) =
>         Just (HordeHumanoid n)
>     progressTheHorde _ _ = Nothing
>     progressTheSwarm n (Swarm _) = Just (Swarm n)
>     progressTheSwarm _ _ = Nothing

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
>     (dungeonCards,heroes,village) <- gameSetup
>     state <- getState
>     setState state {
>         thunderstoneCurrentPlayer = 0,
>         thunderstonePlayers = replicate numberOfPlayers newPlayer,
>         thunderstoneDungeon = map Dungeon dungeonCards,
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

Hero stack:

Place both level 3 Hero cards in the stack.  Next, place all four level
2 Hero cards on top of the stack.  Finally, place all six level 1 Hero
cards on top of those.  This will create a stack of Hero cards with all
level 3 cards on the bottom, the level 2 cards in the middle, and the
level 1 cards on top.

>     heroStack :: HeroType -> (HeroType,[HeroCard])
>     heroStack hero = (hero, replicate 6 (HeroCard hero (HeroLevel 1))
>                          ++ replicate 4 (HeroCard hero (HeroLevel 2))
>                          ++ replicate 2 (HeroCard hero (HeroLevel 3)))

Village stack:

There are eight of each village card.

>     villageStack :: VillageType -> (Card,Int)
>     villageStack village = (Village village,8)

Basic stack:

The base game has 90 basic cards, including disease cards.

30 Militia, 15 each of Torch, Dagger, Iron Rations, and disease.

The Wrath expansion has 30 replacement Militia, and 12 special disease cards.

>     basicStack :: BasicType -> (Card,Int)
>     basicStack Militia = (Basic Militia,30)
>     basicStack basic = (Basic basic,15)

Game state:

> type Thunderstone a = StateTransformer ThunderstoneState a

> newtype PlayerId = PlayerId Int

> data ThunderstoneState = ThunderstoneState {
>     thunderstoneStdGen :: StdGen,
>     thunderstoneCurrentPlayer :: Int,
>     thunderstonePlayers :: [Player],
>     thunderstoneDungeon :: [Card],
>     thunderstoneHeroes :: [(HeroType,[HeroCard])],
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

>     endTurn = do
>         hand <- getHand playerId
>         discard playerId hand
>         handSize <- getHandSize playerId
>         hand <- multiple handSize $ drawCard playerId
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
> shuffle deck = do
>     state <- getState
>     let (stdGen,shuffled) = Shuffle.shuffle (thunderstoneStdGen state) deck
>     setState state { thunderstoneStdGen = stdGen }
>     return shuffled

> shuffleToBottom :: Int -> [a] -> [a] -> Thunderstone [a]
> shuffleToBottom count cards deck = do
>     bottom <- shuffle (cards ++ take count deck)
>     return (drop count deck ++ bottom)

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
>     return (maybe 0 id (lookup card resources))

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

> drawHeroWithLevel :: HeroType -> HeroLevel -> Thunderstone (Maybe HeroCard)
> drawHeroWithLevel heroType heroLevel = do
>     state <- getState
>     let maybeStack = lookup heroType (thunderstoneHeroes state)
>     maybe (return Nothing) drawCard maybeStack
>   where
>     card = HeroCard heroType heroLevel
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
>     maybe (selectBySubstring matchingIndices)
>           Just (find ((== name) . show) items)
>   where
>     selectBySubstring [index] = Just (items !! index)
>     selectBySubstring _ = Nothing
>     matchingIndices = findIndices ((name `elem`) . subsequences . show) items

For the Horde and the Swarm: add progressive stats to the cards
once they are shuffled into the dungeon deck.

> setProgressiveCards :: [a -> Maybe a] -> [a] -> [a]
> setProgressiveCards setStats deck =
>     reverse $ fst $ foldl updateCard ([],setStats) deck
>   where
>     updateCard (cards,[]) card = (card:cards,[])
>     updateCard (cards,setStats@(setStat:remainingSetStats)) card =
>         case setStat card of
>             Just newCard -> (newCard:cards,remainingSetStats)
>             Nothing -> (card:cards,setStats)
