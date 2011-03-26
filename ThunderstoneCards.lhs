The cards in Thunderstone, the Wrath of the Elements expansion, the
Doomgate Legion expansion, and Dragonspire, plus some promotional cards.

> module ThunderstoneCards
> where

> data Source =
>     ThunderstoneBase
>   | WrathOfTheElements
>   | DoomgateLegion
>   | Dragonspire
>   | Promotional
>   deriving (Bounded,Enum,Eq,Show)


Card types
===============================================================================
The card types are group cards and are the randomizers.

> data HeroType =
>     HeroMilitia

Base set (11):

>   | HeroAmazon
>   | HeroChalice
>   | HeroDwarf
>   | HeroElf
>   | HeroFeayn
>   | HeroLorigg
>   | HeroOutlands
>   | HeroRedblade
>   | HeroRegian
>   | HeroSelurin
>   | HeroThyrian

Wrath of the Elements (7):

>   | HeroBlind
>   | HeroDiin
>   | HeroDivine
>   | HeroGangland
>   | HeroGohlen
>   | HeroRunespawn
>   | HeroToryn

Doomgate Legion (7):

>   | HeroDeep
>   | HeroDrunari
>   | HeroSidhe
>   | HeroSlynn
>   | HeroTempest
>   | HeroTholis
>   | HeroVerdan

Dragonspire (11):

>   | HeroPhalanx
>   | HeroBelzur
>   | HeroCabal
>   | HeroChulian
>   | HeroEvoker
>   | HeroFlame
>   | HeroGorinth
>   | HeroHalfOrc
>   | HeroStoneguard
>   | HeroTerakian
>   | HeroVeteran

Promotional:

>   | HeroClan
>   | HeroHarruli

>   deriving (Bounded,Enum,Eq,Show)

> data MonsterType =

Base set (8):

>     MonsterAbyssal
>   | MonsterDoomknightHumanoid
>   | MonsterDragon
>   | MonsterEnchanted
>   | MonsterHumanoid
>   | MonsterOoze
>   | MonsterUndeadDoom
>   | MonsterUndeadSpirit

Wrath of the Elements (4):

>   | MonsterElementalNature
>   | MonsterElementalPain
>   | MonsterGolem
>   | MonsterHordeHumanoid

Doomgate Legion (5):

>   | MonsterAbyssalThunderspawn
>   | MonsterCultistHumanoid
>   | MonsterEvilDruid
>   | MonsterTheSwarmAnimal
>   | MonsterUndeadStormwraith

Dragonspire (8):

>   | MonsterBanditHumanoid
>   | MonsterDarkEnchanted
>   | MonsterElementalFire
>   | MonsterGiant
>   | MonsterHydraDragon
>   | MonsterOrcHumanoid
>   | MonsterUndeadLich
>   | MonsterUndeadPlague

Promotional:

>   deriving (Bounded,Enum,Eq,Show)

> data DungeonFeatureType =

>     DungeonFeaturePickTwo

Wrath of the Elements:

>   | DungeonFeatureGuardian
>   | DungeonFeatureTrapDire
>   | DungeonFeatureTrapDeath

Doomgate Legion:

>   | DungeonFeatureAmuletTreatures
>   | DungeonFeatureUlbricksTreasures

Dragonspire:

>   | DungeonFeatureFigurineTreasure
>   | DungeonFeatureSetting
>   | DungeonFeatureTrapDraconic

Promotional:

>   deriving (Bounded,Enum,Eq,Show)



Cards
===============================================================================

> data ThunderstoneCard =

Base set (1):

>     StoneOfMystery

Wrath of the Elements (1):

>   | StoneOfAgony

Doomgate Legion (1):

>   | StoneOfAvarice

Dragonspire (2):

>   | StoneOfTerror
>   | StoneOfScorn

>   deriving (Bounded,Enum,Eq,Show)

> data GuardianCard =

Wrath of the Elements (1):

>     DarkChampion

Doomgate Legion (1):

>   | UnholyGuardian

Dragonspire (2):

>   | GuardianOfNight
>   | GuardianOfTorment

Promotional:

>   | DeathSentinel

>   deriving (Bounded,Enum,Eq,Show)

> data HeroCard =
>     Militia

Base set:

>   | AmazonArcher
>   | AmazonHuntress
>   | AmazonQueen
>   | ChaliceQuester
>   | ChaliceDefender
>   | ChalicePaladin
>   | DwarfGuardian
>   | DwarfJanissary
>   | DwarfSentinel
>   | ElfWizard
>   | ElfSorcerer
>   | ElfArchmage
>   | FeaynArcher
>   | FeaynMarksman
>   | FeaynSniper
>   | LoriggThief
>   | LoriggRogue
>   | LoriggOutlaw
>   | OutlandsWarrior
>   | OutlandsSlayer
>   | OutlandsKhan
>   | RedbladeKiller
>   | RedbladePoisoner
>   | RedbladeAssassin
>   | RegianCleric
>   | RegianPriest
>   | RegianBishop
>   | SelurinMagician
>   | SelurinWarlock
>   | SelurinTheurge
>   | ThyrianSquire
>   | ThyrianKnight
>   | ThyrianLord

Wrath of the Elements:

>   | BlindNeophyte
>   | BlindMonk
>   | BlindGrandmaster
>   | DiinIllusionist
>   | DiinBeguiler
>   | DiinEnchanter
>   | DivineHealer
>   | DivineChaplain
>   | DivineProphet
>   | GanglandThug
>   | GanglandHeavy
>   | GanglandCrook
>   | GohlenTrapper
>   | GohlenTracker
>   | GohlenHunter
>   | RunespawnAdept
>   | RunespawnSiren
>   | RunespawnWitch
>   | TorynScrapper
>   | TorynDuelist
>   | TorynGladiator

Doomgate Legion:

>   | DeepMiner
>   | DeepDigger
>   | DeepWrecker
>   | DrunariOrphan
>   | DrunariVagabond
>   | DrunariGypsy
>   | SidheNatural
>   | SidheDruid
>   | SidheSpirit
>   | SlynnBowman
>   | SlynnBowmaster
>   | SlynnLongbowman
>   | TempestAvenger
>   | TempestReaver
>   | TempestWarden
>   | TholisMedium
>   | TholisClairvoyant
>   | TholisOracle
>   | VerdanMinstrel
>   | VerdanBard
>   | VerdanTrouadour

Dragonspire:

>   | PhalanxFootman
>   | PhalanxOfficer
>   | BelzurCurate
>   | BelzurBishop
>   | BelzurCardinal
>   | CabalAstrologer
>   | CabalSage
>   | CabalMaster
>   | ChulianRat
>   | ChulianScavenger
>   | ChulianLooter
>   | EvokerAdept
>   | EvokerScorcher
>   | EvokerPyroclast
>   | FlameWatch
>   | FlameGuard
>   | FlameHero
>   | GorinthAmateur
>   | GorinthHoarder
>   | GorinthMiser
>   | HalfOrcRaider
>   | HalfOrcMarauder
>   | HalfOrcDervish
>   | StoneguardBrute
>   | StoneguardBruiser
>   | StoneguardTanker
>   | TerakianDefender
>   | TerakianPeer
>   | TerakianTemplar
>   | VeteranWarrior
>   | VeteranBerserker
>   | VeteranReaver
>   | VeteranWarmonger

Promotional:

>   | ClanSergeant
>   | ClanCommander
>   | ClanChampion
>   | HarruliInitiate
>   | HarruliSpellsword
>   | HarruliAvatar

>   deriving (Bounded,Enum,Eq,Show)

> data VillageCard =
>     Dagger
>   | IronRations
>   | Torch

Base set (19):

>   | ArcaneEnergies
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

Wrath of the Elements (14):

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
>   | Shortbow
>   | TavernBrawl
>   | TaxCollector

Doomgate Legion (13):

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

Dragonspire (18):

>   | BluefireStaff
>   | BurntOffering
>   | ChieftainsDrum
>   | FrostBolt
>   | FrostGiantAxe
>   | GuardianBlade
>   | Guide
>   | Polymorph
>   | Quartermaster
>   | RecurveBow
>   | Scout
>   | Silverstorm
>   | Skullbreaker
>   | SoulGem
>   | SpoiledFood
>   | ThunderRing
>   | TorynGuantlet
>   | Trader

>   deriving (Bounded,Enum,Eq,Show)

> data MonsterCard =

Base set:

>     ArchdukeOfPain
>   | Grudgebeast
>   | Succubus
>   | Tormentor
>   | TheUnchained

>   | Darkness
>   | Judgement
>   | Knightmare
>   | LordMortis
>   | ThePrince

>   | EbonFume
>   | Mythlurian
>   | Skaladak
>   | TyxrTheOld
>   | UyrilUnending

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
>   | GrayOoze
>   | GreenBlob
>   | NoxiousSlag
>   | RedJelly

>   | Famine
>   | Harbinger
>   | Kingdom
>   | LordOfDeath
>   | Suffering

>   | Ghost
>   | Haunt
>   | Revenant
>   | Spectre
>   | Wraith

Wrath of the Elements:

>   | AirWrath
>   | EarthWrath
>   | FireWrath
>   | ThunderWrath
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

>   | HordePlaceholder
>   | Horde3
>   | Horde4
>   | Horde5
>   | Horde6
>   | Horde7
>   | Horde8
>   | Horde9
>   | Horde10
>   | Horde11
>   | Horde12

Doomgate Legion:

>   | TheBloodless
>   | Razorback
>   | Regicide
>   | TendrilMinion
>   | Usurper

>   | TheAuthority
>   | TheCleansed
>   | TheDevout
>   | TheFaithful
>   | TheVoice

>   | Arachnea
>   | CrowTalker
>   | GaiasCurse
>   | MonarchDruid
>   | NaturesMistress

>   | SwarmPlaceholder
>   | Swarm4
>   | Swarm5
>   | Swarm6
>   | Swarm7
>   | Swarm8
>   | Swarm9
>   | Swarm10
>   | Swarm11
>   | Swarm12
>   | Swarm13

>   | Deathchill
>   | Hellstorm
>   | LightningsGaze
>   | MurderWind
>   | Rage

Dragonspire:

>   | Assassin
>   | Cutthroat
>   | Highwayman
>   | Stalker
>   | Thug

>   | Basilisk
>   | Harpy
>   | Manticore
>   | Medusa
>   | Minotaur

>   | Blaze
>   | ChokingSmoke
>   | Ember
>   | Flare
>   | Inferno

>   | FireGiant
>   | FrostGiant
>   | MountainGiant
>   | StoneGiant
>   | Titan

>   | EarthTempest
>   | FlameRage
>   | Hydra
>   | WaterWrathHydra
>   | WindFury

>   | HalfOgre
>   | OrcBlademaster
>   | OrcWarlord
>   | ShadowKiller
>   | StandardBearer

>   | Deathbringer
>   | Destiny
>   | GraveKnight
>   | LichLord
>   | TombHaunt

>   | HungryDead
>   | Plaguebearer
>   | PlagueZombie
>   | RestlessCorpse
>   | WalkingScourge

Promotional:

>   | TheVision
>   | Mammoth

>   deriving (Bounded,Enum,Eq,Show)

> data DungeonFeatureCard =

Wrath of the Elements:

>     TheCage
>   | DeliriumRoom
>   | PoisonGasTrap

>   | PitTrap
>   | RollingBoulder

Doomgate Legion:

>   | GreedAmulet
>   | NaturesAmulet
>   | StrengthAmulet

>   | UlbricksArmor
>   | UlbricksGauntlets
>   | UlbricksHelmet

Dragonspire:

>   | DragonsClaws
>   | DragonsJaw
>   | DragonsTeeth
>   | DragonsWords

>   | EmeraldDragon
>   | IvoryDragon
>   | RubyDragon
>   | SapphireDragon

Promotional:

>   | BladeTrap

>   deriving (Bounded,Enum,Eq,Show)

> data DiseaseCard =

Base set:

>     Disease

Doomgate Legion:

>   | BalefulPlague
>   | Fatigue
>   | Leprosy
>   | Malaise
>   | ThundersCurse

>   deriving (Bounded,Enum,Eq,Show)

> data SettingCard =
>     SettingBarrowsdale
>   | SettingDoomgate
>   | SettingDragonspire
>   | SettingDreadwatch
>   | SettingFeaynSwamp
>   | SettingGrimhold
>   | SettingRegianCove
>   deriving (Bounded,Enum,Eq,Show)

> data CardClass =
>     ClassAbyssal
>   | ClassArcher
>   | ClassBlack
>   | ClassBlue
>   | ClassBlunt
>   | ClassCleric
>   | ClassDoom
>   | ClassDoomknight
>   | ClassDisease
>   | ClassDragon
>   | ClassEdged
>   | ClassEnchanted
>   | ClassFighter
>   | ClassFood
>   | ClassGreen
>   | ClassGuardian
>   | ClassHero
>   | ClassHumanoid
>   | ClassItem
>   | ClassLight
>   | ClassMagic
>   | ClassMilitia
>   | ClassOoze
>   | ClassRed
>   | ClassSpell
>   | ClassSpirit
>   | ClassThief
>   | ClassThunderstone
>   | ClassUndead
>   | ClassVillager
>   | ClassWeapon
>   | ClassWhite
>   | ClassWizard
>   deriving (Eq,Show)

Card details
===============================================================================

> data CardDetails cardType = CardDetails {
>     cardName :: String,
>     cardSource :: Source,
>     cardType :: cardType,
>     cardIcon :: CardIcon,
>     cardCount :: Int,
>     cardClasses :: [CardClass],
>     cardGold :: Maybe Int,
>     cardLight :: Maybe Int,
>     cardVictoryPoints :: Maybe Int,
>     cardStrength :: Maybe Int,
>     cardPrice :: Maybe Int,
>     cardXP :: Maybe Int,
>     cardHealth :: Maybe Int,
>     cardWeight :: Maybe Int,
>     cardLevelUp :: Maybe Int,
>     cardText :: [String],
>     cardGlossary :: [String]
>     }

> data CardIcon =
>     CardIconBasic
>   | CardIconHero Int
>   | CardIconMonster
>   | CardIconVillage
>   deriving (Eq,Show)

> initCardDetails :: CardDetails cardType
> initCardDetails = CardDetails {
>     cardName = undefined,
>     cardSource = undefined,
>     cardType = undefined,
>     cardIcon = undefined,
>     cardCount = undefined,
>     cardClasses = undefined,
>     cardGold = Nothing,
>     cardLight = Nothing,
>     cardVictoryPoints = Nothing,
>     cardStrength = Nothing,
>     cardPrice = Nothing,
>     cardXP = Nothing,
>     cardHealth = Nothing,
>     cardWeight = Nothing,
>     cardLevelUp = Nothing,
>     cardText = [],
>     cardGlossary = []
>     }

Thunderstone cards:

> thunderstoneDetails :: ThunderstoneCard -> CardDetails ThunderstoneCard

> thunderstoneDetails StoneOfMystery = initCardDetails {
>     cardName = "Stone of Mystery",
>     cardSource = ThunderstoneBase,
>     cardType = StoneOfMystery,
>     cardIcon = CardIconBasic,
>     cardCount = 1,
>     cardClasses = [ClassThunderstone],
>     cardVictoryPoints = Just 3,
>     cardText = ["...and the thunder of the wind shouts back."],
>     cardGlossary =
>        ["Stone of Mystery: This is the only Thunderstone card "
>         ++ "in the basic set.  Some expansions will include "
>         ++ "other Stones, each with its own powers.  You are "
>         ++ "welcome to use any Thunderstone card during setup."]
>     }

> thunderstoneDetails StoneOfAgony =
>     undefinedCardDetails StoneOfAgony WrathOfTheElements
> thunderstoneDetails StoneOfAvarice =
>     undefinedCardDetails StoneOfAvarice DoomgateLegion
> thunderstoneDetails StoneOfTerror =
>     undefinedCardDetails StoneOfTerror Dragonspire
> thunderstoneDetails StoneOfScorn =
>     undefinedCardDetails StoneOfScorn Dragonspire

Hero cards:

> heroDetails :: HeroCard -> CardDetails HeroType

> heroDetails Militia = initCardDetails {
>     cardName = "Militia",
>     cardSource = ThunderstoneBase,
>     cardType = HeroMilitia,
>     cardIcon = CardIconBasic,
>     cardCount = 30,
>     cardClasses = [ClassMilitia,ClassHero],
>     cardGold = Just 0,
>     cardStrength = Just 2,
>     cardPrice = Just 0,
>     cardLevelUp = Just 3,
>     cardText = ["ATTACK +1"],
>     cardGlossary =
>        ["Militia: Militia are considered Heroes for all "
>         ++ "purposes.  Militia have a gold value of 0.  "
>         ++ "This is a asic card included in every game."]
>     }

> heroDetails AmazonArcher = initCardDetails {
>     cardName = "Amazon Archer",
>     cardSource = ThunderstoneBase,
>     cardType = HeroAmazon,
>     cardIcon = CardIconHero 1,
>     cardCount = 6,
>     cardClasses = [ClassFighter,ClassArcher],
>     cardStrength = Just 4,
>     cardPrice = Just 6,
>     cardLevelUp = Just 2,
>     cardText = ["ATTACK +1","Additional ATTACK +2 at Rank 2 or 3."],
>     cardGlossary =
>         ["Amazon: This Hero's Dungeon Effect is an Attack that "
>          ++ "is in addition to the Amazon's normal Attack."]
>     }

> heroDetails AmazonHuntress = initCardDetails {
>     cardName = "Amazon Huntress",
>     cardSource = ThunderstoneBase,
>     cardType = HeroAmazon,
>     cardIcon = CardIconHero 2,
>     cardCount = 4,
>     cardClasses = [ClassFighter,ClassArcher],
>     cardStrength = Just 5,
>     cardPrice = Just 9,
>     cardLevelUp = Just 3,
>     cardText = ["ATTACK +2","Additional ATTACK +3 at Rank 2 or 3."],
>     cardGlossary =
>         ["Amazon: This Hero's Dungeon Effect is an Attack that "
>          ++ "is in addition to the Amazon's normal Attack."]
>     }

> heroDetails AmazonQueen = initCardDetails {
>     cardName = "Amazon Queen",
>     cardSource = ThunderstoneBase,
>     cardType = HeroAmazon,
>     cardIcon = CardIconHero 3,
>     cardCount = 2,
>     cardClasses = [ClassFighter,ClassArcher],
>     cardVictoryPoints = Just 2,
>     cardStrength = Just 6,
>     cardPrice = Just 11,
>     cardText = ["ATTACK +2","Additional ATTACK +4 at Rank 2 or 3."],
>     cardGlossary =
>         ["Amazon: This Hero's Dungeon Effect is an Attack that "
>          ++ "is in addition to the Amazon's normal Attack."]
>     }

> heroDetails ChaliceQuester = initCardDetails {
>     cardName = "Chalice Quester",
>     cardSource = ThunderstoneBase,
>     cardType = HeroChalice,
>     cardIcon = CardIconHero 1,
>     cardCount = 6,
>     cardClasses = [ClassFighter,ClassCleric],
>     cardStrength = Just 5,
>     cardPrice = Just 7,
>     cardLevelUp = Just 2,
>     cardText = ["ATTACK +2",
>                 "REPEAT DUNGEON: Destroy one Disease to draw one card."],
>     cardGlossary =
>         ["Chalice Quester and Defender: You man continue to destroy "
>          ++ "Disease cards and draw new cards until you choose "
>          ++ "which Monster to attack."]
>     }

> heroDetails ChaliceDefender = initCardDetails {
>     cardName = "Chalice Defender",
>     cardSource = ThunderstoneBase,
>     cardType = HeroChalice,
>     cardIcon = CardIconHero 2,
>     cardCount = 4,
>     cardClasses = [ClassFighter,ClassCleric],
>     cardStrength = Just 6,
>     cardPrice = Just 10,
>     cardLevelUp = Just 3,
>     cardText = ["ATTACK +3",
>                 "DUNGEON: ATTACK +1 for each Item that produces Light.",
>                 "DUNGEON: Draw one card.",
>                 "REPEAT DUNGEON: Destroy one Disease to draw one card."],
>     cardGlossary =
>         ["Chalice Quester and Defender: You man continue to destroy "
>          ++ "Disease cards and draw new cards until you choose which "
>          ++ "Monster to attack.",
>          "Chalice Defender: Only Items (not Weapons) that provide a "
>          ++ "Light bonus increase the Defender's Attack Value."]
>     }

> heroDetails ChalicePaladin = initCardDetails {
>     cardName = "Chalice Paladin",
>     cardSource = ThunderstoneBase,
>     cardType = HeroChalice,
>     cardIcon = CardIconHero 3,
>     cardCount = 2,
>     cardClasses = [ClassFighter,ClassCleric],
>     cardVictoryPoints = Just 2,
>     cardStrength = Just 7,
>     cardPrice = Just 12,
>     cardText = ["ATTACK +4","DUNGEON: Draw one card.","Spoils (Village)."],
>     cardGlossary =
>         ["Chalic Paladin: You may purchase any one Village card "
>          ++ "(including Basic and Hero cards) from the Village "
>          ++ "after a victorious battle, using the gold in your hand."]
>     }

> heroDetails DwarfGuardian = initCardDetails {
>     cardName = "Dwarf Guardian",
>     cardSource = ThunderstoneBase,
>     cardType = HeroDwarf,
>     cardIcon = CardIconHero 1,
>     cardCount = 6,
>     cardClasses = [ClassFighter],
>     cardStrength = Just 5,
>     cardPrice = Just 6,
>     cardLevelUp = Just 2,
>     cardText = ["ATTACK +1",
>                 "Additional ATTACK +3 when equipped with an Edged Weapon."],
>     cardGlossary =
>         ["Dwarf Guardian: His total Attack Value if an Edged Weapon "
>          ++ "is equipped is +4.  This bonus is part of the Dwarf's "
>          ++ "ability which he retains even if the Weapon later "
>          ++ "becomes useless (due to a Monster's Battle Effect, for "
>          ++ "instance)."]
>     }

> heroDetails DwarfJanissary = initCardDetails {
>     cardName = "Dwarf Janissary",
>     cardSource = ThunderstoneBase,
>     cardType = HeroDwarf,
>     cardIcon = CardIconHero 2,
>     cardCount = 4,
>     cardClasses = [ClassFighter],
>     cardStrength = Just 6,
>     cardPrice = Just 9,
>     cardLevelUp = Just 3,
>     cardText = ["ATTACK +2",
>                 "Additional ATTACK +4 when equipped with an Edged Weapon.",
>                 "Spoils (Weapon)."],
>     cardGlossary =
>         ["Dwarf Janissary: If revealed during a Dungeon action, you "
>          ++ "may purchase one Weapon card from the Village after a "
>          ++ "victorious battle, using the gold in your hand.  His "
>          ++ "total Attack Vlaue if an Edged Weapon is equipped is +6."]
>     }

> heroDetails DwarfSentinel = initCardDetails {
>     cardName = "Dwarf Sentinel",
>     cardSource = ThunderstoneBase,
>     cardType = HeroDwarf,
>     cardIcon = CardIconHero 3,
>     cardCount = 2,
>     cardClasses = [ClassFighter],
>     cardVictoryPoints = Just 2,
>     cardStrength = Just 8,
>     cardPrice = Just 12,
>     cardText = ["ATTACK +3",
>                 "Additional ATTACK +5 when equipped with an Edged Weapon."],
>     cardGlossary =
>         ["Dwarf Sentinel: His total Attack Value with an Edged Weapon "
>          ++ "equipped is +8."]
>     }

> heroDetails ElfWizard = initCardDetails {
>     cardName = "Elf Wizard",
>     cardSource = ThunderstoneBase,
>     cardType = HeroElf,
>     cardIcon = CardIconHero 1,
>     cardCount = 6,
>     cardClasses = [ClassWizard],
>     cardLight = Just 1,
>     cardStrength = Just 3,
>     cardPrice = Just 5,
>     cardLevelUp = Just 2,
>     cardText = ["MAGIC ATTACK +2"]
>     }

> heroDetails ElfSorcerer = initCardDetails {
>     cardName = "Elf Sorcerer",
>     cardSource = ThunderstoneBase,
>     cardType = HeroElf,
>     cardIcon = CardIconHero 2,
>     cardCount = 4,
>     cardClasses = [ClassWizard],
>     cardLight = Just 2,
>     cardStrength = Just 3,
>     cardPrice = Just 8,
>     cardLevelUp = Just 2,
>     cardText = ["MAGIC ATTACK +3",
>                 "You may return one Monster to the bottom of the deck "
>                 ++ "after defeating a monster.  (Refill the hall.)"],
>     cardGlossary =
>         ["Elf Sorcerer/Archmage: When a Monster is returned to the "
>          ++ "bottom of the monster deck, refill the Dungeon Hall.  "
>          ++ "If this results in a Breach effect, resolve it "
>          ++ "immediately.  If the Thunderstone moves to Rank 1 of "
>          ++ "the Dungeon Hall, the game ends immediately; you do "
>          ++ "not collect the Thunderstone."]
>     }

> heroDetails ElfArchmage = initCardDetails {
>     cardName = "Elf Archmage",
>     cardSource = ThunderstoneBase,
>     cardType = HeroElf,
>     cardIcon = CardIconHero 3,
>     cardCount = 2,
>     cardClasses = [ClassWizard],
>     cardLight = Just 2,
>     cardVictoryPoints = Just 2,
>     cardStrength = Just 4,
>     cardPrice = Just 10,
>     cardText = ["MAGIC ATTACK +4",
>                 "DUNGEON You may return one Monster to the bottom of "
>                 ++ "the deck and refill the hall before the beginning of "
>                 ++ "a battle."],
>     cardGlossary =
>         ["Elf Sorcerer/Archmage: When a Monster is returned to the "
>          ++ "bottom of the monster deck, refill the Dungeon Hall.  "
>          ++ "If this results in a Breach effect, resolve it "
>          ++ "immediately.  If the Thunderstone moves to Rank 1 of "
>          ++ "the Dungeon Hall, the game ends immediately; you do "
>          ++ "not collect the Thunderstone."]
>     }

> heroDetails FeaynArcher = initCardDetails {
>     cardName = "Feayn Archer",
>     cardSource = ThunderstoneBase,
>     cardType = HeroFeayn,
>     cardIcon = CardIconHero 1,
>     cardCount = 6,
>     cardClasses = [ClassFighter,ClassArcher],
>     cardLight = Just 1,
>     cardStrength = Just 4,
>     cardPrice = Just 7,
>     cardLevelUp = Just 2,
>     cardText = ["Cannot attack Rank 1.","ATTACK +2"],
>     cardGlossary =
>         ["Feayn: If a Dungeon Actions causes you to attack a Monster "
>          ++ "in Rank 1, do not add the Feayn's Attack bonus to your "
>          ++ "Attack Value.  If Feayn does not attack, his Light bonus "
>          ++ "is lost."]
>     }

> heroDetails FeaynMarksman = initCardDetails {
>     cardName = "Feayn Marksman",
>     cardSource = ThunderstoneBase,
>     cardType = HeroFeayn,
>     cardIcon = CardIconHero 2,
>     cardCount = 4,
>     cardClasses = [ClassFighter,ClassArcher],
>     cardLight = Just 1,
>     cardStrength = Just 5,
>     cardPrice = Just 10,
>     cardLevelUp = Just 3,
>     cardText = ["Cannot attack Rank 1.","ATTACK +3"],
>     cardGlossary =
>         ["Feayn: If a Dungeon Actions causes you to attack a "
>          ++ "Monster in Rank 1, do not add the Feayn's Attack "
>          ++ "bonus to your Attack Value.  If Feayn does not "
>          ++ "attack, his Light bonus is lost."]
>     }

> heroDetails FeaynSniper = initCardDetails {
>     cardName = "Feayn Sniper",
>     cardSource = ThunderstoneBase,
>     cardType = HeroFeayn,
>     cardIcon = CardIconHero 3,
>     cardCount = 2,
>     cardClasses = [ClassFighter,ClassArcher],
>     cardLight = Just 2,
>     cardVictoryPoints = Just 2,
>     cardStrength = Just 6,
>     cardPrice = Just 12,
>     cardText = ["Cannot attack Rank 1.",
>                 "ATTACK +4",
>                 "Gain +1 XP if you defeat a Monster in Rank 3."],
>     cardGlossary =
>         ["Feayn: If a Dungeon Actions causes you to attack a Monster "
>          ++ "in Rank 1, do not add the Feayn's Attack bonus to your "
>          ++ "Attack Value.  If Feayn does not attack, his Light bonus "
>          ++ "is lost."]
>     }

> heroDetails LoriggThief = initCardDetails {
>     cardName = "Lorigg Thief",
>     cardSource = ThunderstoneBase,
>     cardType = HeroLorigg,
>     cardIcon = CardIconHero 1,
>     cardCount = 6,
>     cardClasses = [ClassThief],
>     cardGold = Just 2,
>     cardLight = Just 1,
>     cardStrength = Just 4,
>     cardPrice = Just 5,
>     cardLevelUp = Just 2,
>     cardText = ["ATTACK +1"]
>     }

> heroDetails LoriggRogue = initCardDetails {
>     cardName = "Lorigg Rogue",
>     cardSource = ThunderstoneBase,
>     cardType = HeroLorigg,
>     cardIcon = CardIconHero 2,
>     cardCount = 4,
>     cardClasses = [ClassThief],
>     cardGold = Just 3,
>     cardLight = Just 1,
>     cardStrength = Just 4,
>     cardPrice = Just 8,
>     cardLevelUp = Just 3,
>     cardText = ["ATTACK +2","DUNGEON: All other players discard one card."],
>     cardGlossary =
>         ["Lorigg Outlaw or Rogue: Regardless of whether the battle "
>          ++ "is victorious or not, all other players must discard "
>          ++ "cards when this Hero enters the Dungeon."]
>     }

> heroDetails LoriggOutlaw = initCardDetails {
>     cardName = "Lorigg Outlaw",
>     cardSource = ThunderstoneBase,
>     cardType = HeroLorigg,
>     cardIcon = CardIconHero 3,
>     cardCount = 2,
>     cardClasses = [ClassThief],
>     cardGold = Just 4,
>     cardLight = Just 2,
>     cardVictoryPoints = Just 1,
>     cardStrength = Just 5,
>     cardPrice = Just 10,
>     cardText = ["ATTACK +2","DUNGEON: All other players discard one card."],
>     cardGlossary =
>         ["Lorigg Outlaw or Rogue: Regardless of whether the battle "
>          ++ "is victorious or not, all other players must discard "
>          ++ "cards when this Hero enters the Dungeon."]
>     }

> heroDetails OutlandsWarrior = initCardDetails {
>     cardName = "Outlands Warrior",
>     cardSource = ThunderstoneBase,
>     cardType = HeroOutlands,
>     cardIcon = CardIconHero 1,
>     cardCount = 6,
>     cardClasses = [ClassFighter],
>     cardStrength = Just 7,
>     cardPrice = Just 8,
>     cardLevelUp = Just 2,
>     cardText = ["ATTACK +3",
>                 "DUNGEON: Destroy one Food for an additional ATTACK +3."]
>     }

> heroDetails OutlandsSlayer = initCardDetails {
>     cardName = "Outlands Slayer",
>     cardSource = ThunderstoneBase,
>     cardType = HeroOutlands,
>     cardIcon = CardIconHero 2,
>     cardCount = 4,
>     cardClasses = [ClassFighter],
>     cardStrength = Just 8,
>     cardPrice = Just 11,
>     cardLevelUp = Just 3,
>     cardText = ["ATTACK +5",
>                 "DUNGEON: Gain +1 ATTACK for each Monster card "
>                 ++ "revealed from your hand.",
>                 "REPEAT DUNGEON: Destroy one Food for an additional "
>                 ++ "ATTACK +3."],
>     cardGlossary =
>         ["Outlands Slayer or Khan: The Hero gains an Attack bonus "
>          ++ "for each Monster card revealed in your hand before the battle."]
>     }

> heroDetails OutlandsKhan = initCardDetails {
>     cardName = "Outlands Khan",
>     cardSource = ThunderstoneBase,
>     cardType = HeroOutlands,
>     cardIcon = CardIconHero 3,
>     cardCount = 2,
>     cardClasses = [ClassFighter],
>     cardVictoryPoints = Just 3,
>     cardStrength = Just 8,
>     cardPrice = Just 13,
>     cardText = ["ATTACK +7",
>                 "DUNGEON: ATTACK +2 for each Monster card revealed "
>                 ++ "from your hand."],
>     cardGlossary =
>         ["Outlands Slayer or Khan: The Hero gains an Attack bonus for "
>          ++ "each Monster card revealed in your hand before the battle."]
>     }

> heroDetails RedbladeKiller = initCardDetails {
>     cardName = "Redblade Killer",
>     cardSource = ThunderstoneBase,
>     cardType = HeroRedblade,
>     cardIcon = CardIconHero 1,
>     cardCount = 6,
>     cardClasses = [ClassFighter,ClassThief],
>     cardGold = Just 1,
>     cardStrength = Just 5,
>     cardPrice = Just 5,
>     cardLevelUp = Just 2,
>     cardText = ["ATTACK +2"]
>     }

> heroDetails RedbladePoisoner = initCardDetails {
>     cardName = "Redblade Poisoner",
>     cardSource = ThunderstoneBase,
>     cardType = HeroRedblade,
>     cardIcon = CardIconHero 2,
>     cardCount = 4,
>     cardClasses = [ClassFighter,ClassThief],
>     cardGold = Just 2,
>     cardStrength = Just 5,
>     cardPrice = Just 8,
>     cardLevelUp = Just 3,
>     cardText = ["ATTACK +3",
>                 "DUNGEON: All other players discard one card."],
>     cardGlossary =
>         ["Redblade Assassin or Poisoner: Regardless of whether the "
>          ++ "battle is victorious or not, all other players must "
>          ++ "discard cards when this Hero enters the Dungeon."]
>     }

> heroDetails RedbladeAssassin = initCardDetails {
>     cardName = "Redblade Assassin",
>     cardSource = ThunderstoneBase,
>     cardType = HeroRedblade,
>     cardIcon = CardIconHero 3,
>     cardCount = 2,
>     cardClasses = [ClassFighter,ClassThief],
>     cardGold = Just 2,
>     cardVictoryPoints = Just 2,
>     cardStrength = Just 6,
>     cardPrice = Just 9,
>     cardText = ["ATTACK +4",
>                 "DUNGEON: All other players discard one Hero or two cards."],
>     cardGlossary =
>         ["Redblade Assassin or Poisoner: Regardless of whether the "
>          ++ "battle is victorious or not, all other players must "
>          ++ "discard cards when this Hero enters the Dungeon."]
>     }

> heroDetails RegianCleric = initCardDetails {
>     cardName = "Regian Cleric",
>     cardSource = ThunderstoneBase,
>     cardType = HeroRegian,
>     cardIcon = CardIconHero 1,
>     cardCount = 6,
>     cardClasses = [ClassCleric],
>     cardStrength = Just 4,
>     cardPrice = Just 5,
>     cardLevelUp = Just 2,
>     cardText = ["MAGIC ATTACK +1",
>                 "REPEAT DUNGEON: Destroy one Disease to draw one card."],
>     cardGlossary =
>         ["Regian: You may continue to destroy Disease cards and draw "
>          ++ "new cards ntil the battle begins."]
>     }

> heroDetails RegianPriest = initCardDetails {
>     cardName = "Regian Priest",
>     cardSource = ThunderstoneBase,
>     cardType = HeroRegian,
>     cardIcon = CardIconHero 2,
>     cardCount = 4,
>     cardClasses = [ClassCleric],
>     cardStrength = Just 4,
>     cardPrice = Just 8,
>     cardLevelUp = Just 3,
>     cardText = ["MAGIC ATTACK +2","DUNGEON: Draw one card.",
>                 "REPEAT DUNGEON: Destroy one Disease to draw one card."],
>     cardGlossary =
>         ["Regian: You may continue to destroy Disease cards and draw "
>          ++ "new cards until the battle begins."]
>     }

> heroDetails RegianBishop = initCardDetails {
>     cardName = "Regian Bishop",
>     cardSource = ThunderstoneBase,
>     cardType = HeroRegian,
>     cardIcon = CardIconHero 3,
>     cardCount = 2,
>     cardClasses = [ClassCleric],
>     cardVictoryPoints = Just 2,
>     cardStrength = Just 5,
>     cardPrice = Just 11,
>     cardText = ["MAGIC ATTACK +3","DUNGEON: Draw two cards.",
>                 "REPEAT DUNGEON: Destroy one Disease to draw one card."],
>     cardGlossary =
>         ["Regian: You may continue to destroy Disease cards and draw "
>          ++ "new cards until the battle begins."]
>     }

> heroDetails SelurinMagician = initCardDetails {
>     cardName = "Selurin Magician",
>     cardSource = ThunderstoneBase,
>     cardType = HeroSelurin,
>     cardIcon = CardIconHero 1,
>     cardCount = 6,
>     cardClasses = [ClassWizard],
>     cardStrength = Just 2,
>     cardPrice = Just 8,
>     cardLevelUp = Just 2,
>     cardText = ["MAGIC ATTACK +2",
>                 "All Items and Magic Attack Spells gain MAGIC ATTACK +1."],
>     cardGlossary =
>         ["Selurin: Each Spell with a Magic Attack bonus gains a "
>          ++ "Magic Attack bonus of +1.  Each Item (with the Item "
>          ++ "keyword), regardless of whether it has an Attack bonus "
>          ++ "or not, gains a Magic Attack bonus of +1."]
>     }

> heroDetails SelurinWarlock = initCardDetails {
>     cardName = "Selurin Warlock",
>     cardSource = ThunderstoneBase,
>     cardType = HeroSelurin,
>     cardIcon = CardIconHero 2,
>     cardCount = 4,
>     cardClasses = [ClassWizard],
>     cardStrength = Just 2,
>     cardPrice = Just 10,
>     cardLevelUp = Just 3,
>     cardText = ["MAGIC ATTACK +2","Total MAGIC ATTACK x2* (apply last)"],
>     cardGlossary =
>         ["Selurin Theurge or Warlock: The x2 multiplier of the "
>          ++ "Selurin Wizard affects only Magic Attack bonuses, "
>          ++ "and is applied after all Magic Attack bonuses have "
>          ++ "been calculated.  Multiple Wizards multiply together "
>          ++ "(two become x4, three become x8, etc.)."]
>     }

> heroDetails SelurinTheurge = initCardDetails {
>     cardName = "Selurin Theurge",
>     cardSource = ThunderstoneBase,
>     cardType = HeroSelurin,
>     cardIcon = CardIconHero 3,
>     cardCount = 2,
>     cardClasses = [ClassWizard],
>     cardVictoryPoints = Just 1,
>     cardStrength = Just 3,
>     cardPrice = Just 13,
>     cardText = ["MAGIC ATTACK +2","Total MAGIC ATTACK x2* (apply last)",
>                 "DUNGEON: Each player discards one Hero or shows they "
>                 ++ "have none.  You may borrow one of those discarded "
>                 ++ "Heroes for the battle, returning it at the end."],
>     cardGlossary =
>         ["Selurin Theurge or Warlock: The x2 multiplier of the "
>          ++ "Selurin Wizard affects only Magic Attack bonuses, and is "
>          ++ "applied after all Magic Attack bonuses have been "
>          ++ "calculated.  Multiple Wizards multiply together (two "
>          ++ "become x4, three become x8, etc.).",
>          "Selurin Theurge: If the borrowed Hero is destroyed by a "
>          ++ "Battle Effect, it is not returned to the original owner.  "
>          ++ "Instead, destroy the card."]
>     }

> heroDetails ThyrianSquire = initCardDetails {
>     cardName = "Thyrian Squire",
>     cardSource = ThunderstoneBase,
>     cardType = HeroThyrian,
>     cardIcon = CardIconHero 1,
>     cardCount = 6,
>     cardClasses = [ClassFighter],
>     cardStrength = Just 6,
>     cardPrice = Just 7,
>     cardLevelUp = Just 2,
>     cardText = ["ATTACK +2",
>                 "DUNGEON: Destroy one Food for additional ATTACK +2."],
>     cardGlossary =
>         ["Thyrian: Food destroyed by this Dungeon Effect cannot also "
>          ++ "be used to gain a Strength bonus or for any other effect."]
>     }

> heroDetails ThyrianKnight = initCardDetails {
>     cardName = "Thyrian Knight",
>     cardSource = ThunderstoneBase,
>     cardType = HeroThyrian,
>     cardIcon = CardIconHero 2,
>     cardCount = 4,
>     cardClasses = [ClassFighter],
>     cardStrength = Just 8,
>     cardPrice = Just 9,
>     cardLevelUp = Just 3,
>     cardText = ["ATTACK +4","All Militia gain ATTACK +1.",
>                 "DUNGEON: Destroy one Food for additional ATTACK +2."],
>     cardGlossary =
>         ["Thyrian: Food destroyed by this Dungeon Effect cannot also "
>          ++ "be used to gain a Strength bonus or for any other effect."]
>     }

> heroDetails ThyrianLord = initCardDetails {
>     cardName = "Thyrian Lord",
>     cardSource = ThunderstoneBase,
>     cardType = HeroThyrian,
>     cardIcon = CardIconHero 3,
>     cardCount = 2,
>     cardClasses = [ClassFighter],
>     cardVictoryPoints = Just 2,
>     cardStrength = Just 9,
>     cardPrice = Just 11,
>     cardText = ["ATTACK +4","All Heroes other than Fighters gain ATTACK +2.",
>                 "DUNGEON: Destroy one Food to place one Monster from "
>                 ++ "the hall worth 1 or 2 VP into your discard pile.  "
>                 ++ "Refill the hall."],
>     cardGlossary =
>         ["Thyrian: Food destroyed by this Dungeon Effect cannot also "
>          ++ "be used to gain a Strength bonus or for any other effect.",
>          "Thyrian Lord: You may only select a Monster with 1 or 2 VP, "
>          ++ "and not 0 VP.  When a Monster is placed in your discard pile, "
>          ++ "refill the Dungeon Hall.  If this results in a Breach effect, "
>          ++ "resolve it immediately.  If the Thunderstone moves to Rank 1 "
>          ++ "of the Dungeon Hall, the game ends immediately; you do not "
>          ++ "collect the Thunderstone.  You do not earn any Experience "
>          ++ "Points for the Effect."]
>     }

> heroDetails BlindNeophyte =
>     undefinedHeroDetails HeroBlind WrathOfTheElements 1
> heroDetails BlindMonk =
>     undefinedHeroDetails HeroBlind WrathOfTheElements 2
> heroDetails BlindGrandmaster =
>     undefinedHeroDetails HeroBlind WrathOfTheElements 3
> heroDetails DiinIllusionist =
>     undefinedHeroDetails HeroDiin WrathOfTheElements 1
> heroDetails DiinBeguiler =
>     undefinedHeroDetails HeroDiin WrathOfTheElements 2
> heroDetails DiinEnchanter =
>     undefinedHeroDetails HeroDiin WrathOfTheElements 3
> heroDetails DivineHealer =
>     undefinedHeroDetails HeroDivine WrathOfTheElements 1
> heroDetails DivineChaplain =
>     undefinedHeroDetails HeroDivine WrathOfTheElements 2
> heroDetails DivineProphet =
>     undefinedHeroDetails HeroDivine WrathOfTheElements 3
> heroDetails GanglandThug =
>     undefinedHeroDetails HeroGangland WrathOfTheElements 1
> heroDetails GanglandHeavy =
>     undefinedHeroDetails HeroGangland WrathOfTheElements 2
> heroDetails GanglandCrook =
>     undefinedHeroDetails HeroGangland WrathOfTheElements 3
> heroDetails GohlenTrapper =
>     undefinedHeroDetails HeroGohlen WrathOfTheElements 1
> heroDetails GohlenTracker =
>     undefinedHeroDetails HeroGohlen WrathOfTheElements 2
> heroDetails GohlenHunter =
>     undefinedHeroDetails HeroGohlen WrathOfTheElements 3
> heroDetails RunespawnAdept =
>     undefinedHeroDetails HeroRunespawn WrathOfTheElements 1
> heroDetails RunespawnSiren =
>     undefinedHeroDetails HeroRunespawn WrathOfTheElements 2
> heroDetails RunespawnWitch =
>     undefinedHeroDetails HeroRunespawn WrathOfTheElements 3
> heroDetails TorynScrapper =
>     undefinedHeroDetails HeroToryn WrathOfTheElements 1
> heroDetails TorynDuelist =
>     undefinedHeroDetails HeroToryn WrathOfTheElements 2
> heroDetails TorynGladiator =
>     undefinedHeroDetails HeroToryn WrathOfTheElements 3

> heroDetails DeepMiner =
>     undefinedHeroDetails HeroDeep DoomgateLegion 1
> heroDetails DeepDigger =
>     undefinedHeroDetails HeroDeep DoomgateLegion 2
> heroDetails DeepWrecker =
>     undefinedHeroDetails HeroDeep DoomgateLegion 3
> heroDetails DrunariOrphan =
>     undefinedHeroDetails HeroDrunari DoomgateLegion 1
> heroDetails DrunariVagabond =
>     undefinedHeroDetails HeroDrunari DoomgateLegion 2
> heroDetails DrunariGypsy =
>     undefinedHeroDetails HeroDrunari DoomgateLegion 3
> heroDetails SidheNatural =
>     undefinedHeroDetails HeroSidhe DoomgateLegion 1
> heroDetails SidheDruid =
>     undefinedHeroDetails HeroSidhe DoomgateLegion 2
> heroDetails SidheSpirit =
>     undefinedHeroDetails HeroSidhe DoomgateLegion 3
> heroDetails SlynnBowman =
>     undefinedHeroDetails HeroSlynn DoomgateLegion 1
> heroDetails SlynnBowmaster =
>     undefinedHeroDetails HeroSlynn DoomgateLegion 2
> heroDetails SlynnLongbowman =
>     undefinedHeroDetails HeroSlynn DoomgateLegion 3
> heroDetails TempestAvenger =
>     undefinedHeroDetails HeroTempest DoomgateLegion 1
> heroDetails TempestReaver =
>     undefinedHeroDetails HeroTempest DoomgateLegion 2
> heroDetails TempestWarden =
>     undefinedHeroDetails HeroTempest DoomgateLegion 3
> heroDetails TholisMedium =
>     undefinedHeroDetails HeroTholis DoomgateLegion 1
> heroDetails TholisClairvoyant =
>     undefinedHeroDetails HeroTholis DoomgateLegion 2
> heroDetails TholisOracle =
>     undefinedHeroDetails HeroTholis DoomgateLegion 3
> heroDetails VerdanMinstrel =
>     undefinedHeroDetails HeroVerdan DoomgateLegion 1
> heroDetails VerdanBard =
>     undefinedHeroDetails HeroVerdan DoomgateLegion 2
> heroDetails VerdanTrouadour =
>     undefinedHeroDetails HeroVerdan DoomgateLegion 3

> heroDetails PhalanxFootman =
>     undefinedHeroDetails HeroPhalanx Dragonspire 1
> heroDetails PhalanxOfficer =
>     undefinedHeroDetails HeroPhalanx Dragonspire 2
> heroDetails BelzurCurate =
>     undefinedHeroDetails HeroBelzur Dragonspire 1
> heroDetails BelzurBishop =
>     undefinedHeroDetails HeroBelzur Dragonspire 2
> heroDetails BelzurCardinal =
>     undefinedHeroDetails HeroBelzur Dragonspire 3
> heroDetails CabalAstrologer =
>     undefinedHeroDetails HeroCabal Dragonspire 1
> heroDetails CabalSage =
>     undefinedHeroDetails HeroCabal Dragonspire 2
> heroDetails CabalMaster =
>     undefinedHeroDetails HeroCabal Dragonspire 3
> heroDetails ChulianRat =
>     undefinedHeroDetails HeroChulian Dragonspire 1
> heroDetails ChulianScavenger =
>     undefinedHeroDetails HeroChulian Dragonspire 2
> heroDetails ChulianLooter =
>     undefinedHeroDetails HeroChulian Dragonspire 3
> heroDetails EvokerAdept =
>     undefinedHeroDetails HeroEvoker Dragonspire 1
> heroDetails EvokerScorcher =
>     undefinedHeroDetails HeroEvoker Dragonspire 2
> heroDetails EvokerPyroclast =
>     undefinedHeroDetails HeroEvoker Dragonspire 3
> heroDetails FlameWatch =
>     undefinedHeroDetails HeroFlame Dragonspire 1
> heroDetails FlameGuard =
>     undefinedHeroDetails HeroFlame Dragonspire 2
> heroDetails FlameHero =
>     undefinedHeroDetails HeroFlame Dragonspire 3
> heroDetails GorinthAmateur =
>     undefinedHeroDetails HeroGorinth Dragonspire 1
> heroDetails GorinthHoarder =
>     undefinedHeroDetails HeroGorinth Dragonspire 2
> heroDetails GorinthMiser =
>     undefinedHeroDetails HeroGorinth Dragonspire 3
> heroDetails HalfOrcRaider =
>     undefinedHeroDetails HeroHalfOrc Dragonspire 1
> heroDetails HalfOrcMarauder =
>     undefinedHeroDetails HeroHalfOrc Dragonspire 2
> heroDetails HalfOrcDervish =
>     undefinedHeroDetails HeroHalfOrc Dragonspire 3
> heroDetails StoneguardBrute =
>     undefinedHeroDetails HeroStoneguard Dragonspire 1
> heroDetails StoneguardBruiser =
>     undefinedHeroDetails HeroStoneguard Dragonspire 2
> heroDetails StoneguardTanker =
>     undefinedHeroDetails HeroStoneguard Dragonspire 3
> heroDetails TerakianDefender =
>     undefinedHeroDetails HeroTerakian Dragonspire 1
> heroDetails TerakianPeer =
>     undefinedHeroDetails HeroTerakian Dragonspire 2
> heroDetails TerakianTemplar =
>     undefinedHeroDetails HeroTerakian Dragonspire 3
> heroDetails VeteranWarrior =
>     undefinedHeroDetails HeroVeteran Dragonspire 1
> heroDetails VeteranBerserker =
>     undefinedHeroDetails HeroVeteran Dragonspire 2
> heroDetails VeteranReaver =
>     undefinedHeroDetails HeroVeteran Dragonspire 3
> heroDetails VeteranWarmonger =
>     undefinedHeroDetails HeroVeteran Dragonspire 4

> heroDetails ClanSergeant =
>     undefinedHeroDetails HeroClan Promotional 1
> heroDetails ClanCommander =
>     undefinedHeroDetails HeroClan Promotional 2
> heroDetails ClanChampion =
>     undefinedHeroDetails HeroClan Promotional 3
> heroDetails HarruliInitiate =
>     undefinedHeroDetails HeroHarruli Promotional 1
> heroDetails HarruliSpellsword =
>     undefinedHeroDetails HeroHarruli Promotional 2
> heroDetails HarruliAvatar =
>     undefinedHeroDetails HeroHarruli Promotional 3





> monsterDetails :: MonsterCard -> CardDetails MonsterType

> monsterDetails ArchdukeOfPain = initCardDetails {
>     cardName = "Archduke of Pain",
>     cardSource = ThunderstoneBase,
>     cardType = MonsterAbyssal,
>     cardIcon = CardIconMonster,
>     cardCount = 1,
>     cardClasses = [ClassAbyssal],
>     cardGold = Just 3,
>     cardVictoryPoints = Just 8,
>     cardHealth = Just 10,
>     cardXP = Just 3,
>     cardText = ["Magic Attack Required",
>                 "BREACH: Destroy the top two cards from each Hero deck "
>                 ++ "in the Village.",
>                 "BATTLE: Destroy all Clerics and Wizards."],
>     cardGlossary =
>         ["Archduke of Pain: You must have a Magic Attack of at least "
>          ++ "+1 in order to defeat the Archduke of Pain.  You may still "
>          ++ "choose to attack the Archduke, even without Magic Attack "
>          ++ "present.  If there are no Cleric and/or Wizard cards in "
>          ++ "the battle, there is no effect.  When the Archduke reaches "
>          ++ "Rank 1 of he Dungeon Hall, destroy the top two cards from "
>          ++ "each Hero stack in the Village, including Militia."]
>     }

> monsterDetails Grudgebeast = initCardDetails {
>     cardName = "Grudgebeast",
>     cardSource = ThunderstoneBase,
>     cardType = MonsterAbyssal,
>     cardIcon = CardIconMonster,
>     cardCount = 2,
>     cardClasses = [ClassAbyssal],
>     cardGold = Just 1,
>     cardVictoryPoints = Just 2,
>     cardHealth = Just 4,
>     cardXP = Just 1,
>     cardGlossary = ["Grudgebeast: This Monster has no special Effects."]
>     }

> monsterDetails Succubus = initCardDetails {
>     cardName = "Succubus",
>     cardSource = ThunderstoneBase,
>     cardType = MonsterAbyssal,
>     cardIcon = CardIconMonster,
>     cardCount = 2,
>     cardClasses = [ClassAbyssal],
>     cardGold = Just 2,
>     cardVictoryPoints = Just 4,
>     cardHealth = Just 6,
>     cardXP = Just 2,
>     cardText = ["HALF-ATTACK without MAGIC ATTACK present"],
>     cardGlossary =
>         ["Succubus: If you do not have at least +1 Magic Attack, the "
>          ++ "total Attack Value is halved, round down."]
>     }

> monsterDetails Tormentor = initCardDetails {
>     cardName = "Tormentor",
>     cardSource = ThunderstoneBase,
>     cardType = MonsterAbyssal,
>     cardIcon = CardIconMonster,
>     cardCount = 2,
>     cardClasses = [ClassAbyssal],
>     cardGold = Just 2,
>     cardVictoryPoints = Just 6,
>     cardHealth = Just 8,
>     cardXP = Just 3,
>     cardText = ["HALF-ATTACK without a Weapon present",
>                 "BATTLE: Destroy one Cleric."],
>     cardGlossary =
>         ["Tormentor: If you do not have at least one equipped "
>          ++ "Weapon in the battle, the total Attack Value is halved, "
>          ++ "round down.  If there are no Cleric cards in the battle, "
>          ++ "there is no effect.  Destroyed Cleric cards remain until "
>          ++ "the end of the battle."]
>     }

> monsterDetails TheUnchained = initCardDetails {
>     cardName = "The Unchained",
>     cardSource = ThunderstoneBase,
>     cardType = MonsterAbyssal,
>     cardIcon = CardIconMonster,
>     cardCount = 3,
>     cardClasses = [ClassAbyssal],
>     cardGold = Just 1,
>     cardVictoryPoints = Just 3,
>     cardHealth = Just 5,
>     cardXP = Just 1,
>     cardText = ["BATTLE: Gain one Disease.","* MAGIC ATTACK +1"],
>     cardGlossary =
>         ["Unchained, the: Disease cards gained in battle go directly "
>          ++ "to your discard pile."]
>     }

> monsterDetails Darkness = initCardDetails {
>     cardName = "Darkness",
>     cardSource = ThunderstoneBase,
>     cardType = MonsterDoomknightHumanoid,
>     cardIcon = CardIconMonster,
>     cardCount = 2,
>     cardClasses = [ClassDoomknight,ClassHumanoid],
>     cardGold = Just 1,
>     cardVictoryPoints = Just 4,
>     cardHealth = Just 6,
>     cardXP = Just 2,
>     cardText = ["Unequipped Heroes cannot attack","Light -1"],
>     cardGlossary =
>         ["Darkness: Heroes without a Weapon equipped have an "
>          ++ "Attack Bonus of 0, but are still affected by Battle "
>          ++ "Effects.  Militia are Heroes.  Increase the Light "
>          ++ "Penalty by 1.  Light Penalties are applied before "
>          ++ "the battle."]
>     }

> monsterDetails Judgement = initCardDetails {
>     cardName = "Judgement",
>     cardSource = ThunderstoneBase,
>     cardType = MonsterDoomknightHumanoid,
>     cardIcon = CardIconMonster,
>     cardCount = 2,
>     cardClasses = [ClassDoomknight,ClassHumanoid],
>     cardGold = Just 1,
>     cardVictoryPoints = Just 3,
>     cardHealth = Just 5,
>     cardXP = Just 1,
>     cardText = ["BATTLE: All Heroes suffer Strength -2 and ATTACK -1."],
>     cardGlossary =
>         ["Judgement: You may choose to decrease Attack or Magic Attack "
>          ++ "for this Battle Effect.  Affected Strength can cause "
>          ++ "Weapons to become unequipped.  Militia are Heroes."]
>     }

> monsterDetails Knightmare = initCardDetails {
>     cardName = "Knightmare",
>     cardSource = ThunderstoneBase,
>     cardType = MonsterDoomknightHumanoid,
>     cardIcon = CardIconMonster,
>     cardCount = 2,
>     cardClasses = [ClassDoomknight,ClassHumanoid],
>     cardGold = Just 1,
>     cardVictoryPoints = Just 6,
>     cardHealth = Just 8,
>     cardXP = Just 3,
>     cardText = ["Light -2","BATTLE: Destroy one Fighter."],
>     cardGlossary =
>         ["Knightmare: Increase the Light Penalty of the Rank Knightmare "
>          ++ "is in by 2.  Light Penalties are applied before the battle.  "
>          ++ "If there are no Fighters in the battle, there is no effect."]
>     }

> monsterDetails LordMortis = initCardDetails {
>     cardName = "Lord Mortis",
>     cardSource = ThunderstoneBase,
>     cardType = MonsterDoomknightHumanoid,
>     cardIcon = CardIconMonster,
>     cardCount = 2,
>     cardClasses = [ClassDoomknight,ClassHumanoid],
>     cardGold = Just 1,
>     cardVictoryPoints = Just 2,
>     cardHealth = Just 4,
>     cardXP = Just 1,
>     cardText = ["Light -1"],
>     cardGlossary =
>         ["Lord Mortis: Increase the Light Penalty of the Rank "
>          ++ "Lord Mortis is in by 2.  Light Penalties are applied "
>          ++ "before the battle."]
>     }

> monsterDetails ThePrince = initCardDetails {
>     cardName = "The Prince",
>     cardSource = ThunderstoneBase,
>     cardType = MonsterDoomknightHumanoid,
>     cardIcon = CardIconMonster,
>     cardCount = 2,
>     cardClasses = [ClassDoomknight,ClassHumanoid],
>     cardGold = Just 1,
>     cardVictoryPoints = Just 5,
>     cardHealth = Just 7,
>     cardXP = Just 2,
>     cardText = ["BATTLE: All Heroes suffer Strength -2.",
>                 "BATTLE: Destroy one Fighter."],
>     cardGlossary =
>         ["Prince, the: If there are no Fighter cards in the battle, "
>          ++ "there is no effect.  Reduced Strength can cause Weapons "
>          ++ "to become unequipped."]
>     }

> monsterDetails EbonFume = initCardDetails {
>     cardName = "Ebon Fume",
>     cardSource = ThunderstoneBase,
>     cardType = MonsterDragon,
>     cardIcon = CardIconMonster,
>     cardCount = 1,
>     cardClasses = [ClassDragon,ClassBlack],
>     cardGold = Just 1,
>     cardVictoryPoints = Just 8,
>     cardHealth = Just 11,
>     cardXP = Just 3,
>     cardText = ["Immune to Magic Attack",
>                 "BATTLE: Destroy one Hero with the highest Strength.",
>                 "* ATTACK +3"],
>     cardGlossary =
>         ["Ebon Fume: Magic Attacks against Ebon Fume do not count "
>          ++ "towards the total Attack Value.  If two Heroes have "
>          ++ "the highest (modified) Strength, you choose which to "
>          ++ "destroy.  Militia are considered Heroes."]
>     }

> monsterDetails Mythlurian = initCardDetails {
>     cardName = "Mythlurian",
>     cardSource = ThunderstoneBase,
>     cardType = MonsterDragon,
>     cardIcon = CardIconMonster,
>     cardCount = 2,
>     cardClasses = [ClassDragon,ClassGreen],
>     cardGold = Just 3,
>     cardVictoryPoints = Just 4,
>     cardHealth = Just 8,
>     cardXP = Just 2,
>     cardText = ["BATTLE: Destroy one Hero."],
>     cardGlossary =
>         ["Mythlurian: If there are no Hero cards in the battle, "
>          ++ "there is no effect.  Destroyed Hero cards remain until "
>          ++ "the end of the battle.  Militia are considered Heroes."]
>     }

> monsterDetails Skaladak = initCardDetails {
>     cardName = "Skaladak",
>     cardSource = ThunderstoneBase,
>     cardType = MonsterDragon,
>     cardIcon = CardIconMonster,
>     cardCount = 3,
>     cardClasses = [ClassDragon,ClassWhite],
>     cardGold = Just 3,
>     cardVictoryPoints = Just 3,
>     cardHealth = Just 7,
>     cardXP = Just 2,
>     cardText = ["BATTLE: Destroy one Weapon."],
>     cardGlossary =
>         ["Skaladak: If there are no Weapon cards in the battle, "
>          ++ "there is no effect.  Destroyed Weapons remain until "
>          ++ "the end of the battle."]
>     }

> monsterDetails TyxrTheOld = initCardDetails {
>     cardName = "Tyxr the Old",
>     cardSource = ThunderstoneBase,
>     cardType = MonsterDragon,
>     cardIcon = CardIconMonster,
>     cardCount = 2,
>     cardClasses = [ClassDragon,ClassRed],
>     cardGold = Just 2,
>     cardVictoryPoints = Just 6,
>     cardHealth = Just 10,
>     cardXP = Just 3,
>     cardText = ["BREACH: Each player must discard two cards.",
>                 "BATTLE: Destroy one Hero.",
>                 "* MAGIC ATTACK +2"],
>     cardGlossary =
>         ["Tyxr the Old: If there are no Hero cards in the battle, "
>          ++ "there is no effect.  Destroyed Hero cards remain until "
>          ++ "the end of the battle.  Militia are Heroes.  When Tyxr "
>          ++ "reaches Rank 1 of the Dungeon Hall, all players must "
>          ++ "discard two cards each, including the active player.  "
>          ++ "This takes place before the active player refills his hand."]
>     }

> monsterDetails UyrilUnending = initCardDetails {
>     cardName = "Uyril Unending",
>     cardSource = ThunderstoneBase,
>     cardType = MonsterDragon,
>     cardIcon = CardIconMonster,
>     cardCount = 2,
>     cardClasses = [ClassDragon,ClassBlue],
>     cardGold = Just 2,
>     cardVictoryPoints = Just 5,
>     cardHealth = Just 9,
>     cardXP = Just 2,
>     cardText = ["HALF-ATTACK without MAGIC ATTACK present",
>                 "BATTLE: Destroy one Militia.",
>                 "* MAGIC ATTACK +1"],
>     cardGlossary =
>         ["Uyril Unending: If you do not have at least +1 Magic Attack, "
>          ++ "the total Attack Value is halved, round down.  If there "
>          ++ "are no Militia cards in the battle, there is no effect.  "
>          ++ "Destroyed Militia remain until the end of the battle."]
>     }

> monsterDetails BlinkDog = initCardDetails {
>     cardName = "Blink Dog",
>     cardSource = ThunderstoneBase,
>     cardType = MonsterEnchanted,
>     cardIcon = CardIconMonster,
>     cardCount = 2,
>     cardClasses = [ClassEnchanted],
>     cardGold = Just 1,
>     cardVictoryPoints = Just 2,
>     cardHealth = Just 3,
>     cardXP = Just 1,
>     cardText = ["Light -1",
>                 "Cannot be attacked if a Light Penalty persists."],
>     cardGlossary =
>         ["Blink Dog: If you have a Light Penalty of 1 or more, you "
>          ++ "cannot choose to attack the Blink Dog.  Therefore, without "
>          ++ "sufficient light, you may not choose to attack it merely to "
>          ++ "move it to the bottom of the Dungeon Deck.  Light Penalties "
>          ++ "are applied before the battle."]
>     }

> monsterDetails Griffon = initCardDetails {
>     cardName = "Griffon",
>     cardSource = ThunderstoneBase,
>     cardType = MonsterEnchanted,
>     cardIcon = CardIconMonster,
>     cardCount = 2,
>     cardClasses = [ClassEnchanted],
>     cardGold = Just 1,
>     cardVictoryPoints = Just 4,
>     cardHealth = Just 7,
>     cardXP = Just 2,
>     cardText = ["* MAGIC ATTACK +1"],
>     cardGlossary = ["Griffon: The Magic Attack bonus is a Trophy Effect."]
>     }

> monsterDetails Nixie = initCardDetails {
>     cardName = "Nixie",
>     cardSource = ThunderstoneBase,
>     cardType = MonsterEnchanted,
>     cardIcon = CardIconMonster,
>     cardCount = 2,
>     cardClasses = [ClassEnchanted],
>     cardGold = Just 4,
>     cardVictoryPoints = Just 1,
>     cardHealth = Just 5,
>     cardXP = Just 1,
>     cardGlossary = ["Nixie: This Monster has no special Effects."]
>     }

> monsterDetails Pegasus = initCardDetails {
>     cardName = "Pegasus",
>     cardSource = ThunderstoneBase,
>     cardType = MonsterEnchanted,
>     cardIcon = CardIconMonster,
>     cardCount = 3,
>     cardClasses = [ClassEnchanted],
>     cardGold = Just 2,
>     cardVictoryPoints = Just 2,
>     cardHealth = Just 6,
>     cardXP = Just 1,
>     cardText = ["* ATTACK +1"],
>     cardGlossary = ["Pegasus: The Attack bonus is a Trophy Effect."]
>     }

> monsterDetails Sphinx = initCardDetails {
>     cardName = "Sphinx",
>     cardSource = ThunderstoneBase,
>     cardType = MonsterEnchanted,
>     cardIcon = CardIconMonster,
>     cardCount = 1,
>     cardClasses = [ClassEnchanted],
>     cardGold = Just 2,
>     cardVictoryPoints = Just 7,
>     cardHealth = Just 8,
>     cardXP = Just 3,
>     cardText = ["Magic Attack Only",
>                 "Spoils (Reveal six cards from your deck and destroy "
>                 ++ "any of these cards you choose.  Discard the rest.)",
>                 "* MAGIC ATTACK +2"],
>     cardGlossary =
>         ["Sphinx: Only Magic Attack bonuses count against the Sphinx.  "
>          ++ "The six cards revealed from your deck do not affect or "
>          ++ "replace your hand, and are drawn before any Breach or "
>          ++ "Trap Effects are resolved.  This is a Spoils effect."]
>     }

> monsterDetails BloodskullOrc = initCardDetails {
>     cardName = "Bloodskull Orc",
>     cardSource = ThunderstoneBase,
>     cardType = MonsterHumanoid,
>     cardIcon = CardIconMonster,
>     cardCount = 2,
>     cardClasses = [ClassHumanoid],
>     cardGold = Just 3,
>     cardHealth = Just 5,
>     cardXP = Just 1,
>     cardGlossary =
>         ["Bloodskull Orc: This monster has no special Effects."]
>     }

> monsterDetails DeadboneTroll = initCardDetails {
>     cardName = "Deadbone Troll",
>     cardSource = ThunderstoneBase,
>     cardType = MonsterHumanoid,
>     cardIcon = CardIconMonster,
>     cardCount = 2,
>     cardClasses = [ClassHumanoid],
>     cardGold = Just 2,
>     cardVictoryPoints = Just 5,
>     cardHealth = Just 7,
>     cardXP = Just 2,
>     cardGlossary =
>         ["Deadbone Troll: Disease cards gained in battle go directly "
>          ++ "to your discard pile."]
>     }

> monsterDetails FirebrandCyclops = initCardDetails {
>     cardName = "Firebrand Cyclops",
>     cardSource = ThunderstoneBase,
>     cardType = MonsterHumanoid,
>     cardIcon = CardIconMonster,
>     cardCount = 2,
>     cardClasses = [ClassHumanoid],
>     cardGold = Just 1,
>     cardVictoryPoints = Just 4,
>     cardHealth = Just 6,
>     cardXP = Just 2,
>     cardGlossary =
>         ["Firebrand Cyclops: This Monster has no special Effects."]
>     }

> monsterDetails GrayskinLizard = initCardDetails {
>     cardName = "Grayskin Lizard",
>     cardSource = ThunderstoneBase,
>     cardType = MonsterHumanoid,
>     cardIcon = CardIconMonster,
>     cardCount = 2,
>     cardClasses = [ClassHumanoid],
>     cardGold = Just 2,
>     cardVictoryPoints = Just 3,
>     cardHealth = Just 6,
>     cardXP = Just 1,
>     cardGlossary =
>         ["Grayskin Lizard: This Battle Effect makes the vulnerable "
>          ++ "to Weapons.  They lack thick skin."]
>     }

> monsterDetails GriknackGoblin = initCardDetails {
>     cardName = "Griknack Goblin",
>     cardSource = ThunderstoneBase,
>     cardType = MonsterHumanoid,
>     cardIcon = CardIconMonster,
>     cardCount = 2,
>     cardClasses = [ClassHumanoid],
>     cardGold = Just 1,
>     cardVictoryPoints = Just 1,
>     cardHealth = Just 4,
>     cardXP = Just 1,
>     cardGlossary =
>         ["Griknack Goblin: This Monster has no special Effects."]
>     }

> monsterDetails BlackSlime = initCardDetails {
>     cardName = "Black Slime",
>     cardSource = ThunderstoneBase,
>     cardType = MonsterOoze,
>     cardIcon = CardIconMonster,
>     cardCount = 2,
>     cardClasses = [ClassOoze],
>     cardGold = Just 1,
>     cardVictoryPoints = Just 2,
>     cardHealth = Just 5,
>     cardXP = Just 1,
>     cardText = ["BATTLE: Destroy one Militia."],
>     cardGlossary =
>         ["Black Slime: If there are no Militia cards in the battle, "
>          ++ "there is no effect.  Destroyed Militia remain until the "
>          ++ "end of the battle."]
>     }

> monsterDetails GrayOoze = initCardDetails {
>     cardName = "Gray Ooze",
>     cardSource = ThunderstoneBase,
>     cardType = MonsterOoze,
>     cardIcon = CardIconMonster,
>     cardCount = 2,
>     cardClasses = [ClassOoze],
>     cardGold = Just 1,
>     cardVictoryPoints = Just 3,
>     cardHealth = Just 6,
>     cardXP = Just 2,
>     cardText = ["BATTLE: Destroy one Hero unless at least one Weapon "
>                 ++ "is attached to the Party.",
>                 "Spoils (Food)"],
>     cardGlossary =
>         ["Gray Oooze: If at least one Hero has a Weapon equipped (or "
>          ++ "there are not Heroes), there is no effect.  Destroyed "
>          ++ "Hero cards remain until the end of the battle.  Militia "
>          ++ "are Heroes, and can be destroyed.  After a victorious "
>          ++ "battle, you may purchase one Food card from the Village, "
>          ++ "using the gold in your hand."]
>     }

> monsterDetails GreenBlob = initCardDetails {
>     cardName = "Green Blob",
>     cardSource = ThunderstoneBase,
>     cardType = MonsterOoze,
>     cardIcon = CardIconMonster,
>     cardCount = 2,
>     cardClasses = [ClassOoze],
>     cardGold = Just 1,
>     cardVictoryPoints = Just 2,
>     cardHealth = Just 5,
>     cardXP = Just 1,
>     cardText = ["BATTLE: Destroy one Food."],
>     cardGlossary =
>         ["Green Blob: If there are no Food cards in the battle, "
>          ++ "there is no effect."]
>     }

> monsterDetails NoxiousSlag = initCardDetails {
>     cardName = "Noxious Slag",
>     cardSource = ThunderstoneBase,
>     cardType = MonsterOoze,
>     cardIcon = CardIconMonster,
>     cardCount = 2,
>     cardClasses = [ClassOoze],
>     cardGold = Just 1,
>     cardVictoryPoints = Just 6,
>     cardHealth = Just 7,
>     cardXP = Just 3,
>     cardText = ["HALF-ATTACK from MAGIC ATTACK","Immune to Edged Weapons"],
>     cardGlossary =
>         ["Noxious Slag: Edged Weapons do not add to your total "
>          ++ "Attack Value against the Noxious Slag.  After "
>          ++ "calculating your total Magic Attack Value, cut the "
>          ++ "total in half (round down)."]
>     }

> monsterDetails RedJelly = initCardDetails {
>     cardName = "Red Jelly",
>     cardSource = ThunderstoneBase,
>     cardType = MonsterOoze,
>     cardIcon = CardIconMonster,
>     cardCount = 2,
>     cardClasses = [ClassOoze],
>     cardGold = Just 2,
>     cardVictoryPoints = Just 2,
>     cardHealth = Just 6,
>     cardXP = Just 2,
>     cardText = ["BATTLE: Destroy one Weapon."],
>     cardGlossary =
>         ["Red Jelly: If there are no Weapon cards in the battle, "
>          ++ "there is no effect.  The Light bonus is a Trophy Effect.  "
>          ++ "It only applies when the defeated Red Jelly is revealed "
>          ++ "in your hand.  It is not a Battle Effect."]
>     }

> monsterDetails Famine = initCardDetails {
>     cardName = "Famine",
>     cardSource = ThunderstoneBase,
>     cardType = MonsterUndeadDoom,
>     cardIcon = CardIconMonster,
>     cardCount = 2,
>     cardClasses = [ClassUndead,ClassDoom],
>     cardGold = Just 1,
>     cardVictoryPoints = Just 2,
>     cardHealth = Just 4,
>     cardXP = Just 1,
>     cardText = ["BATTLE: Gain one Disease."],
>     cardGlossary =
>         ["Famine: Disease cards gained in battle go directly to "
>          ++ "your discard pile."]
>     }

> monsterDetails Harbinger = initCardDetails {
>     cardName = "Harbinger",
>     cardSource = ThunderstoneBase,
>     cardType = MonsterUndeadDoom,
>     cardIcon = CardIconMonster,
>     cardCount = 2,
>     cardClasses = [ClassUndead,ClassDoom],
>     cardGold = Just 2,
>     cardVictoryPoints = Just 1,
>     cardHealth = Just 3,
>     cardXP = Just 1,
>     cardText = ["BATTLE: Destroy one Spell."],
>     cardGlossary =
>         ["Harbinger: If there are no Spell cards in the battle, "
>          ++ "there is no effect.  Destroyed Spell cards remain "
>          ++ "until the end of the battle."]
>     }

> monsterDetails Kingdom = initCardDetails {
>     cardName = "Kingdom",
>     cardSource = ThunderstoneBase,
>     cardType = MonsterUndeadDoom,
>     cardIcon = CardIconMonster,
>     cardCount = 3,
>     cardClasses = [ClassUndead,ClassDoom],
>     cardGold = Just 2,
>     cardVictoryPoints = Just 3,
>     cardHealth = Just 5,
>     cardXP = Just 1,
>     cardText = ["BATTLE: Gain one Disease."],
>     cardGlossary =
>         ["Kingdom: Disease cards gained in battle go directly to "
>          ++ "your dicard pile."]
>     }

> monsterDetails LordOfDeath = initCardDetails {
>     cardName = "Lord of Death",
>     cardSource = ThunderstoneBase,
>     cardType = MonsterUndeadDoom,
>     cardIcon = CardIconMonster,
>     cardCount = 1,
>     cardClasses = [ClassUndead,ClassDoom],
>     cardGold = Just 2,
>     cardVictoryPoints = Just 7,
>     cardHealth = Just 9,
>     cardXP = Just 3,
>     cardText = ["BATTLE: Gain two Diseases."],
>     cardGlossary =
>         ["Lord of Death: Disease cards gained in battle go directly "
>          ++ "to your discard pile."]
>     }

> monsterDetails Suffering = initCardDetails {
>     cardName = "Suffering",
>     cardSource = ThunderstoneBase,
>     cardType = MonsterUndeadDoom,
>     cardIcon = CardIconMonster,
>     cardCount = 2,
>     cardClasses = [ClassUndead,ClassDoom],
>     cardGold = Just 2,
>     cardVictoryPoints = Just 4,
>     cardHealth = Just 6,
>     cardXP = Just 2,
>     cardText = ["BATTLE: All Heroes suffer Strength -2.",
>                 "BATTLE: Gain one Disease."],
>     cardGlossary =
>         ["Suffering: Reduced Strength can cause Weapons to become "
>          ++ "unequipped.  Disease cards gained in battle go directly "
>          ++ "to your discard pile."]
>     }

> monsterDetails Ghost = initCardDetails {
>     cardName = "Ghost",
>     cardSource = ThunderstoneBase,
>     cardType = MonsterUndeadSpirit,
>     cardIcon = CardIconMonster,
>     cardCount = 2,
>     cardClasses = [ClassUndead,ClassSpirit],
>     cardGold = Just 1,
>     cardVictoryPoints = Just 4,
>     cardHealth = Just 6,
>     cardXP = Just 1,
>     cardText = ["HALF-ATTACK without MAGIC ATTACK present",
>                 "BATTLE: All Heroes suffer Strength -2."],
>     cardGlossary =
>         ["Ghost: If you do not have at least +1 Magic Attack, "
>          ++ "the total Attack Value is halved, rounded down.  "
>          ++ "Reduced Strength can cause Weapons to become unequipped."]
>     }

> monsterDetails Haunt = initCardDetails {
>     cardName = "Haunt",
>     cardSource = ThunderstoneBase,
>     cardType = MonsterUndeadSpirit,
>     cardIcon = CardIconMonster,
>     cardCount = 2,
>     cardClasses = [ClassUndead,ClassSpirit],
>     cardGold = Just 1,
>     cardVictoryPoints = Just 2,
>     cardHealth = Just 4,
>     cardXP = Just 1,
>     cardText = ["BATTLE: One Hero cannot attack."],
>     cardGlossary =
>         ["Haunt: Choose one Hero (and any equipped Weapon).  "
>          ++ "All Light, Attack, and Magic Attack of the Hero "
>          ++ "(and Weapon) are reduced to 0.  Militia are Heroes."]
>     }

> monsterDetails Revenant = initCardDetails {
>     cardName = "Revenant",
>     cardSource = ThunderstoneBase,
>     cardType = MonsterUndeadSpirit,
>     cardIcon = CardIconMonster,
>     cardCount = 2,
>     cardClasses = [ClassUndead,ClassSpirit],
>     cardGold = Just 2,
>     cardVictoryPoints = Just 5,
>     cardHealth = Just 7,
>     cardXP = Just 2,
>     cardText = ["Magic Attack Required",
>                 "BATTLE: All Heroes suffer Strength -4.  Any Heroes "
>                 ++ "with Strength 0 or less are Destroyed."],
>     cardGlossary =
>         ["Revenant: You must have at least +1 Magic Attack in order "
>          ++ "to kill the Revenant.  Heroes destroyed by the Revenant "
>          ++ "die at the end of the battle.  Reduce Strength can cause "
>          ++ "Weapons to become unequipped."]
>     }

> monsterDetails Spectre = initCardDetails {
>     cardName = "Spectre",
>     cardSource = ThunderstoneBase,
>     cardType = MonsterUndeadSpirit,
>     cardIcon = CardIconMonster,
>     cardCount = 2,
>     cardClasses = [ClassUndead,ClassSpirit],
>     cardGold = Just 1,
>     cardVictoryPoints = Just 3,
>     cardHealth = Just 5,
>     cardXP = Just 1,
>     cardText = ["BATTLE: Destroy one Militia."],
>     cardGlossary =
>         ["Spectre: If there are no Militia cards in the battle, there "
>          ++ "is no effect.  Destroyed Militia remain until the end of "
>          ++ "the battle."]
>     }

> monsterDetails Wraith = initCardDetails {
>     cardName = "Wraith",
>     cardSource = ThunderstoneBase,
>     cardType = MonsterUndeadSpirit,
>     cardIcon = CardIconMonster,
>     cardCount = 2,
>     cardClasses = [ClassUndead,ClassSpirit],
>     cardGold = Just 2,
>     cardVictoryPoints = Just 4,
>     cardHealth = Just 6,
>     cardXP = Just 2,
>     cardText = ["BATTLE: Destroy one Militia."],
>     cardGlossary =
>         ["Wraith: If there are no Militia cards in the battle, "
>          ++ "there is no effect.  Destroyed Militia remain until "
>          ++ "the end of the battle."]
>     }

> monsterDetails AirWrath =
>     undefinedCardDetails MonsterElementalNature WrathOfTheElements
> monsterDetails EarthWrath =
>     undefinedCardDetails MonsterElementalNature WrathOfTheElements
> monsterDetails FireWrath =
>     undefinedCardDetails MonsterElementalNature WrathOfTheElements
> monsterDetails ThunderWrath =
>     undefinedCardDetails MonsterElementalNature WrathOfTheElements
> monsterDetails WaterWrath =
>     undefinedCardDetails MonsterElementalNature WrathOfTheElements

> monsterDetails BloodTorment =
>     undefinedCardDetails MonsterElementalPain WrathOfTheElements
> monsterDetails LavaTorment =
>     undefinedCardDetails MonsterElementalPain WrathOfTheElements
> monsterDetails ShadowTorment =
>     undefinedCardDetails MonsterElementalPain WrathOfTheElements
> monsterDetails SmokeTorment =
>     undefinedCardDetails MonsterElementalPain WrathOfTheElements
> monsterDetails SteamTorment =
>     undefinedCardDetails MonsterElementalPain WrathOfTheElements

> monsterDetails BronzeGolem =
>     undefinedCardDetails MonsterGolem WrathOfTheElements
> monsterDetails ClayGolem =
>     undefinedCardDetails MonsterGolem WrathOfTheElements
> monsterDetails Colossus =
>     undefinedCardDetails MonsterGolem WrathOfTheElements
> monsterDetails IronGolem =
>     undefinedCardDetails MonsterGolem WrathOfTheElements
> monsterDetails StoneGolem =
>     undefinedCardDetails MonsterGolem WrathOfTheElements

> monsterDetails HordePlaceholder =
>     undefinedCardDetails MonsterHordeHumanoid WrathOfTheElements
> monsterDetails Horde3 =
>     undefinedCardDetails MonsterHordeHumanoid WrathOfTheElements
> monsterDetails Horde4 =
>     undefinedCardDetails MonsterHordeHumanoid WrathOfTheElements
> monsterDetails Horde5 =
>     undefinedCardDetails MonsterHordeHumanoid WrathOfTheElements
> monsterDetails Horde6 =
>     undefinedCardDetails MonsterHordeHumanoid WrathOfTheElements
> monsterDetails Horde7 =
>     undefinedCardDetails MonsterHordeHumanoid WrathOfTheElements
> monsterDetails Horde8 =
>     undefinedCardDetails MonsterHordeHumanoid WrathOfTheElements
> monsterDetails Horde9 =
>     undefinedCardDetails MonsterHordeHumanoid WrathOfTheElements
> monsterDetails Horde10 =
>     undefinedCardDetails MonsterHordeHumanoid WrathOfTheElements
> monsterDetails Horde11 =
>     undefinedCardDetails MonsterHordeHumanoid WrathOfTheElements
> monsterDetails Horde12 =
>     undefinedCardDetails MonsterHordeHumanoid WrathOfTheElements

> monsterDetails TheBloodless =
>     undefinedCardDetails MonsterAbyssalThunderspawn DoomgateLegion
> monsterDetails Razorback =
>     undefinedCardDetails MonsterAbyssalThunderspawn DoomgateLegion
> monsterDetails Regicide =
>     undefinedCardDetails MonsterAbyssalThunderspawn DoomgateLegion
> monsterDetails TendrilMinion =
>     undefinedCardDetails MonsterAbyssalThunderspawn DoomgateLegion
> monsterDetails Usurper =
>     undefinedCardDetails MonsterAbyssalThunderspawn DoomgateLegion

> monsterDetails TheAuthority =
>     undefinedCardDetails MonsterCultistHumanoid DoomgateLegion
> monsterDetails TheCleansed =
>     undefinedCardDetails MonsterCultistHumanoid DoomgateLegion
> monsterDetails TheDevout =
>     undefinedCardDetails MonsterCultistHumanoid DoomgateLegion
> monsterDetails TheFaithful =
>     undefinedCardDetails MonsterCultistHumanoid DoomgateLegion
> monsterDetails TheVoice =
>     undefinedCardDetails MonsterCultistHumanoid DoomgateLegion

> monsterDetails Arachnea =
>     undefinedCardDetails MonsterEvilDruid DoomgateLegion
> monsterDetails CrowTalker =
>     undefinedCardDetails MonsterEvilDruid DoomgateLegion
> monsterDetails GaiasCurse =
>     undefinedCardDetails MonsterEvilDruid DoomgateLegion
> monsterDetails MonarchDruid =
>     undefinedCardDetails MonsterEvilDruid DoomgateLegion
> monsterDetails NaturesMistress =
>     undefinedCardDetails MonsterEvilDruid DoomgateLegion

> monsterDetails SwarmPlaceholder =
>     undefinedCardDetails MonsterTheSwarmAnimal DoomgateLegion
> monsterDetails Swarm4 =
>     undefinedCardDetails MonsterTheSwarmAnimal DoomgateLegion
> monsterDetails Swarm5 =
>     undefinedCardDetails MonsterTheSwarmAnimal DoomgateLegion
> monsterDetails Swarm6 =
>     undefinedCardDetails MonsterTheSwarmAnimal DoomgateLegion
> monsterDetails Swarm7 =
>     undefinedCardDetails MonsterTheSwarmAnimal DoomgateLegion
> monsterDetails Swarm8 =
>     undefinedCardDetails MonsterTheSwarmAnimal DoomgateLegion
> monsterDetails Swarm9 =
>     undefinedCardDetails MonsterTheSwarmAnimal DoomgateLegion
> monsterDetails Swarm10 =
>     undefinedCardDetails MonsterTheSwarmAnimal DoomgateLegion
> monsterDetails Swarm11 =
>     undefinedCardDetails MonsterTheSwarmAnimal DoomgateLegion
> monsterDetails Swarm12 =
>     undefinedCardDetails MonsterTheSwarmAnimal DoomgateLegion
> monsterDetails Swarm13 =
>     undefinedCardDetails MonsterTheSwarmAnimal DoomgateLegion

> monsterDetails Deathchill =
>     undefinedCardDetails MonsterUndeadStormwraith DoomgateLegion
> monsterDetails Hellstorm =
>     undefinedCardDetails MonsterUndeadStormwraith DoomgateLegion
> monsterDetails LightningsGaze =
>     undefinedCardDetails MonsterUndeadStormwraith DoomgateLegion
> monsterDetails MurderWind =
>     undefinedCardDetails MonsterUndeadStormwraith DoomgateLegion
> monsterDetails Rage =
>     undefinedCardDetails MonsterUndeadStormwraith DoomgateLegion

> monsterDetails Assassin =
>     undefinedCardDetails MonsterBanditHumanoid Dragonspire
> monsterDetails Cutthroat =
>     undefinedCardDetails MonsterBanditHumanoid Dragonspire
> monsterDetails Highwayman =
>     undefinedCardDetails MonsterBanditHumanoid Dragonspire
> monsterDetails Stalker =
>     undefinedCardDetails MonsterBanditHumanoid Dragonspire
> monsterDetails Thug =
>     undefinedCardDetails MonsterBanditHumanoid Dragonspire

> monsterDetails Basilisk =
>     undefinedCardDetails MonsterDarkEnchanted Dragonspire
> monsterDetails Harpy =
>     undefinedCardDetails MonsterDarkEnchanted Dragonspire
> monsterDetails Manticore =
>     undefinedCardDetails MonsterDarkEnchanted Dragonspire
> monsterDetails Medusa =
>     undefinedCardDetails MonsterDarkEnchanted Dragonspire
> monsterDetails Minotaur =
>     undefinedCardDetails MonsterDarkEnchanted Dragonspire

> monsterDetails Blaze =
>     undefinedCardDetails MonsterElementalFire Dragonspire
> monsterDetails ChokingSmoke =
>     undefinedCardDetails MonsterElementalFire Dragonspire
> monsterDetails Ember =
>     undefinedCardDetails MonsterElementalFire Dragonspire
> monsterDetails Flare =
>     undefinedCardDetails MonsterElementalFire Dragonspire
> monsterDetails Inferno =
>     undefinedCardDetails MonsterElementalFire Dragonspire

> monsterDetails FireGiant =
>     undefinedCardDetails MonsterGiant Dragonspire
> monsterDetails FrostGiant =
>     undefinedCardDetails MonsterGiant Dragonspire
> monsterDetails MountainGiant =
>     undefinedCardDetails MonsterGiant Dragonspire
> monsterDetails StoneGiant =
>     undefinedCardDetails MonsterGiant Dragonspire
> monsterDetails Titan =
>     undefinedCardDetails MonsterGiant Dragonspire

> monsterDetails EarthTempest =
>     undefinedCardDetails MonsterHydraDragon Dragonspire
> monsterDetails FlameRage =
>     undefinedCardDetails MonsterHydraDragon Dragonspire
> monsterDetails Hydra =
>     undefinedCardDetails MonsterHydraDragon Dragonspire
> monsterDetails WaterWrathHydra =
>     undefinedCardDetails MonsterHydraDragon Dragonspire
> monsterDetails WindFury =
>     undefinedCardDetails MonsterHydraDragon Dragonspire

> monsterDetails HalfOgre =
>     undefinedCardDetails MonsterOrcHumanoid Dragonspire
> monsterDetails OrcBlademaster =
>     undefinedCardDetails MonsterOrcHumanoid Dragonspire
> monsterDetails OrcWarlord =
>     undefinedCardDetails MonsterOrcHumanoid Dragonspire
> monsterDetails ShadowKiller =
>     undefinedCardDetails MonsterOrcHumanoid Dragonspire
> monsterDetails StandardBearer =
>     undefinedCardDetails MonsterOrcHumanoid Dragonspire

> monsterDetails Deathbringer =
>     undefinedCardDetails MonsterUndeadLich Dragonspire
> monsterDetails Destiny =
>     undefinedCardDetails MonsterUndeadLich Dragonspire
> monsterDetails GraveKnight =
>     undefinedCardDetails MonsterUndeadLich Dragonspire
> monsterDetails LichLord =
>     undefinedCardDetails MonsterUndeadLich Dragonspire
> monsterDetails TombHaunt =
>     undefinedCardDetails MonsterUndeadLich Dragonspire

> monsterDetails HungryDead =
>     undefinedCardDetails MonsterUndeadPlague Dragonspire
> monsterDetails Plaguebearer =
>     undefinedCardDetails MonsterUndeadPlague Dragonspire
> monsterDetails PlagueZombie =
>     undefinedCardDetails MonsterUndeadPlague Dragonspire
> monsterDetails RestlessCorpse =
>     undefinedCardDetails MonsterUndeadPlague Dragonspire
> monsterDetails WalkingScourge =
>     undefinedCardDetails MonsterUndeadPlague Dragonspire

> monsterDetails TheVision =
>     undefinedCardDetails MonsterCultistHumanoid Promotional
> monsterDetails Mammoth =
>     undefinedCardDetails MonsterEvilDruid Promotional






> villageDetails :: VillageCard -> CardDetails VillageCard
> villageDetails Dagger = initCardDetails {
>     cardName = "Dagger",
>     cardSource = ThunderstoneBase,
>     cardType = Dagger,
>     cardIcon = CardIconBasic,
>     cardCount = 15,
>     cardClasses = [ClassWeapon,ClassEdged],
>     cardGold = Just 1,
>     cardWeight = Just 2,
>     cardPrice = Just 3,
>     cardText = ["ATTACK +1"],
>     cardGlossary =
>         ["Dagger: This is an Edged Weapon.  It is a Basic card "
>          ++ "included in every game."]
>     }

> villageDetails IronRations = initCardDetails {
>     cardName = "Iron Rations",
>     cardSource = ThunderstoneBase,
>     cardType = IronRations,
>     cardIcon = CardIconBasic,
>     cardCount = 15,
>     cardClasses = [ClassItem,ClassFood],
>     cardGold = Just 2,
>     cardPrice = Just 2,
>     cardText = ["DUNGEON: One Hero gains Strength +2."],
>     cardGlossary =
>         ["Iron Rations: You can use multiple Iron Rations to "
>          ++ "increase the same Hero's Strength.  If a Dungeon Effect "
>          ++ "destroys Iron Rations, you cannot use it to gain the "
>          ++ "Strength bonus.  It is a Basic card included in every game."]
>     }

> villageDetails Torch = initCardDetails {
>     cardName = "Torch",
>     cardSource = ThunderstoneBase,
>     cardType = Torch,
>     cardIcon = CardIconBasic,
>     cardCount = 15,
>     cardClasses = [ClassItem,ClassLight],
>     cardGold = Just 2,
>     cardLight = Just 1,
>     cardPrice = Just 3,
>     cardGlossary =
>         ["Torch: This Item always provides Light +1, even when no "
>          ++ "Hero is present.  It is a Basic card included in every game."]
>     }

> villageDetails ArcaneEnergies = initCardDetails {
>     cardName = "Arcane Energies",
>     cardSource = ThunderstoneBase,
>     cardType = ArcaneEnergies,
>     cardIcon = CardIconVillage,
>     cardCount = 8,
>     cardClasses = [ClassSpell],
>     cardPrice = Just 2,
>     cardText = ["DUNGEON: All ATTACKS from Heroes with Weapons "
>                 ++ "equipped become MAGIC ATTACKS.  Draw one card."],
>     cardGlossary =
>         ["Arcane Energies: You must draw a card when you use this "
>          ++ "dungeon ability."]
>     }

> villageDetails Banish = initCardDetails {
>     cardName = "Banish",
>     cardSource = ThunderstoneBase,
>     cardType = Banish,
>     cardIcon = CardIconVillage,
>     cardCount = 8,
>     cardClasses = [ClassSpell],
>     cardPrice = Just 4,
>     cardText = ["DUNGEON: Return one Monster to the bottom of the "
>                 ++ "deck and refill the hall, or rearrange the hall.  "
>                 ++ "Destroy one card from your hand.  Draw one card."],
>     cardGlossary =
>         ["Banish: You must declare you are entering the Dungeon "
>          ++ "to play Banish, but do not choose which Monster to "
>          ++ "attack until after the Hall is refilled.  If Banish "
>          ++ "results in a Breach (or Trap) Effect, resolve it "
>          ++ "immediately.  You may rearrange the hall so as to "
>          ++ "place the Thunderstone in Rank 1 of the Dungeon Hall, "
>          ++ "ending the game immediately without collecting the "
>          ++ "Thunderstone.  Multiple Banish cards can be used "
>          ++ "before choosing which Monster to attack, but each must "
>          ++ "be completely resolved before the next can be played.  "
>          ++ "You must draw a card when using this Dungeon Ability."]
>     }

> villageDetails Barkeep = initCardDetails {
>     cardName = "Barkeep",
>     cardSource = ThunderstoneBase,
>     cardType = Barkeep,
>     cardIcon = CardIconVillage,
>     cardCount = 8,
>     cardClasses = [ClassVillager],
>     cardGold = Just 1,
>     cardPrice = Just 2,
>     cardText = ["VILLAGE: You may purchase one additional card this turn.",
>                 "VILLAGE: Destroy this card to gain 2 Gold."],
>     cardGlossary =
>         ["Barkeep: Each additional Barkeep allows you to purchase one "
>          ++ "additional card.  You do not gain the gold value of the "
>          ++ "Barkeep when it is destroyed."]
>     }

> villageDetails BattleFury = initCardDetails {
>     cardName = "BattleFury",
>     cardSource = ThunderstoneBase,
>     cardType = BattleFury,
>     cardIcon = CardIconVillage,
>     cardCount = 8,
>     cardClasses = [ClassSpell],
>     cardPrice = Just 3,
>     cardText = ["DUNGEON: All Heroes gain ATTACK +1."],
>     cardGlossary =
>         ["Battle Fury: Militia are Heroes, and gain the Attack bonus "
>          ++ "from this spell."]
>     }

> villageDetails Feast = initCardDetails {
>     cardName = "Feast",
>     cardSource = ThunderstoneBase,
>     cardType = Feast,
>     cardIcon = CardIconVillage,
>     cardCount = 8,
>     cardClasses = [ClassItem,ClassFood],
>     cardGold = Just 3,
>     cardPrice = Just 5,
>     cardText = ["DUNGEON: All Heroes gain Strength +1 and ATTACK +1."],
>     cardGlossary =
>         ["Feast: Militia cards are Heroes, so they gain the Attack and "
>          ++ "Strength bonuses from this card."]
>     }

> villageDetails Fireball = initCardDetails {
>     cardName = "Fireball",
>     cardSource = ThunderstoneBase,
>     cardType = Fireball,
>     cardIcon = CardIconVillage,
>     cardCount = 8,
>     cardClasses = [ClassSpell],
>     cardLight = Just 1,
>     cardPrice = Just 9,
>     cardText = ["MAGIC ATTACK +3"],
>     cardGlossary =
>         ["Fireball: You do not need Heroes present to use this Spell."]
>     }

> villageDetails FlamingSword = initCardDetails {
>     cardName = "Flaming Sword",
>     cardSource = ThunderstoneBase,
>     cardType = FlamingSword,
>     cardIcon = CardIconVillage,
>     cardCount = 8,
>     cardClasses = [ClassWeapon,ClassEdged],
>     cardGold = Just 2,
>     cardLight = Just 1,
>     cardWeight = Just 5,
>     cardPrice = Just 5,
>     cardText = ["MAGIC ATTACK +3"],
>     cardGlossary =
>         ["Flaming Sword: You only gain the Light bonus if the "
>          ++ "Flaming Sword is equipped to a Hero."]
>     }

> villageDetails Goodberries = initCardDetails {
>     cardName = "Goodberries",
>     cardSource = ThunderstoneBase,
>     cardType = Goodberries,
>     cardIcon = CardIconVillage,
>     cardCount = 8,
>     cardClasses = [ClassItem,ClassFood,ClassMagic],
>     cardGold = Just 2,
>     cardVictoryPoints = Just 1,
>     cardPrice = Just 4,
>     cardText = ["DUNGEON: One Hero gains Strength +3 and ATTACK "
>                 ++ "becomes MAGIC ATTACK for that Hero."],
>     cardGlossary =
>         ["Goodberries: After the final Attack bonus of the Hero is "
>          ++ "calculated, its entire bonus becomes Magic Attack.  "
>          ++ "Militia are Heroes, and may benefit from this card."]
>     }

> villageDetails Hatchet = initCardDetails {
>     cardName = "Hatchet",
>     cardSource = ThunderstoneBase,
>     cardType = Hatchet,
>     cardIcon = CardIconVillage,
>     cardCount = 8,
>     cardClasses = [ClassWeapon,ClassEdged],
>     cardGold = Just 1,
>     cardWeight = Just 3,
>     cardPrice = Just 4,
>     cardText = ["ATTACK +3"],
>     cardGlossary = ["Hatchet: This is an Edged Weapon."]
>     }

> villageDetails Lantern = initCardDetails {
>     cardName = "Lantern",
>     cardSource = ThunderstoneBase,
>     cardType = Lantern,
>     cardIcon = CardIconVillage,
>     cardCount = 8,
>     cardClasses = [ClassItem,ClassLight],
>     cardGold = Just 2,
>     cardLight = Just 2,
>     cardPrice = Just 4,
>     cardGlossary =
>         ["Lantern: This Item always provides Light +2, even without "
>          ++ "a Hero present."]
>     }

> villageDetails LightstoneGem = initCardDetails {
>     cardName = "Lightstone Gem",
>     cardSource = ThunderstoneBase,
>     cardType = LightstoneGem,
>     cardIcon = CardIconVillage,
>     cardCount = 8,
>     cardClasses = [ClassItem,ClassLight,ClassMagic],
>     cardGold = Just 3,
>     cardLight = Just 3,
>     cardPrice = Just 6,
>     cardGlossary =
>         ["Lightstone Gem: This Item always provides Light +3, even "
>          ++ "without a Hero present."]
>     }

> villageDetails MagicalAura = initCardDetails {
>     cardName = "Magical Aura",
>     cardSource = ThunderstoneBase,
>     cardType = MagicalAura,
>     cardIcon = CardIconVillage,
>     cardCount = 8,
>     cardClasses = [ClassSpell],
>     cardGold = Just 2,
>     cardPrice = Just 4,
>     cardText = ["DUNGEON: All Weapons become Weight 0.  Draw one card."],
>     cardGlossary =
>         ["Magical Aura: When played with a Polearm, the Hero must still "
>          ++ "have a Strength of 8 or more to gain the +6 bonus.  You "
>          ++ "must draw a card when using this Dungeon Effect."]
>     }

> villageDetails Pawnbroker = initCardDetails {
>     cardName = "Pawnbroker",
>     cardSource = ThunderstoneBase,
>     cardType = Pawnbroker,
>     cardIcon = CardIconVillage,
>     cardCount = 8,
>     cardClasses = [ClassVillager],
>     cardPrice = Just 3,
>     cardText = ["VILLAGE: Destroy any card with a gold value to gain "
>                 ++ "its gold value plus 3 Gold.",
>                 "VILLAGE: Destroy this card to gain 2 Gold."],
>     cardGlossary =
>         ["Pawnbroker: You can destroy both the Pawnbroker and another "
>          ++ "card to produce X+5 gold in a single turn.  When you "
>          ++ "destroy a card with Pawnbroker, do not add its inherent "
>          ++ "gold value to your total gold that turn."]
>     }

> villageDetails Polearm = initCardDetails {
>     cardName = "Polearm",
>     cardSource = ThunderstoneBase,
>     cardType = Polearm,
>     cardIcon = CardIconVillage,
>     cardCount = 8,
>     cardClasses = [ClassWeapon,ClassEdged],
>     cardGold = Just 3,
>     cardWeight = Just 2,
>     cardPrice = Just 7,
>     cardText = ["ATTACK +2, or ATTACK +6 when attached to a Hero "
>                 ++ "with 8 or more Strength."],
>     cardGlossary =
>         ["Polearm: A Hero with a Strength of 2 can equip the Polearm "
>          ++ "for an Attack bonus of +2.  A Hero with a Strength of 8 "
>          ++ "or higher gains +6 instead."]
>     }

> villageDetails ShortSword = initCardDetails {
>     cardName = "Short Sword",
>     cardSource = ThunderstoneBase,
>     cardType = ShortSword,
>     cardIcon = CardIconVillage,
>     cardCount = 8,
>     cardClasses = [ClassWeapon,ClassEdged],
>     cardGold = Just 3,
>     cardWeight = Just 4,
>     cardPrice = Just 6,
>     cardText = ["ATTACK +4"],
>     cardGlossary = ["Short Sword: This is an Edged Weapon."]
>     }

> villageDetails Spear = initCardDetails {
>     cardName = "Spear",
>     cardSource = ThunderstoneBase,
>     cardType = Spear,
>     cardIcon = CardIconVillage,
>     cardCount = 8,
>     cardClasses = [ClassWeapon,ClassEdged],
>     cardGold = Just 2,
>     cardWeight = Just 4,
>     cardPrice = Just 4,
>     cardText = ["ATTACK +2"],
>     cardGlossary =
>         ["Spear: If you destroy (throw) the Spear, the Attack bonus "
>          ++ "increases by an additional +3, for a total of +5.  "
>          ++ "However, the Spear is still considered equipped for "
>          ++ "the entire battle, even if you use the effect."]
>     }

> villageDetails TownGuard = initCardDetails {
>     cardName = "Town Guard",
>     cardSource = ThunderstoneBase,
>     cardType = TownGuard,
>     cardIcon = CardIconVillage,
>     cardCount = 8,
>     cardClasses = [ClassVillager],
>     cardPrice = Just 3,
>     cardText = ["VILLAGE: Draw two cards.",
>                 "VILLAGE: Destroy this card to draw three additional "
>                 ++ "cards."],
>     cardGlossary =
>         ["Town Guard: Destroying this card allows you to draw a total "
>          ++ "of five extra cards."]
>     }

> villageDetails Trainer = initCardDetails {
>     cardName = "Trainer",
>     cardSource = ThunderstoneBase,
>     cardType = Trainer,
>     cardIcon = CardIconVillage,
>     cardCount = 8,
>     cardClasses = [ClassVillager],
>     cardPrice = Just 4,
>     cardText = ["VILLAGE: Destroy one Militia to gain 2 XP.",
>                 "VILLAGE: Destroy this card to gain 2 Gold."],
>     cardGlossary =
>         ["Trainer: Each Trainer in your hand may only destroy one "
>          ++ "Militia each turn."]
>     }

> villageDetails Warhammer = initCardDetails {
>     cardName = "Warhammer",
>     cardSource = ThunderstoneBase,
>     cardType = Warhammer,
>     cardIcon = CardIconVillage,
>     cardCount = 8,
>     cardClasses = [ClassWeapon,ClassBlunt],
>     cardGold = Just 2,
>     cardWeight = Just 5,
>     cardPrice = Just 4,
>     cardText = ["ATTACK +3",
>                 "Clerics gain an additional ATTACK +3 against "
>                 ++ "Doomknights and Undead."],
>     cardGlossary =
>         ["Warhammer: A Cleric attacking a Doomknight or Undead gains "
>          ++ "a total Attack bonus of +6."]
>     }

> villageDetails Ambrosia =
>     undefinedCardDetails Ambrosia WrathOfTheElements
> villageDetails AmuletOfPower =
>     undefinedCardDetails AmuletOfPower WrathOfTheElements
> villageDetails Blacksmith =
>     undefinedCardDetails Blacksmith WrathOfTheElements
> villageDetails Claymore =
>     undefinedCardDetails Claymore WrathOfTheElements
> villageDetails CreepingDeath =
>     undefinedCardDetails CreepingDeath WrathOfTheElements
> villageDetails CursedMace =
>     undefinedCardDetails CursedMace WrathOfTheElements
> villageDetails ForesightElixir =
>     undefinedCardDetails ForesightElixir WrathOfTheElements
> villageDetails IllusoryBlade =
>     undefinedCardDetails IllusoryBlade WrathOfTheElements
> villageDetails MagiStaff =
>     undefinedCardDetails MagiStaff WrathOfTheElements
> villageDetails MagicMissile =
>     undefinedCardDetails MagicMissile WrathOfTheElements
> villageDetails Sage =
>     undefinedCardDetails Sage WrathOfTheElements
> villageDetails Shortbow =
>     undefinedCardDetails Shortbow WrathOfTheElements
> villageDetails TavernBrawl =
>     undefinedCardDetails TavernBrawl WrathOfTheElements
> villageDetails TaxCollector =
>     undefinedCardDetails TaxCollector WrathOfTheElements

> villageDetails BlessedHammer =
>     undefinedCardDetails BlessedHammer DoomgateLegion
> villageDetails BorderGuard =
>     undefinedCardDetails BorderGuard DoomgateLegion
> villageDetails Cyclone =
>     undefinedCardDetails Cyclone DoomgateLegion
> villageDetails DivineStaff =
>     undefinedCardDetails DivineStaff DoomgateLegion
> villageDetails DoomgateSquire =
>     undefinedCardDetails DoomgateSquire DoomgateLegion
> villageDetails FlaskOfOil =
>     undefinedCardDetails FlaskOfOil DoomgateLegion
> villageDetails FortuneTeller =
>     undefinedCardDetails FortuneTeller DoomgateLegion
> villageDetails Glowberries =
>     undefinedCardDetails Glowberries DoomgateLegion
> villageDetails GreedBlade =
>     undefinedCardDetails GreedBlade DoomgateLegion
> villageDetails PiousChaplain =
>     undefinedCardDetails PiousChaplain DoomgateLegion
> villageDetails SoulJar =
>     undefinedCardDetails SoulJar DoomgateLegion
> villageDetails SpiritBlast =
>     undefinedCardDetails SpiritBlast DoomgateLegion
> villageDetails SpiritHunter =
>     undefinedCardDetails SpiritHunter DoomgateLegion

> villageDetails BluefireStaff =
>     undefinedCardDetails BluefireStaff Dragonspire
> villageDetails BurntOffering =
>     undefinedCardDetails BurntOffering Dragonspire
> villageDetails ChieftainsDrum =
>     undefinedCardDetails ChieftainsDrum Dragonspire
> villageDetails FrostBolt =
>     undefinedCardDetails FrostBolt Dragonspire
> villageDetails FrostGiantAxe =
>     undefinedCardDetails FrostGiantAxe Dragonspire
> villageDetails GuardianBlade =
>     undefinedCardDetails GuardianBlade Dragonspire
> villageDetails Guide =
>     undefinedCardDetails Guide Dragonspire
> villageDetails Polymorph =
>     undefinedCardDetails Polymorph Dragonspire
> villageDetails Quartermaster =
>     undefinedCardDetails Quartermaster Dragonspire
> villageDetails RecurveBow =
>     undefinedCardDetails RecurveBow Dragonspire
> villageDetails Scout =
>     undefinedCardDetails Scout Dragonspire
> villageDetails Silverstorm =
>     undefinedCardDetails Silverstorm Dragonspire
> villageDetails Skullbreaker =
>     undefinedCardDetails Skullbreaker Dragonspire
> villageDetails SoulGem =
>     undefinedCardDetails SoulGem Dragonspire
> villageDetails SpoiledFood =
>     undefinedCardDetails SpoiledFood Dragonspire
> villageDetails ThunderRing =
>     undefinedCardDetails ThunderRing Dragonspire
> villageDetails TorynGuantlet =
>     undefinedCardDetails TorynGuantlet Dragonspire
> villageDetails Trader =
>     undefinedCardDetails Trader Dragonspire





> dungeonFeatureDetails :: DungeonFeatureCard -> CardDetails DungeonFeatureType
> dungeonFeatureDetails = undefined





> guardianDetails :: GuardianCard -> CardDetails DungeonFeatureType
> guardianDetails = undefined

> cardsOfType :: (Bounded card, Enum card, Eq cardType) => 
>                (card -> CardDetails cardType) -> cardType
>             -> [card]
> cardsOfType details ofType = concatMap countOff cards
>   where
>     cards =
>         filter ((== ofType) . cardType . details) [minBound .. maxBound]
>     countOff card = replicate (cardCount $ details card) card




TEMPORARY CODE:

> undefinedHeroDetails cardType source level = initCardDetails {
>     cardSource = source,
>     cardType = cardType,
>     cardIcon = CardIconHero level
>     }

> undefinedCardDetails cardType source = initCardDetails {
>     cardSource = source,
>     cardType = cardType
>     }
