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
>   deriving (Bounded,Enum,Eq,Read,Show)


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

>   deriving (Bounded,Enum,Eq,Read,Show)

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
>   | MonsterDungeonFeature

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

>   deriving (Bounded,Enum,Eq,Read,Show)

> data DungeonFeatureType =

>     DungeonFeaturePickTwo

Wrath of the Elements:

>   | DungeonFeatureGuardian
>   | DungeonFeatureTrapDire
>   | DungeonFeatureTrapDeath

Doomgate Legion:

>   | DungeonFeatureAmuletTreasures
>   | DungeonFeatureUlbricksTreasures

Dragonspire:

>   | DungeonFeatureFigurineTreasure
>   | DungeonFeatureSetting
>   | DungeonFeatureTrapDraconic

Promotional:

>   deriving (Bounded,Enum,Eq,Read,Show)



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

>   deriving (Bounded,Enum,Eq,Read,Show)

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

>   deriving (Bounded,Enum,Eq,Read,Show)

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
>   | VerdanTroubadour

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

>   deriving (Bounded,Enum,Eq,Read,Show)

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
>   | ShortBow
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
>   | GlowBerries
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
>   | TorynGauntlet
>   | Trader

>   deriving (Bounded,Enum,Eq,Read,Show)

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

>   deriving (Bounded,Enum,Eq,Read,Show)

> data DungeonFeatureCard =

Wrath of the Elements:

>     TheCage
>   | DeliriumPoison
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

>   deriving (Bounded,Enum,Eq,Read,Show)

> data DiseaseCard =

Base set:

>     Disease

Doomgate Legion:

>   | BalefulPlague
>   | Fatigue
>   | Leprosy
>   | Malaise
>   | ThundersCurse

>   deriving (Bounded,Enum,Eq,Read,Show)

> data SettingCard =
>     SettingBarrowsdale
>   | SettingDoomgate
>   | SettingDragonspire
>   | SettingDreadwatch
>   | SettingFeaynSwamp
>   | SettingGrimhold
>   | SettingRegianCove
>   deriving (Bounded,Enum,Eq,Read,Show)

> data CardClass =
>     ClassAbyssal
>   | ClassAmuletTreasure
>   | ClassAnimal
>   | ClassArcher
>   | ClassBandit
>   | ClassBlack
>   | ClassBlue
>   | ClassBlunt
>   | ClassBow
>   | ClassCleric
>   | ClassCultist
>   | ClassDarkEnchanted
>   | ClassDeath
>   | ClassDire
>   | ClassDoom
>   | ClassDoomknight
>   | ClassDisease
>   | ClassDraconic
>   | ClassDragon
>   | ClassEdged
>   | ClassElemental
>   | ClassEnchanted
>   | ClassEvilDruid
>   | ClassFamiliar
>   | ClassFighter
>   | ClassFigurine
>   | ClassFire
>   | ClassFood
>   | ClassGiant
>   | ClassGreen
>   | ClassGolem
>   | ClassGuardian
>   | ClassHero
>   | ClassHorde
>   | ClassHumanoid
>   | ClassHydra
>   | ClassItem
>   | ClassLich
>   | ClassLight
>   | ClassMagic
>   | ClassMercenary
>   | ClassMilitia
>   | ClassNature
>   | ClassOoze
>   | ClassOrc
>   | ClassPain
>   | ClassPlague
>   | ClassRed
>   | ClassSpecial
>   | ClassSpell
>   | ClassSpirit
>   | ClassStormwraith
>   | ClassTheSwarm
>   | ClassThief
>   | ClassThunderspawn
>   | ClassThunderstone
>   | ClassTrap
>   | ClassTreasure
>   | ClassUlbricksTreasure
>   | ClassUndead
>   | ClassVillager
>   | ClassWeapon
>   | ClassWhite
>   | ClassWizard
>   deriving (Bounded,Enum,Eq,Read,Show)
