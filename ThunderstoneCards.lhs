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

> data CardDetails cardType stats = CardDetails {
>     cardName :: String,
>     cardSource :: Source,
>     cardType :: cardType,
>     cardCount :: Int,
>     cardClasses :: [CardClass],
>     cardGold :: Int,
>     cardLight :: Int,
>     cardVictoryPoints :: Int,
>     cardStats :: stats
>     }



Thunderstone cards:

> thunderstoneDetails :: ThunderstoneCard
>         -> CardDetails ThunderstoneCard ()

> thunderstoneDetails StoneOfMystery = CardDetails {
>     cardName = "Stone of Mystery",

...and the thunder of the wind shouts back.

>     cardSource = ThunderstoneBase,
>     cardType = StoneOfMystery,
>     cardCount = 1,
>     cardClasses = [ClassThunderstone],
>     cardGold = 0,
>     cardLight = 0,
>     cardVictoryPoints = 3,
>     cardStats = ()
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

> data HeroStats = HeroStats {
>     heroStrength :: Int,
>     heroLevel :: Int,
>     heroPrice :: Int,
>     heroUpgrade :: (Int,[HeroCard])
>     }

> heroDetails :: HeroCard -> CardDetails HeroType HeroStats

> heroDetails Militia = CardDetails {
>     cardName = "Militia",

ATTACK +1

>     cardSource = ThunderstoneBase,
>     cardType = HeroMilitia,
>     cardCount = 30,
>     cardClasses = [ClassMilitia,ClassHero],
>     cardGold = 0,
>     cardLight = 0,
>     cardVictoryPoints = 0,
>     cardStats = HeroStats {
>         heroStrength = 2,
>         heroLevel = 0,
>         heroPrice = 0,
>         heroUpgrade = (3,levelOneHeroes)
>         }
>     }
>   where
>     levelOneHeroes = filter levelOne [minBound .. maxBound]
>     levelOne card = (heroLevel $ cardStats $ heroDetails card) == 1

Amazon: This Hero's Dungeon Effect is an Attack that is in addition
to the Amazon's normal Attack.

> heroDetails AmazonArcher = CardDetails {
>     cardName = "Amazon Archer",

ATTACK +1

Additional ATTACK +2 at Rank 2 or 3.

>     cardSource = ThunderstoneBase,
>     cardType = HeroAmazon,
>     cardCount = 6,
>     cardClasses = [ClassFighter,ClassArcher],
>     cardGold = 0,
>     cardLight = 0,
>     cardVictoryPoints = 0,
>     cardStats = HeroStats {
>         heroStrength = 4,
>         heroLevel = 1,
>         heroPrice = 6,
>         heroUpgrade = (2,[AmazonHuntress])
>         }
>     }

> heroDetails AmazonHuntress = CardDetails {
>     cardName = "Amazon Huntress",

ATTACK +2

Additional ATTACK +3 at Rank 2 or 3.

>     cardSource = ThunderstoneBase,
>     cardType = HeroAmazon,
>     cardCount = 4,
>     cardClasses = [ClassFighter,ClassArcher],
>     cardGold = 0,
>     cardLight = 0,
>     cardVictoryPoints = 0,
>     cardStats = HeroStats {
>         heroStrength = 5,
>         heroLevel = 2,
>         heroPrice = 9,
>         heroUpgrade = (3,[AmazonQueen])
>         }
>     }

> heroDetails AmazonQueen = CardDetails {
>     cardName = "Amazon Queen",

ATTACK +2

Additional ATTACK +4 at Rank 2 or 3.

>     cardSource = ThunderstoneBase,
>     cardType = HeroAmazon,
>     cardCount = 2,
>     cardClasses = [ClassFighter,ClassArcher],
>     cardGold = 0,
>     cardLight = 0,
>     cardVictoryPoints = 2,
>     cardStats = HeroStats {
>         heroStrength = 6,
>         heroLevel = 3,
>         heroPrice = 11,
>         heroUpgrade = (0,[])
>         }
>     }

Chalice Quester and Defender: You man continue to destroy Disease
cards and draw new cards until you choose which Monster to attack.

Chalice Defender: Only Items (not Weapons) that provide a Light
bonus increase the Defender's Attack Value.

Chalic Paladin: You may purchase any one Village card (including
Basic and Hero cards) from the Village after a victorious battle,
using the gold in your hand.

> heroDetails ChaliceQuester = CardDetails {
>     cardName = "Chalice Quester",

ATTACK +2

REPEAT DUNGEON: Destroy one Disease to draw one card.

>     cardSource = ThunderstoneBase,
>     cardType = HeroChalice,
>     cardCount = 6,
>     cardClasses = [ClassFighter,ClassCleric],
>     cardGold = 0,
>     cardLight = 0,
>     cardVictoryPoints = 0,
>     cardStats = HeroStats {
>         heroStrength = 5,
>         heroLevel = 1,
>         heroPrice = 7,
>         heroUpgrade = (2,[ChaliceDefender])
>         }
>     }

> heroDetails ChaliceDefender = CardDetails {
>     cardName = "Chalice Defender",

ATTACK +3

DUNGEON: ATTACK +1 for each Item that produces Light.

DUNGEON: Draw one card.

REPEAT DUNGEON: Destroy one Disease to draw one card.

>     cardSource = ThunderstoneBase,
>     cardType = HeroChalice,
>     cardCount = 4,
>     cardClasses = [ClassFighter,ClassCleric],
>     cardGold = 0,
>     cardLight = 0,
>     cardVictoryPoints = 0,
>     cardStats = HeroStats {
>         heroStrength = 6,
>         heroLevel = 2,
>         heroPrice = 10,
>         heroUpgrade = (3,[ChalicePaladin])
>         }
>     }

> heroDetails ChalicePaladin = CardDetails {
>     cardName = "Chalice Paladin",

ATTACK +4

DUNGEON: Draw one card.

Spoils (Village).

>     cardSource = ThunderstoneBase,
>     cardType = HeroChalice,
>     cardCount = 2,
>     cardClasses = [ClassFighter,ClassCleric],
>     cardGold = 0,
>     cardLight = 0,
>     cardVictoryPoints = 2,
>     cardStats = HeroStats {
>         heroStrength = 7,
>         heroLevel = 3,
>         heroPrice = 12,
>         heroUpgrade = (0,[])
>         }
>     }

Dwarf Guardian: His total Attack Value if an Edged Weapon is equipped
is +4.  This bonus is part of the Dwarf's ability which he retains even
if the Weapon later becomes useless (due to a Monster's Battle Effect,
for instance).

Dwarf Janissary: If revealed during a Dungeon action, you may purchase
one Weapon card from the Village after a victorious battle, using the
gold in your hand.  His total Attack Vlaue if an Edged Weapon is
equipped is +6.

Dwarf Sentinel: His total Attack Value with an Edged Weapon equipped
is +8.

> heroDetails DwarfGuardian = CardDetails {
>     cardName = "Dwarf Guardian",

ATTACK +1

Additional ATTACK +3 when equipped with an Edged Weapon.

>     cardSource = ThunderstoneBase,
>     cardType = HeroDwarf,
>     cardCount = 6,
>     cardClasses = [ClassFighter],
>     cardGold = 0,
>     cardLight = 0,
>     cardVictoryPoints = 0,
>     cardStats = HeroStats {
>         heroStrength = 5,
>         heroLevel = 1,
>         heroPrice = 6,
>         heroUpgrade = (2,[DwarfJanissary])
>         }
>     }

> heroDetails DwarfJanissary = CardDetails {
>     cardName = "Dwarf Janissary",

ATTACK +2

Additional ATTACK +4 when equipped with an Edged Weapon.

Spoils (Weapon).

>     cardSource = ThunderstoneBase,
>     cardType = HeroDwarf,
>     cardCount = 4,
>     cardClasses = [ClassFighter],
>     cardGold = 0,
>     cardLight = 0,
>     cardVictoryPoints = 0,
>     cardStats = HeroStats {
>         heroStrength = 6,
>         heroLevel = 2,
>         heroPrice = 9,
>         heroUpgrade = (3,[DwarfSentinel])
>         }
>     }

> heroDetails DwarfSentinel = CardDetails {
>     cardName = "Dwarf Sentinel",

ATTACK +3

Additional ATTACK +5 when equipped with an Edged Weapon.

>     cardSource = ThunderstoneBase,
>     cardType = HeroDwarf,
>     cardCount = 2,
>     cardClasses = [ClassFighter],
>     cardGold = 0,
>     cardLight = 0,
>     cardVictoryPoints = 2,
>     cardStats = HeroStats {
>         heroStrength = 8,
>         heroLevel = 3,
>         heroPrice = 12,
>         heroUpgrade = (0,[])
>         }
>     }

Elf Sorcerer/Archmage: When a Monster is returned to the bottom of
the monster deck, refill the Dungeon Hall.  If this results in a
Breach effect, resolve it immediately.  If the Thunderstone moves
to Rank 1 of the Dungeon Hall, the game ends immediately; you do
not collect the Thunderstone.

> heroDetails ElfWizard = CardDetails {
>     cardName = "Elf Wizard",

MAGIC ATTACK +2

>     cardSource = ThunderstoneBase,
>     cardType = HeroElf,
>     cardCount = 6,
>     cardClasses = [ClassWizard],
>     cardGold = 0,
>     cardLight = 1,
>     cardVictoryPoints = 0,
>     cardStats = HeroStats {
>         heroStrength = 3,
>         heroLevel = 1,
>         heroPrice = 5,
>         heroUpgrade = (2,[ElfSorcerer])
>         }
>     }

> heroDetails ElfSorcerer = CardDetails {
>     cardName = "Elf Sorcerer",

MAGIC ATTACK +3

You may return one Monster to the bottom of the deck after
defeating a monster.  (Refill the hall.)

>     cardSource = ThunderstoneBase,
>     cardType = HeroElf,
>     cardCount = 4,
>     cardClasses = [ClassWizard],
>     cardGold = 0,
>     cardLight = 2,
>     cardVictoryPoints = 0,
>     cardStats = HeroStats {
>         heroStrength = 3,
>         heroLevel = 2,
>         heroPrice = 8,
>         heroUpgrade = (2,[ElfArchmage])
>         }
>     }

> heroDetails ElfArchmage = CardDetails {
>     cardName = "Elf Archmage",

MAGIC ATTACK +4

DUNGEON: You may return one Monster to the bottom of the deck and
refill the hall before the beginning of a battle.

>     cardSource = ThunderstoneBase,
>     cardType = HeroElf,
>     cardCount = 2,
>     cardClasses = [ClassWizard],
>     cardGold = 0,
>     cardLight = 2,
>     cardVictoryPoints = 2,
>     cardStats = HeroStats {
>         heroStrength = 4,
>         heroLevel = 3,
>         heroPrice = 10,
>         heroUpgrade = (0,[])
>         }
>     }

Feayn: If a Dungeon Actions causes you to attack a Monster in
Rank 1, do not add the Feayn's Attack bonus to your Attack Value.
If Feayn does not attack, his Light bonus is lost.

> heroDetails FeaynArcher = CardDetails {
>     cardName = "Feayn Archer",

Cannot attack Rank 1.

ATTACK +2

>     cardSource = ThunderstoneBase,
>     cardType = HeroFeayn,
>     cardCount = 6,
>     cardClasses = [ClassFighter,ClassArcher],
>     cardGold = 0,
>     cardLight = 1,
>     cardVictoryPoints = 0,
>     cardStats = HeroStats {
>         heroStrength = 4,
>         heroLevel = 1,
>         heroPrice = 7,
>         heroUpgrade = (2,[FeaynMarksman])
>         }
>     }

> heroDetails FeaynMarksman = CardDetails {
>     cardName = "Feayn Marksman",

Cannot attack Rank 1.

ATTACK +3

>     cardSource = ThunderstoneBase,
>     cardType = HeroFeayn,
>     cardCount = 4,
>     cardClasses = [ClassFighter,ClassArcher],
>     cardGold = 0,
>     cardLight = 1,
>     cardVictoryPoints = 0,
>     cardStats = HeroStats {
>         heroStrength = 5,
>         heroLevel = 2,
>         heroPrice = 10,
>         heroUpgrade = (3,[FeaynSniper])
>         }
>     }

> heroDetails FeaynSniper = CardDetails {
>     cardName = "Feayn Sniper",

Cannot attack Rank 1.

ATTACK +4

Gain +1 XP if you defeat a Monster in Rank 3.

>     cardSource = ThunderstoneBase,
>     cardType = HeroFeayn,
>     cardCount = 2,
>     cardClasses = [ClassFighter,ClassArcher],
>     cardGold = 0,
>     cardLight = 2,
>     cardVictoryPoints = 2,
>     cardStats = HeroStats {
>         heroStrength = 6,
>         heroLevel = 3,
>         heroPrice = 12,
>         heroUpgrade = (0,[])
>         }
>     }

> heroDetails LoriggThief = CardDetails {
>     cardName = "Lorigg Thief",

ATTACK +1

>     cardSource = ThunderstoneBase,
>     cardType = HeroLorigg,
>     cardCount = 6,
>     cardClasses = [ClassThief],
>     cardGold = 2,
>     cardLight = 1,
>     cardVictoryPoints = 0,
>     cardStats = HeroStats {
>         heroStrength = 4,
>         heroLevel = 1,
>         heroPrice = 5,
>         heroUpgrade = (2,[LoriggRogue])
>         }
>     }

> heroDetails LoriggRogue = CardDetails {
>     cardName = "Lorigg Rogue",

ATTACK +2

DUNGEON: All other players discard one card.

>     cardSource = ThunderstoneBase,
>     cardType = HeroLorigg,
>     cardCount = 4,
>     cardClasses = [ClassThief],
>     cardGold = 3,
>     cardLight = 1,
>     cardVictoryPoints = 0,
>     cardStats = HeroStats {
>         heroStrength = 4,
>         heroLevel = 2,
>         heroPrice = 8,
>         heroUpgrade = (3,[LoriggOutlaw])
>         }
>     }

> heroDetails LoriggOutlaw = CardDetails {
>     cardName = "Lorigg Outlaw",

ATTACK +2

DUNGEON: All other players discard one card.

>     cardSource = ThunderstoneBase,
>     cardType = HeroLorigg,
>     cardCount = 2,
>     cardClasses = [ClassThief],
>     cardGold = 4,
>     cardLight = 2,
>     cardVictoryPoints = 1,
>     cardStats = HeroStats {
>         heroStrength = 5,
>         heroLevel = 3,
>         heroPrice = 10,
>         heroUpgrade = (0,[])
>         }
>     }

> heroDetails OutlandsWarrior = CardDetails {
>     cardName = "Outlands Warrior",

ATTACK +3

DUNGEON: Destroy one Food for an additional ATTACK +3.

>     cardSource = ThunderstoneBase,
>     cardType = HeroOutlands,
>     cardCount = 6,
>     cardClasses = [ClassFighter],
>     cardGold = 0,
>     cardLight = 0,
>     cardVictoryPoints = 0,
>     cardStats = HeroStats {
>         heroStrength = 7,
>         heroLevel = 1,
>         heroPrice = 8,
>         heroUpgrade = (2,[OutlandsSlayer])
>         }
>     }

> heroDetails OutlandsSlayer = CardDetails {
>     cardName = "Outlands Slayer",

ATTACK +5

DUNGEON: Gain +1 ATTACK for each Monster card revealed from your hand.

REPEAT DUNGEON: Destroy one Food for an additional ATTACK +3.

>     cardSource = ThunderstoneBase,
>     cardType = HeroOutlands,
>     cardCount = 4,
>     cardClasses = [ClassFighter],
>     cardGold = 0,
>     cardLight = 0,
>     cardVictoryPoints = 0,
>     cardStats = HeroStats {
>         heroStrength = 8,
>         heroLevel = 2,
>         heroPrice = 11,
>         heroUpgrade = (3,[OutlandsKhan])
>         }
>     }

> heroDetails OutlandsKhan = CardDetails {
>     cardName = "Outlands Khan",

ATTACK +7

DUNGEON: ATTACK +2 for each Monster card revealed from your hand.

>     cardSource = ThunderstoneBase,
>     cardType = HeroOutlands,
>     cardCount = 2,
>     cardClasses = [ClassFighter],
>     cardGold = 0,
>     cardLight = 0,
>     cardVictoryPoints = 3,
>     cardStats = HeroStats {
>         heroStrength = 8,
>         heroLevel = 3,
>         heroPrice = 13,
>         heroUpgrade = (0,[])
>         }
>     }

> heroDetails RedbladeKiller = CardDetails {
>     cardName = "Redblade Killer",

ATTACK +2

>     cardSource = ThunderstoneBase,
>     cardType = HeroRedblade,
>     cardCount = 6,
>     cardClasses = [ClassFighter,ClassThief],
>     cardGold = 1,
>     cardLight = 0,
>     cardVictoryPoints = 0,
>     cardStats = HeroStats {
>         heroStrength = 5,
>         heroLevel = 1,
>         heroPrice = 5,
>         heroUpgrade = (2,[RedbladePoisoner])
>         }
>     }

> heroDetails RedbladePoisoner = CardDetails {
>     cardName = "Redblade Poisoner",

ATTACK +3

DUNGEON: All other players discard one card.

>     cardSource = ThunderstoneBase,
>     cardType = HeroRedblade,
>     cardCount = 4,
>     cardClasses = [ClassFighter,ClassThief],
>     cardGold = 2,
>     cardLight = 0,
>     cardVictoryPoints = 0,
>     cardStats = HeroStats {
>         heroStrength = 5,
>         heroLevel = 2,
>         heroPrice = 8,
>         heroUpgrade = (3,[RedbladeAssassin])
>         }
>     }

> heroDetails RedbladeAssassin = CardDetails {
>     cardName = "Redblade Assassin",

ATTACK +4

DUNGEON: All other players discard one Hero or two cards.

>     cardSource = ThunderstoneBase,
>     cardType = HeroRedblade,
>     cardCount = 2,
>     cardClasses = [ClassFighter,ClassThief],
>     cardGold = 2,
>     cardLight = 0,
>     cardVictoryPoints = 2,
>     cardStats = HeroStats {
>         heroStrength = 6,
>         heroLevel = 3,
>         heroPrice = 9,
>         heroUpgrade = (0,[])
>         }
>     }

> heroDetails RegianCleric = CardDetails {
>     cardName = "Regian Cleric",

MAGIC ATTACK +1

REPEAT DUNGEON: Destroy one Disease to draw one card.

>     cardSource = ThunderstoneBase,
>     cardType = HeroRegian,
>     cardCount = 6,
>     cardClasses = [ClassCleric],
>     cardGold = 0,
>     cardLight = 0,
>     cardVictoryPoints = 0,
>     cardStats = HeroStats {
>         heroStrength = 4,
>         heroLevel = 1,
>         heroPrice = 5,
>         heroUpgrade = (2,[RegianPriest])
>         }
>     }

> heroDetails RegianPriest = CardDetails {
>     cardName = "Regian Priest",

MAGIC ATTACK +2

DUNGEON: Draw one card.

REPEAT DUNGEON: Destroy one Disease to draw one card.

>     cardSource = ThunderstoneBase,
>     cardType = HeroRegian,
>     cardCount = 4,
>     cardClasses = [ClassCleric],
>     cardGold = 0,
>     cardLight = 0,
>     cardVictoryPoints = 0,
>     cardStats = HeroStats {
>         heroStrength = 4,
>         heroLevel = 2,
>         heroPrice = 8,
>         heroUpgrade = (3,[RegianBishop])
>         }
>     }

> heroDetails RegianBishop = CardDetails {
>     cardName = "Regian Bishop",

MAGIC ATTACK +3

DUNGEON: Draw two cards.

REPEAT DUNGEON: Destroy one Disease to draw one card.

>     cardSource = ThunderstoneBase,
>     cardType = HeroRegian,
>     cardCount = 2,
>     cardClasses = [ClassCleric],
>     cardGold = 0,
>     cardLight = 0,
>     cardVictoryPoints = 2,
>     cardStats = HeroStats {
>         heroStrength = 5,
>         heroLevel = 3,
>         heroPrice = 11,
>         heroUpgrade = (0,[])
>         }
>     }

> heroDetails SelurinMagician = CardDetails {
>     cardName = "Selurin Magician",

MAGIC ATTACK +2

All Items and Magic Attack Spells gain MAGIC ATTACK +1.

>     cardSource = ThunderstoneBase,
>     cardType = HeroSelurin,
>     cardCount = 6,
>     cardClasses = [ClassWizard],
>     cardGold = 0,
>     cardLight = 0,
>     cardVictoryPoints = 0,
>     cardStats = HeroStats {
>         heroStrength = 2,
>         heroLevel = 1,
>         heroPrice = 8,
>         heroUpgrade = (2,[SelurinWarlock])
>         }
>     }

> heroDetails SelurinWarlock = CardDetails {
>     cardName = "Selurin Warlock",

MAGIC ATTACK +2

Total MAGIC ATTACK x2* (apply last)

>     cardSource = ThunderstoneBase,
>     cardType = HeroSelurin,
>     cardCount = 4,
>     cardClasses = [ClassWizard],
>     cardGold = 0,
>     cardLight = 0,
>     cardVictoryPoints = 0,
>     cardStats = HeroStats {
>         heroStrength = 2,
>         heroLevel = 2,
>         heroPrice = 10,
>         heroUpgrade = (3,[SelurinTheurge])
>         }
>     }

> heroDetails SelurinTheurge = CardDetails {
>     cardName = "Selurin Theurge",

MAGIC ATTACK +2
Total MAGIC ATTACK x2* (apply last)
DUNGEON: Each player dicards one Hero or shows they have none.
You may borrow one of those discarded Heroes for the battle, returning
it at the end.

>     cardSource = ThunderstoneBase,
>     cardType = HeroSelurin,
>     cardCount = 2,
>     cardClasses = [ClassWizard],
>     cardGold = 0,
>     cardLight = 0,
>     cardVictoryPoints = 1,
>     cardStats = HeroStats {
>         heroStrength = 3,
>         heroLevel = 3,
>         heroPrice = 13,
>         heroUpgrade = (0,[])
>         }
>     }

> heroDetails ThyrianSquire = CardDetails {
>     cardName = "Thyrian Squire",

ATTACK +2

DUNGEON: Destroy one Food for additional ATTACK +2.

>     cardSource = ThunderstoneBase,
>     cardType = HeroThyrian,
>     cardCount = 6,
>     cardClasses = [ClassFighter],
>     cardGold = 0,
>     cardLight = 0,
>     cardVictoryPoints = 0,
>     cardStats = HeroStats {
>         heroStrength = 6,
>         heroLevel = 1,
>         heroPrice = 7,
>         heroUpgrade = (2,[ThyrianKnight])
>         }
>     }

> heroDetails ThyrianKnight = CardDetails {
>     cardName = "Thyrian Knight",

ATTACK +4

All Militia gain ATTACK +1.

DUNGEON: Destroy one Food for additional ATTACK +2.

>     cardSource = ThunderstoneBase,
>     cardType = HeroThyrian,
>     cardCount = 4,
>     cardClasses = [ClassFighter],
>     cardGold = 0,
>     cardLight = 0,
>     cardVictoryPoints = 0,
>     cardStats = HeroStats {
>         heroStrength = 8,
>         heroLevel = 2,
>         heroPrice = 9,
>         heroUpgrade = (3,[ThyrianLord])
>         }
>     }

> heroDetails ThyrianLord = CardDetails {
>     cardName = "Thyrian Lord",

ATTACK +4

All Heroes other than Fighters gain ATTACK +2.

DUNGEON: Destroy one Food to playce one Monster from the hall
work 1 or 2 VP into your discard pile.  Refill the hall.

>     cardSource = ThunderstoneBase,
>     cardType = HeroThyrian,
>     cardCount = 2,
>     cardClasses = [ClassFighter],
>     cardGold = 0,
>     cardLight = 0,
>     cardVictoryPoints = 2,
>     cardStats = HeroStats {
>         heroStrength = 9,
>         heroLevel = 3,
>         heroPrice = 11,
>         heroUpgrade = (0,[])
>         }
>     }

> heroDetails BlindNeophyte =
>     undefinedCardDetails HeroBlind WrathOfTheElements
> heroDetails BlindMonk =
>     undefinedCardDetails HeroBlind WrathOfTheElements
> heroDetails BlindGrandmaster =
>     undefinedCardDetails HeroBlind WrathOfTheElements
> heroDetails DiinIllusionist =
>     undefinedCardDetails HeroDiin WrathOfTheElements
> heroDetails DiinBeguiler =
>     undefinedCardDetails HeroDiin WrathOfTheElements
> heroDetails DiinEnchanter =
>     undefinedCardDetails HeroDiin WrathOfTheElements
> heroDetails DivineHealer =
>     undefinedCardDetails HeroDivine WrathOfTheElements
> heroDetails DivineChaplain =
>     undefinedCardDetails HeroDivine WrathOfTheElements
> heroDetails DivineProphet =
>     undefinedCardDetails HeroDivine WrathOfTheElements
> heroDetails GanglandThug =
>     undefinedCardDetails HeroGangland WrathOfTheElements
> heroDetails GanglandHeavy =
>     undefinedCardDetails HeroGangland WrathOfTheElements
> heroDetails GanglandCrook =
>     undefinedCardDetails HeroGangland WrathOfTheElements
> heroDetails GohlenTrapper =
>     undefinedCardDetails HeroGohlen WrathOfTheElements
> heroDetails GohlenTracker =
>     undefinedCardDetails HeroGohlen WrathOfTheElements
> heroDetails GohlenHunter =
>     undefinedCardDetails HeroGohlen WrathOfTheElements
> heroDetails RunespawnAdept =
>     undefinedCardDetails HeroRunespawn WrathOfTheElements
> heroDetails RunespawnSiren =
>     undefinedCardDetails HeroRunespawn WrathOfTheElements
> heroDetails RunespawnWitch =
>     undefinedCardDetails HeroRunespawn WrathOfTheElements
> heroDetails TorynScrapper =
>     undefinedCardDetails HeroToryn WrathOfTheElements
> heroDetails TorynDuelist =
>     undefinedCardDetails HeroToryn WrathOfTheElements
> heroDetails TorynGladiator =
>     undefinedCardDetails HeroToryn WrathOfTheElements

> heroDetails DeepMiner =
>     undefinedCardDetails HeroDeep DoomgateLegion
> heroDetails DeepDigger =
>     undefinedCardDetails HeroDeep DoomgateLegion
> heroDetails DeepWrecker =
>     undefinedCardDetails HeroDeep DoomgateLegion
> heroDetails DrunariOrphan =
>     undefinedCardDetails HeroDrunari DoomgateLegion
> heroDetails DrunariVagabond =
>     undefinedCardDetails HeroDrunari DoomgateLegion
> heroDetails DrunariGypsy =
>     undefinedCardDetails HeroDrunari DoomgateLegion
> heroDetails SidheNatural =
>     undefinedCardDetails HeroSidhe DoomgateLegion
> heroDetails SidheDruid =
>     undefinedCardDetails HeroSidhe DoomgateLegion
> heroDetails SidheSpirit =
>     undefinedCardDetails HeroSidhe DoomgateLegion
> heroDetails SlynnBowman =
>     undefinedCardDetails HeroSlynn DoomgateLegion
> heroDetails SlynnBowmaster =
>     undefinedCardDetails HeroSlynn DoomgateLegion
> heroDetails SlynnLongbowman =
>     undefinedCardDetails HeroSlynn DoomgateLegion
> heroDetails TempestAvenger =
>     undefinedCardDetails HeroTempest DoomgateLegion
> heroDetails TempestReaver =
>     undefinedCardDetails HeroTempest DoomgateLegion
> heroDetails TempestWarden =
>     undefinedCardDetails HeroTempest DoomgateLegion
> heroDetails TholisMedium =
>     undefinedCardDetails HeroTholis DoomgateLegion
> heroDetails TholisClairvoyant =
>     undefinedCardDetails HeroTholis DoomgateLegion
> heroDetails TholisOracle =
>     undefinedCardDetails HeroTholis DoomgateLegion
> heroDetails VerdanMinstrel =
>     undefinedCardDetails HeroVerdan DoomgateLegion
> heroDetails VerdanBard =
>     undefinedCardDetails HeroVerdan DoomgateLegion
> heroDetails VerdanTrouadour =
>     undefinedCardDetails HeroVerdan DoomgateLegion

> heroDetails PhalanxFootman =
>     undefinedCardDetails HeroPhalanx Dragonspire
> heroDetails PhalanxOfficer =
>     undefinedCardDetails HeroPhalanx Dragonspire
> heroDetails BelzurCurate =
>     undefinedCardDetails HeroBelzur Dragonspire
> heroDetails BelzurBishop =
>     undefinedCardDetails HeroBelzur Dragonspire
> heroDetails BelzurCardinal =
>     undefinedCardDetails HeroBelzur Dragonspire
> heroDetails CabalAstrologer =
>     undefinedCardDetails HeroCabal Dragonspire
> heroDetails CabalSage =
>     undefinedCardDetails HeroCabal Dragonspire
> heroDetails CabalMaster =
>     undefinedCardDetails HeroCabal Dragonspire
> heroDetails ChulianRat =
>     undefinedCardDetails HeroChulian Dragonspire
> heroDetails ChulianScavenger =
>     undefinedCardDetails HeroChulian Dragonspire
> heroDetails ChulianLooter =
>     undefinedCardDetails HeroChulian Dragonspire
> heroDetails EvokerAdept =
>     undefinedCardDetails HeroEvoker Dragonspire
> heroDetails EvokerScorcher =
>     undefinedCardDetails HeroEvoker Dragonspire
> heroDetails EvokerPyroclast =
>     undefinedCardDetails HeroEvoker Dragonspire
> heroDetails FlameWatch =
>     undefinedCardDetails HeroFlame Dragonspire
> heroDetails FlameGuard =
>     undefinedCardDetails HeroFlame Dragonspire
> heroDetails FlameHero =
>     undefinedCardDetails HeroFlame Dragonspire
> heroDetails GorinthAmateur =
>     undefinedCardDetails HeroGorinth Dragonspire
> heroDetails GorinthHoarder =
>     undefinedCardDetails HeroGorinth Dragonspire
> heroDetails GorinthMiser =
>     undefinedCardDetails HeroGorinth Dragonspire
> heroDetails HalfOrcRaider =
>     undefinedCardDetails HeroHalfOrc Dragonspire
> heroDetails HalfOrcMarauder =
>     undefinedCardDetails HeroHalfOrc Dragonspire
> heroDetails HalfOrcDervish =
>     undefinedCardDetails HeroHalfOrc Dragonspire
> heroDetails StoneguardBrute =
>     undefinedCardDetails HeroStoneguard Dragonspire
> heroDetails StoneguardBruiser =
>     undefinedCardDetails HeroStoneguard Dragonspire
> heroDetails StoneguardTanker =
>     undefinedCardDetails HeroStoneguard Dragonspire
> heroDetails TerakianDefender =
>     undefinedCardDetails HeroTerakian Dragonspire
> heroDetails TerakianPeer =
>     undefinedCardDetails HeroTerakian Dragonspire
> heroDetails TerakianTemplar =
>     undefinedCardDetails HeroTerakian Dragonspire
> heroDetails VeteranWarrior =
>     undefinedCardDetails HeroVeteran Dragonspire
> heroDetails VeteranBerserker =
>     undefinedCardDetails HeroVeteran Dragonspire
> heroDetails VeteranReaver =
>     undefinedCardDetails HeroVeteran Dragonspire
> heroDetails VeteranWarmonger =
>     undefinedCardDetails HeroVeteran Dragonspire

> heroDetails ClanSergeant =
>     undefinedCardDetails HeroClan Promotional
> heroDetails ClanCommander =
>     undefinedCardDetails HeroClan Promotional
> heroDetails ClanChampion =
>     undefinedCardDetails HeroClan Promotional
> heroDetails HarruliInitiate =
>     undefinedCardDetails HeroHarruli Promotional
> heroDetails HarruliSpellsword =
>     undefinedCardDetails HeroHarruli Promotional
> heroDetails HarruliAvatar =
>     undefinedCardDetails HeroHarruli Promotional




> data MonsterStats = MonsterStats {
>     monsterHealth :: Int,
>     monsterXP :: Int
>     }

> monsterDetails :: MonsterCard -> CardDetails MonsterType MonsterStats

> monsterDetails ArchdukeOfPain = CardDetails {
>     cardName = "Archduke of Pain",

Magic Attack Required

BREACH: Destroy the top two cards from each Hero deck in the Village.

BATTLE: Destroy all Clerics and Wizards.

>     cardSource = ThunderstoneBase,
>     cardType = MonsterAbyssal,
>     cardCount = 1,
>     cardClasses = [ClassAbyssal],
>     cardGold = 3,
>     cardLight = 0,
>     cardVictoryPoints = 8,
>     cardStats = MonsterStats {
>         monsterHealth = 10,
>         monsterXP = 3
>         }
>     }

> monsterDetails Grudgebeast = CardDetails {
>     cardName = "Grudgebeast",

>     cardSource = ThunderstoneBase,
>     cardType = MonsterAbyssal,
>     cardCount = 2,
>     cardClasses = [ClassAbyssal],
>     cardGold = 1,
>     cardLight = 0,
>     cardVictoryPoints = 2,
>     cardStats = MonsterStats {
>         monsterHealth = 4,
>         monsterXP = 1
>         }
>     }

> monsterDetails Succubus = CardDetails {
>     cardName = "Succubus",

HALF-ATTACK without MAGIC ATTACK present

>     cardSource = ThunderstoneBase,
>     cardType = MonsterAbyssal,
>     cardCount = 2,
>     cardClasses = [ClassAbyssal],
>     cardGold = 2,
>     cardLight = 0,
>     cardVictoryPoints = 4,
>     cardStats = MonsterStats {
>         monsterHealth = 6,
>         monsterXP = 2
>         }
>     }

> monsterDetails Tormentor = CardDetails {
>     cardName = "Tormentor",

HALF-ATTACK without a Weapon present

BATTLE: Destroy one Cleric.

>     cardSource = ThunderstoneBase,
>     cardType = MonsterAbyssal,
>     cardCount = 2,
>     cardClasses = [ClassAbyssal],
>     cardGold = 2,
>     cardLight = 0,
>     cardVictoryPoints = 6,
>     cardStats = MonsterStats {
>         monsterHealth = 8,
>         monsterXP = 3
>         }
>     }

> monsterDetails TheUnchained = CardDetails {
>     cardName = "The Unchained",

BATTLE: Gain one Disease.

* MAGIC ATTACK +1

>     cardSource = ThunderstoneBase,
>     cardType = MonsterAbyssal,
>     cardCount = 3,
>     cardClasses = [ClassAbyssal],
>     cardGold = 1,
>     cardLight = 0,
>     cardVictoryPoints = 3,
>     cardStats = MonsterStats {
>         monsterHealth = 5,
>         monsterXP = 1
>         }
>     }

> monsterDetails Darkness = CardDetails {
>     cardName = "Darkness",

Unequipped Heroes cannot attack

Light -1

>     cardSource = ThunderstoneBase,
>     cardType = MonsterDoomknightHumanoid,
>     cardCount = 2,
>     cardClasses = [ClassDoomknight,ClassHumanoid],
>     cardGold = 1,
>     cardLight = 0,
>     cardVictoryPoints = 4,
>     cardStats = MonsterStats {
>         monsterHealth = 6,
>         monsterXP = 2
>         }
>     }

> monsterDetails Judgement = CardDetails {
>     cardName = "Judgement",

BATTLE: All Heroes suffer Strength -2 and ATTACK -1.

>     cardSource = ThunderstoneBase,
>     cardType = MonsterDoomknightHumanoid,
>     cardCount = 2,
>     cardClasses = [ClassDoomknight,ClassHumanoid],
>     cardGold = 1,
>     cardLight = 0,
>     cardVictoryPoints = 3,
>     cardStats = MonsterStats {
>         monsterHealth = 5,
>         monsterXP = 1
>         }
>     }

> monsterDetails Knightmare = CardDetails {
>     cardName = "Knightmare",

Light -2

BATTLE: Destroy one Fighter.

>     cardSource = ThunderstoneBase,
>     cardType = MonsterDoomknightHumanoid,
>     cardCount = 2,
>     cardClasses = [ClassDoomknight,ClassHumanoid],
>     cardGold = 1,
>     cardLight = 0,
>     cardVictoryPoints = 6,
>     cardStats = MonsterStats {
>         monsterHealth = 8,
>         monsterXP = 3
>         }
>     }

> monsterDetails LordMortis = CardDetails {
>     cardName = "Lord Mortis",

Light -1

>     cardSource = ThunderstoneBase,
>     cardType = MonsterDoomknightHumanoid,
>     cardCount = 2,
>     cardClasses = [ClassDoomknight,ClassHumanoid],
>     cardGold = 1,
>     cardLight = 0,
>     cardVictoryPoints = 2,
>     cardStats = MonsterStats {
>         monsterHealth = 4,
>         monsterXP = 1
>         }
>     }

> monsterDetails ThePrince = CardDetails {
>     cardName = "The Prince",

BATTLE: All Heroes suffer Strength -2.

BATTLE: Destroy one Fighter.

>     cardSource = ThunderstoneBase,
>     cardType = MonsterDoomknightHumanoid,
>     cardCount = 2,
>     cardClasses = [ClassDoomknight,ClassHumanoid],
>     cardGold = 1,
>     cardLight = 0,
>     cardVictoryPoints = 5,
>     cardStats = MonsterStats {
>         monsterHealth = 7,
>         monsterXP = 2
>         }
>     }

> monsterDetails EbonFume = CardDetails {
>     cardName = "Ebon Fume",

Immune to Magic Attack

BATTLE: Destroy one Hero with the highest Strength.

* ATTACK +3

>     cardSource = ThunderstoneBase,
>     cardType = MonsterDragon,
>     cardCount = 1,
>     cardClasses = [ClassDragon,ClassBlack],
>     cardGold = 1,
>     cardLight = 0,
>     cardVictoryPoints = 8,
>     cardStats = MonsterStats {
>         monsterHealth = 11,
>         monsterXP = 3
>         }
>     }

> monsterDetails Mythlurian = CardDetails {
>     cardName = "Mythlurian",

BATTLE: Destroy one Hero.

>     cardSource = ThunderstoneBase,
>     cardType = MonsterDragon,
>     cardCount = 2,
>     cardClasses = [ClassDragon,ClassGreen],
>     cardGold = 3,
>     cardLight = 0,
>     cardVictoryPoints = 4,
>     cardStats = MonsterStats {
>         monsterHealth = 8,
>         monsterXP = 2
>         }
>     }

> monsterDetails Skaladak = CardDetails {
>     cardName = "Skaladak",

BATTLE: Destroy one Weapon.

>     cardSource = ThunderstoneBase,
>     cardType = MonsterDragon,
>     cardCount = 3,
>     cardClasses = [ClassDragon,ClassWhite],
>     cardGold = 3,
>     cardLight = 0,
>     cardVictoryPoints = 3,
>     cardStats = MonsterStats {
>         monsterHealth = 7,
>         monsterXP = 2
>         }
>     }

> monsterDetails TyxrTheOld = CardDetails {
>     cardName = "Tyxr the Old",

BREACH: Each player must discard two cards.

BATTLE: Destroy one Hero.

* MAGIC ATTACK +2

>     cardSource = ThunderstoneBase,
>     cardType = MonsterDragon,
>     cardCount = 2,
>     cardClasses = [ClassDragon,ClassRed],
>     cardGold = 2,
>     cardLight = 0,
>     cardVictoryPoints = 6,
>     cardStats = MonsterStats {
>         monsterHealth = 10,
>         monsterXP = 3
>         }
>     }

> monsterDetails UyrilUnending = CardDetails {
>     cardName = "Uyril Unending",

HALF-ATTACK without MAGIC ATTACK present

BATTLE: Destroy one Militia.

* MAGIC ATTACK +1

>     cardSource = ThunderstoneBase,
>     cardType = MonsterDragon,
>     cardCount = 2,
>     cardClasses = [ClassDragon,ClassBlue],
>     cardGold = 2,
>     cardLight = 0,
>     cardVictoryPoints = 5,
>     cardStats = MonsterStats {
>         monsterHealth = 9,
>         monsterXP = 2
>         }
>     }

> monsterDetails BlinkDog = CardDetails {
>     cardName = "Blink Dog",

Light -1

Cannot be attacked if a Light Penalty persists.

>     cardSource = ThunderstoneBase,
>     cardType = MonsterEnchanted,
>     cardCount = 2,
>     cardClasses = [ClassEnchanted],
>     cardGold = 1,
>     cardLight = 0,
>     cardVictoryPoints = 2,
>     cardStats = MonsterStats {
>         monsterHealth = 3,
>         monsterXP = 1
>         }
>     }

> monsterDetails Griffon = CardDetails {
>     cardName = "Griffon",

* MAGIC ATTACK +1

>     cardSource = ThunderstoneBase,
>     cardType = MonsterEnchanted,
>     cardCount = 2,
>     cardClasses = [ClassEnchanted],
>     cardGold = 1,
>     cardLight = 0,
>     cardVictoryPoints = 4,
>     cardStats = MonsterStats {
>         monsterHealth = 7,
>         monsterXP = 2
>         }
>     }

> monsterDetails Nixie = CardDetails {
>     cardName = "Nixie",

>     cardSource = ThunderstoneBase,
>     cardType = MonsterEnchanted,
>     cardCount = 2,
>     cardClasses = [ClassEnchanted],
>     cardGold = 4,
>     cardLight = 0,
>     cardVictoryPoints = 1,
>     cardStats = MonsterStats {
>         monsterHealth = 5,
>         monsterXP = 1
>         }
>     }

> monsterDetails Pegasus = CardDetails {
>     cardName = "Pegasus",

* ATTACK +1

>     cardSource = ThunderstoneBase,
>     cardType = MonsterEnchanted,
>     cardCount = 3,
>     cardClasses = [ClassEnchanted],
>     cardGold = 2,
>     cardLight = 0,
>     cardVictoryPoints = 2,
>     cardStats = MonsterStats {
>         monsterHealth = 6,
>         monsterXP = 1
>         }
>     }

> monsterDetails Sphinx = CardDetails {
>     cardName = "Sphinx",

Magic Attack Only

Spoils (Reveal six cards from your deck and destroy any of these
cards you choose.  Discard the rest.)

* MAGIC ATTACK +2

>     cardSource = ThunderstoneBase,
>     cardType = MonsterEnchanted,
>     cardCount = 1,
>     cardClasses = [ClassEnchanted],
>     cardGold = 2,
>     cardLight = 0,
>     cardVictoryPoints = 7,
>     cardStats = MonsterStats {
>         monsterHealth = 8,
>         monsterXP = 3
>         }
>     }

> monsterDetails BloodskullOrc = CardDetails {
>     cardName = "Bloodskull Orc",

>     cardSource = ThunderstoneBase,
>     cardType = MonsterHumanoid,
>     cardCount = 2,
>     cardClasses = [ClassHumanoid],
>     cardGold = 3,
>     cardLight = 0,
>     cardVictoryPoints = 0,
>     cardStats = MonsterStats {
>         monsterHealth = 5,
>         monsterXP = 1
>         }
>     }

> monsterDetails DeadboneTroll = CardDetails {
>     cardName = "Deadbone Troll",

>     cardSource = ThunderstoneBase,
>     cardType = MonsterHumanoid,
>     cardCount = 2,
>     cardClasses = [ClassHumanoid],
>     cardGold = 2,
>     cardLight = 0,
>     cardVictoryPoints = 5,
>     cardStats = MonsterStats {
>         monsterHealth = 7,
>         monsterXP = 2
>         }
>     }

> monsterDetails FirebrandCyclops = CardDetails {
>     cardName = "Firebrand Cyclops",

>     cardSource = ThunderstoneBase,
>     cardType = MonsterHumanoid,
>     cardCount = 2,
>     cardClasses = [ClassHumanoid],
>     cardGold = 1,
>     cardLight = 0,
>     cardVictoryPoints = 4,
>     cardStats = MonsterStats {
>         monsterHealth = 6,
>         monsterXP = 2
>         }
>     }

> monsterDetails GrayskinLizard = CardDetails {
>     cardName = "Grayskin Lizard",

>     cardSource = ThunderstoneBase,
>     cardType = MonsterHumanoid,
>     cardCount = 2,
>     cardClasses = [ClassHumanoid],
>     cardGold = 2,
>     cardLight = 0,
>     cardVictoryPoints = 3,
>     cardStats = MonsterStats {
>         monsterHealth = 6,
>         monsterXP = 1
>         }
>     }

> monsterDetails GriknackGoblin = CardDetails {
>     cardName = "Griknack Goblin",

>     cardSource = ThunderstoneBase,
>     cardType = MonsterHumanoid,
>     cardCount = 2,
>     cardClasses = [ClassHumanoid],
>     cardGold = 1,
>     cardLight = 0,
>     cardVictoryPoints = 1,
>     cardStats = MonsterStats {
>         monsterHealth = 4,
>         monsterXP = 1
>         }
>     }

> monsterDetails BlackSlime = CardDetails {
>     cardName = "Black Slime",

BATTLE: Destroy one Militia.

>     cardSource = ThunderstoneBase,
>     cardType = MonsterOoze,
>     cardCount = 2,
>     cardClasses = [ClassOoze],
>     cardGold = 1,
>     cardLight = 0,
>     cardVictoryPoints = 2,
>     cardStats = MonsterStats {
>         monsterHealth = 5,
>         monsterXP = 1
>         }
>     }

> monsterDetails GrayOoze = CardDetails {
>     cardName = "Gray Ooze",

BATTLE: Destroy one Hero unless at least one Weapon is attached
to the Party.

Spoils (Food)

>     cardSource = ThunderstoneBase,
>     cardType = MonsterOoze,
>     cardCount = 2,
>     cardClasses = [ClassOoze],
>     cardGold = 1,
>     cardLight = 0,
>     cardVictoryPoints = 3,
>     cardStats = MonsterStats {
>         monsterHealth = 6,
>         monsterXP = 2
>         }
>     }

> monsterDetails GreenBlob = CardDetails {
>     cardName = "Green Blob",

BATTLE: Destroy one Food.

>     cardSource = ThunderstoneBase,
>     cardType = MonsterOoze,
>     cardCount = 2,
>     cardClasses = [ClassOoze],
>     cardGold = 1,
>     cardLight = 0,
>     cardVictoryPoints = 2,
>     cardStats = MonsterStats {
>         monsterHealth = 5,
>         monsterXP = 1
>         }
>     }

> monsterDetails NoxiousSlag = CardDetails {
>     cardName = "Noxious Slag",

HALF-ATTACK from MAGIC ATTACK

Immune to Edged Weapons

>     cardSource = ThunderstoneBase,
>     cardType = MonsterOoze,
>     cardCount = 2,
>     cardClasses = [ClassOoze],
>     cardGold = 1,
>     cardLight = 0,
>     cardVictoryPoints = 6,
>     cardStats = MonsterStats {
>         monsterHealth = 7,
>         monsterXP = 3
>         }
>     }

> monsterDetails RedJelly = CardDetails {
>     cardName = "Red Jelly",

BATTLE: Destroy one Weapon.

>     cardSource = ThunderstoneBase,
>     cardType = MonsterOoze,
>     cardCount = 2,
>     cardClasses = [ClassOoze],
>     cardGold = 2,
>     cardLight = 0,
>     cardVictoryPoints = 2,
>     cardStats = MonsterStats {
>         monsterHealth = 6,
>         monsterXP = 2
>         }
>     }

> monsterDetails Famine = CardDetails {
>     cardName = "Famine",

BATTLE: Gain one Disease.

>     cardSource = ThunderstoneBase,
>     cardType = MonsterUndeadDoom,
>     cardCount = 2,
>     cardClasses = [ClassUndead,ClassDoom],
>     cardGold = 1,
>     cardLight = 0,
>     cardVictoryPoints = 2,
>     cardStats = MonsterStats {
>         monsterHealth = 4,
>         monsterXP = 1
>         }
>     }

> monsterDetails Harbinger = CardDetails {
>     cardName = "Harbinger",

BATTLE: Destroy one Spell.

>     cardSource = ThunderstoneBase,
>     cardType = MonsterUndeadDoom,
>     cardCount = 2,
>     cardClasses = [ClassUndead,ClassDoom],
>     cardGold = 2,
>     cardLight = 0,
>     cardVictoryPoints = 1,
>     cardStats = MonsterStats {
>         monsterHealth = 3,
>         monsterXP = 1
>         }
>     }

> monsterDetails Kingdom = CardDetails {
>     cardName = "Kingdom",

BATTLE: Gain one Disease.

>     cardSource = ThunderstoneBase,
>     cardType = MonsterUndeadDoom,
>     cardCount = 3,
>     cardClasses = [ClassUndead,ClassDoom],
>     cardGold = 2,
>     cardLight = 0,
>     cardVictoryPoints = 3,
>     cardStats = MonsterStats {
>         monsterHealth = 5,
>         monsterXP = 1
>         }
>     }

> monsterDetails LordOfDeath = CardDetails {
>     cardName = "Lord of Death",

BATTLE: Gain two Diseases.

>     cardSource = ThunderstoneBase,
>     cardType = MonsterUndeadDoom,
>     cardCount = 1,
>     cardClasses = [ClassUndead,ClassDoom],
>     cardGold = 2,
>     cardLight = 0,
>     cardVictoryPoints = 7,
>     cardStats = MonsterStats {
>         monsterHealth = 9,
>         monsterXP = 3
>         }
>     }

> monsterDetails Suffering = CardDetails {
>     cardName = "Suffering",

BATTLE: All Heroes suffer Strength -2.

BATTLE: Gain one Disease.

>     cardSource = ThunderstoneBase,
>     cardType = MonsterUndeadDoom,
>     cardCount = 2,
>     cardClasses = [ClassUndead,ClassDoom],
>     cardGold = 2,
>     cardLight = 0,
>     cardVictoryPoints = 4,
>     cardStats = MonsterStats {
>         monsterHealth = 6,
>         monsterXP = 2
>         }
>     }

> monsterDetails Ghost = CardDetails {
>     cardName = "Ghost",

HALF-ATTACK without MAGIC ATTACK present

BATTLE: All Heroes suffer Strength -2.

>     cardSource = ThunderstoneBase,
>     cardType = MonsterUndeadSpirit,
>     cardCount = 2,
>     cardClasses = [ClassUndead,ClassSpirit],
>     cardGold = 1,
>     cardLight = 0,
>     cardVictoryPoints = 4,
>     cardStats = MonsterStats {
>         monsterHealth = 6,
>         monsterXP = 1
>         }
>     }

> monsterDetails Haunt = CardDetails {
>     cardName = "Haunt",

BATTLE: One Hero cannot attack.

>     cardSource = ThunderstoneBase,
>     cardType = MonsterUndeadSpirit,
>     cardCount = 2,
>     cardClasses = [ClassUndead,ClassSpirit],
>     cardGold = 1,
>     cardLight = 0,
>     cardVictoryPoints = 2,
>     cardStats = MonsterStats {
>         monsterHealth = 4,
>         monsterXP = 1
>         }
>     }

> monsterDetails Revenant = CardDetails {
>     cardName = "Revenant",

Magic Attack Required

BATTLE: All Heroes suffer Strength -4.  Any Heroes with Strength 0 or
less are Destroyed.

>     cardSource = ThunderstoneBase,
>     cardType = MonsterUndeadSpirit,
>     cardCount = 2,
>     cardClasses = [ClassUndead,ClassSpirit],
>     cardGold = 2,
>     cardLight = 0,
>     cardVictoryPoints = 5,
>     cardStats = MonsterStats {
>         monsterHealth = 7,
>         monsterXP = 2
>         }
>     }

> monsterDetails Spectre = CardDetails {
>     cardName = "Spectre",

BATTLE: Destroy one Militia.

>     cardSource = ThunderstoneBase,
>     cardType = MonsterUndeadSpirit,
>     cardCount = 2,
>     cardClasses = [ClassUndead,ClassSpirit],
>     cardGold = 1,
>     cardLight = 0,
>     cardVictoryPoints = 3,
>     cardStats = MonsterStats {
>         monsterHealth = 5,
>         monsterXP = 1
>         }
>     }

> monsterDetails Wraith = CardDetails {
>     cardName = "Wraith",

BATTLE: Destroy one Militia.

>     cardSource = ThunderstoneBase,
>     cardType = MonsterUndeadSpirit,
>     cardCount = 2,
>     cardClasses = [ClassUndead,ClassSpirit],
>     cardGold = 2,
>     cardLight = 0,
>     cardVictoryPoints = 4,
>     cardStats = MonsterStats {
>         monsterHealth = 6,
>         monsterXP = 2
>         }
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

> data VillageStats = VillageStats {
>     villageWeight :: Int,
>     villagePrice :: Int
>     }

> villageDetails :: VillageCard -> CardDetails VillageCard VillageStats
> villageDetails Dagger = CardDetails {
>     cardName = "Dagger",

ATTACK +1

>     cardSource = ThunderstoneBase,
>     cardType = Dagger,
>     cardCount = 15,
>     cardClasses = [ClassWeapon,ClassEdged],
>     cardGold = 1,
>     cardLight = 0,
>     cardVictoryPoints = 0,
>     cardStats = VillageStats {
>         villageWeight = 2,
>         villagePrice = 3
>         }
>     }

> villageDetails IronRations = CardDetails {
>     cardName = "Iron Rations",

DUNGEON: One Hero gains Strength +2.

>     cardSource = ThunderstoneBase,
>     cardType = IronRations,
>     cardCount = 15,
>     cardClasses = [ClassItem,ClassFood],
>     cardGold = 2,
>     cardLight = 0,
>     cardVictoryPoints = 0,
>     cardStats = VillageStats {
>         villageWeight = 0,
>         villagePrice = 2
>         }
>     }

> villageDetails Torch = CardDetails {
>     cardName = "Torch",

>     cardSource = ThunderstoneBase,
>     cardType = Torch,
>     cardCount = 15,
>     cardClasses = [ClassItem,ClassLight],
>     cardGold = 2,
>     cardLight = 1,
>     cardVictoryPoints = 0,
>     cardStats = VillageStats {
>         villageWeight = 0,
>         villagePrice = 3
>         }
>     }

> villageDetails ArcaneEnergies = CardDetails {
>     cardName = "Arcane Energies",

DUNGEON: All ATTACKS from Heroes with Weapons equipped become
MAGIC ATTACKS.  Draw one card.

>     cardSource = ThunderstoneBase,
>     cardType = ArcaneEnergies,
>     cardCount = 8,
>     cardClasses = [ClassSpell],
>     cardGold = 0,
>     cardLight = 0,
>     cardVictoryPoints = 0,
>     cardStats = VillageStats {
>         villageWeight = 0,
>         villagePrice = 2
>         }
>     }

> villageDetails Banish = CardDetails {
>     cardName = "Banish",

DUNGEON: Return one Monster to the bottom of the deck and refill
the hall, or rearrange the hall.  Destroy one card from your hand.
Draw one card.

>     cardSource = ThunderstoneBase,
>     cardType = Banish,
>     cardCount = 8,
>     cardClasses = [ClassSpell],
>     cardGold = 0,
>     cardLight = 0,
>     cardVictoryPoints = 0,
>     cardStats = VillageStats {
>         villageWeight = 0,
>         villagePrice = 4
>         }
>     }

> villageDetails Barkeep = CardDetails {
>     cardName = "Barkeep",

VILLAGE: You may purchase one additional card this turn.

VILLAGE: Destroy this card to gain 2 Gold.

>     cardSource = ThunderstoneBase,
>     cardType = Barkeep,
>     cardCount = 8,
>     cardClasses = [ClassVillager],
>     cardGold = 1,
>     cardLight = 0,
>     cardVictoryPoints = 0,
>     cardStats = VillageStats {
>         villageWeight = 0,
>         villagePrice = 2
>         }
>     }

> villageDetails BattleFury = CardDetails {
>     cardName = "BattleFury",

DUNGEON: All Heroes gain ATTACK +1.

>     cardSource = ThunderstoneBase,
>     cardType = BattleFury,
>     cardCount = 8,
>     cardClasses = [ClassSpell],
>     cardGold = 0,
>     cardLight = 0,
>     cardVictoryPoints = 0,
>     cardStats = VillageStats {
>         villageWeight = 0,
>         villagePrice = 3
>         }
>     }

> villageDetails Feast = CardDetails {
>     cardName = "Feast",

DUNGEON: All Heroes gain Strength +1 and ATTACK +1.

>     cardSource = ThunderstoneBase,
>     cardType = Feast,
>     cardCount = 8,
>     cardClasses = [ClassItem,ClassFood],
>     cardGold = 3,
>     cardLight = 0,
>     cardVictoryPoints = 0,
>     cardStats = VillageStats {
>         villageWeight = 0,
>         villagePrice = 5
>         }
>     }

> villageDetails Fireball = CardDetails {
>     cardName = "Fireball",

MAGIC ATTACK +3

>     cardSource = ThunderstoneBase,
>     cardType = Fireball,
>     cardCount = 8,
>     cardClasses = [ClassSpell],
>     cardGold = 0,
>     cardLight = 1,
>     cardVictoryPoints = 0,
>     cardStats = VillageStats {
>         villageWeight = 0,
>         villagePrice = 9
>         }
>     }

> villageDetails FlamingSword = CardDetails {
>     cardName = "Flaming Sword",

MAGIC ATTACK +3

>     cardSource = ThunderstoneBase,
>     cardType = FlamingSword,
>     cardCount = 8,
>     cardClasses = [ClassWeapon,ClassEdged],
>     cardGold = 2,
>     cardLight = 1,
>     cardVictoryPoints = 0,
>     cardStats = VillageStats {
>         villageWeight = 5,
>         villagePrice = 5
>         }
>     }

> villageDetails Goodberries = CardDetails {
>     cardName = "Goodberries",

DUNGEON: One Hero gains Strength +3 and ATTACK becomes MAGIC ATTACK
for that Hero.

>     cardSource = ThunderstoneBase,
>     cardType = Goodberries,
>     cardCount = 8,
>     cardClasses = [ClassItem,ClassFood,ClassMagic],
>     cardGold = 2,
>     cardLight = 0,
>     cardVictoryPoints = 0,
>     cardStats = VillageStats {
>         villageWeight = 0,
>         villagePrice = 4
>         }
>     }

> villageDetails Hatchet = CardDetails {
>     cardName = "Hatchet",

ATTACK +3

>     cardSource = ThunderstoneBase,
>     cardType = Hatchet,
>     cardCount = 8,
>     cardClasses = [ClassWeapon,ClassEdged],
>     cardGold = 1,
>     cardLight = 0,
>     cardVictoryPoints = 0,
>     cardStats = VillageStats {
>         villageWeight = 3,
>         villagePrice = 4
>         }
>     }

> villageDetails Lantern = CardDetails {
>     cardName = "Lantern",

>     cardSource = ThunderstoneBase,
>     cardType = Lantern,
>     cardCount = 8,
>     cardClasses = [ClassItem,ClassLight],
>     cardGold = 2,
>     cardLight = 2,
>     cardVictoryPoints = 0,
>     cardStats = VillageStats {
>         villageWeight = 0,
>         villagePrice = 4
>         }
>     }

> villageDetails LightstoneGem = CardDetails {
>     cardName = "Lightstone Gem",

>     cardSource = ThunderstoneBase,
>     cardType = LightstoneGem,
>     cardCount = 8,
>     cardClasses = [ClassItem,ClassLight,ClassMagic],
>     cardGold = 3,
>     cardLight = 3,
>     cardVictoryPoints = 0,
>     cardStats = VillageStats {
>         villageWeight = 0,
>         villagePrice = 6
>         }
>     }

> villageDetails MagicalAura = CardDetails {
>     cardName = "Magical Aura",

DUNGEON: All Weapons become Weight 0.  Draw one card.

>     cardSource = ThunderstoneBase,
>     cardType = MagicalAura,
>     cardCount = 8,
>     cardClasses = [ClassSpell],
>     cardGold = 2,
>     cardLight = 0,
>     cardVictoryPoints = 0,
>     cardStats = VillageStats {
>         villageWeight = 0,
>         villagePrice = 4
>         }
>     }

> villageDetails Pawnbroker = CardDetails {
>     cardName = "Pawnbroker",

VILLAGE: Destroy any card with a gold value to gain its gold value
plus 3 Gold.

VILLAGE: Destroy this card to gain 2 Gold.

>     cardSource = ThunderstoneBase,
>     cardType = Pawnbroker,
>     cardCount = 8,
>     cardClasses = [ClassVillager],
>     cardGold = 0,
>     cardLight = 0,
>     cardVictoryPoints = 0,
>     cardStats = VillageStats {
>         villageWeight = 0,
>         villagePrice = 3
>         }
>     }

> villageDetails Polearm = CardDetails {
>     cardName = "Polearm",

ATTACK +2, or ATTACK +6 when attached to a Hero with 8 or more Strength.

>     cardSource = ThunderstoneBase,
>     cardType = Polearm,
>     cardCount = 8,
>     cardClasses = [ClassWeapon,ClassEdged],
>     cardGold = 3,
>     cardLight = 0,
>     cardVictoryPoints = 0,
>     cardStats = VillageStats {
>         villageWeight = 2,
>         villagePrice = 7
>         }
>     }

> villageDetails ShortSword = CardDetails {
>     cardName = "Short Sword",

ATTACK +4

>     cardSource = ThunderstoneBase,
>     cardType = ShortSword,
>     cardCount = 8,
>     cardClasses = [ClassWeapon,ClassEdged],
>     cardGold = 3,
>     cardLight = 0,
>     cardVictoryPoints = 0,
>     cardStats = VillageStats {
>         villageWeight = 4,
>         villagePrice = 6
>         }
>     }

> villageDetails Spear = CardDetails {
>     cardName = "Spear",

ATTACK +2

>     cardSource = ThunderstoneBase,
>     cardType = Spear,
>     cardCount = 8,
>     cardClasses = [ClassWeapon,ClassEdged],
>     cardGold = 2,
>     cardLight = 0,
>     cardVictoryPoints = 0,
>     cardStats = VillageStats {
>         villageWeight = 4,
>         villagePrice = 4
>         }
>     }

> villageDetails TownGuard = CardDetails {
>     cardName = "Town Guard",

VILLAGE: Draw two cards.

VILLAGE: Destroy this card to draw three additional cards.

>     cardSource = ThunderstoneBase,
>     cardType = TownGuard,
>     cardCount = 8,
>     cardClasses = [ClassVillager],
>     cardGold = 0,
>     cardLight = 0,
>     cardVictoryPoints = 0,
>     cardStats = VillageStats {
>         villageWeight = 0,
>         villagePrice = 3
>         }
>     }

> villageDetails Trainer = CardDetails {
>     cardName = "Trainer",

VILLAGE: Destroy one Militia to gain 2 XP.

VILLAGE: Destroy this card to gain 2 Gold.

>     cardSource = ThunderstoneBase,
>     cardType = Trainer,
>     cardCount = 8,
>     cardClasses = [ClassVillager],
>     cardGold = 0,
>     cardLight = 0,
>     cardVictoryPoints = 0,
>     cardStats = VillageStats {
>         villageWeight = 0,
>         villagePrice = 4
>         }
>     }

> villageDetails Warhammer = CardDetails {
>     cardName = "Warhammer",

ATTACK +3

Clerics gain an additional ATTACK +3 against Doomknights and Undead.

>     cardSource = ThunderstoneBase,
>     cardType = Warhammer,
>     cardCount = 8,
>     cardClasses = [ClassWeapon,ClassBlunt],
>     cardGold = 2,
>     cardLight = 0,
>     cardVictoryPoints = 0,
>     cardStats = VillageStats {
>         villageWeight = 5,
>         villagePrice = 4
>         }
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

> data DungeonFeatureStats = DungeonFeatureStats

> dungeonFeatureDetails :: DungeonFeatureCard
>         -> CardDetails DungeonFeatureType DungeonFeatureStats
> dungeonFeatureDetails = undefined

> data GuardianStats = GuardianStats

> guardianDetails :: GuardianCard
>         -> CardDetails DungeonFeatureType GuardianStats
> guardianDetails = undefined

> cardsOfType :: (Bounded card, Enum card, Eq cardType) => 
>                (card -> CardDetails cardType cardStats) -> cardType
>             -> [card]
> cardsOfType details ofType = concatMap countOff cards
>   where
>     cards =
>         filter ((== ofType) . cardType . details) [minBound .. maxBound]
>     countOff card = replicate (cardCount $ details card) card







Temporary code:

> undefinedCardDetails :: cardType -> Source
>                         -> CardDetails cardType stats
> undefinedCardDetails cardType source = CardDetails {
>     cardName = undefined,
>     cardSource = source,
>     cardType = cardType,
>     cardCount = undefined,
>     cardClasses = undefined,
>     cardGold = undefined,
>     cardLight = undefined,
>     cardVictoryPoints = undefined,
>     cardStats = undefined
>     }
