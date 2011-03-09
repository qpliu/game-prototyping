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

Randomizers:

> data HeroType =

Base set (11):

>     HeroAmazon
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

Cards:

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

>     DoomknightGuardian

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

>   | Amazon1
>   | Amazon2
>   | Amazon3
>   | Chalice -- 1,2,3
>   | Dwarf -- 1,2,3
>   | Elf -- 1,2,3
>   | Feayn -- 1,2,3
>   | Lorigg -- 1,2,3
>   | Outlands -- 1,2,3
>   | Redblade -- 1,2,3
>   | Regian -- 1,2,3
>   | Selurin -- 1,2,3
>   | Thyrian -- 1,2,3

Wrath of the Elements:

>   | Blind -- 1,2,3
>   | Diin -- 1,2,3
>   | Divine -- 1,2,3
>   | Gangland -- 1,2,3
>   | Gohlen -- 1,2,3
>   | Runespawn -- 1,2,3
>   | Toryn -- 1,2,3

Doomgate Legion:

>   | Deep -- 1,2,3
>   | Drunari -- 1,2,3
>   | Sidhe -- 1,2,3
>   | Slynn -- 1,2,3
>   | Tempest -- 1,2,3
>   | Tholis -- 1,2,3
>   | Verdan -- 1,2,3

Dragonspire:

>   | Phalanx -- 1,2
>   | Belzur -- 1,2,3
>   | Cabal -- 1,2,3
>   | Chulian -- 1,2,3
>   | Evoker -- 1,2,3
>   | Flame -- 1,2,3
>   | Gorinth -- 1,2,3
>   | HalfOrc -- 1,2,3
>   | Stoneguard -- 1,2,3
>   | Terakian -- 1,2,3
>   | Veteran -- 1,2,3,4

Promotional:

>   | Clan -- 1,2,3
>   | Harruli -- 1,2,3

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

The card descriptor links the card to the randomizer and source.

> data CardDescriptor cardType stats = CardDescriptor {
>     cardName :: String,
>     cardSource :: Source,
>     cardType :: cardType,
>     cardCount :: Int,
>     cardStats :: stats
>     }

> data ThunderstoneStats = ThunderstoneStats

> thunderstoneDescriptor :: ThunderstoneCard
>         -> CardDescriptor ThunderstoneCard ThunderstoneStats

> thunderstoneDescriptor StoneOfMystery = undefined
> thunderstoneDescriptor StoneOfAgony = undefined
> thunderstoneDescriptor StoneOfAvarice = undefined
> thunderstoneDescriptor StoneOfTerror = undefined
> thunderstoneDescriptor StoneOfScorn = undefined

> data HeroStats = HeroStats {
>     heroLevel :: Int,
>     heroUpgrade :: [(HeroCard,Int)]
>     }

> heroDescriptor :: HeroCard -> CardDescriptor HeroType HeroStats
> heroDescriptor = undefined

> data MonsterStats = MonsterStats

> monsterDescriptor :: MonsterCard -> CardDescriptor MonsterType MonsterStats
> monsterDescriptor = undefined

> data VillageStats = VillageStats

> villageDescriptor :: VillageCard -> CardDescriptor VillageCard VillageStats
> villageDescriptor = undefined

> data DungeonFeatureStats = DungeonFeatureStats

> dungeonFeatureDescriptor :: DungeonFeatureCard
>         -> CardDescriptor DungeonFeatureType DungeonFeatureStats
> dungeonFeatureDescriptor = undefined

> data GuardianStats = GuardianStats

> guardianDescriptor :: GuardianCard
>         -> CardDescriptor DungeonFeatureType GuardianStats
> guardianDescriptor = undefined
