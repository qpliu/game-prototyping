module ThunderstoneCardDetails where

import ThunderstoneCards

data CardDetails cardType = CardDetails {
    cardName :: String,
    cardSource :: Source,
    cardType :: cardType,
    cardIcon :: CardIcon,
    cardCount :: Int,
    cardClasses :: [CardClass],
    cardGold :: Maybe Int,
    cardLight :: Maybe Int,
    cardVictoryPoints :: Maybe Int,
    cardStrength :: Maybe Int,
    cardPrice :: Maybe Int,
    cardXP :: Maybe Int,
    cardHealth :: Maybe Int,
    cardWeight :: Maybe Int,
    cardLevelUp :: Maybe Int,
    cardText :: [String],
    cardGlossary :: [String]
    }

data CardIcon =
    CardIconBasic
  | CardIconDungeon
  | CardIconHero Int
  | CardIconNone
  | CardIconVillage
  deriving (Eq,Ord)

instance Read CardIcon where
    readsPrec _ str =
        maybe (error $ "Unknown icon: " ++ str) id
            $ foldl matchIcon Nothing icons
      where
        matchIcon Nothing (name,icon)
            | name == take (length name) str =
                Just [(icon,drop (length name) str)]
        matchIcon result _ = result
        icons = [("Basic",CardIconBasic),
                 ("Dungeon",CardIconDungeon),
                 ("Hero1",CardIconHero 1),
                 ("Hero2",CardIconHero 2),
                 ("Hero3",CardIconHero 3),
                 ("Hero4",CardIconHero 4),
                 ("None",CardIconNone),
                 ("Village",CardIconVillage)]

instance Show CardIcon where
    show CardIconBasic = "Basic"
    show CardIconDungeon = "Dungeon"
    show (CardIconHero n) = "Hero" ++ show n
    show CardIconNone = "None"
    show CardIconVillage = "Village"

initCardDetails :: CardDetails cardType
initCardDetails = CardDetails {
    cardName = undefined,
    cardSource = undefined,
    cardType = undefined,
    cardIcon = undefined,
    cardCount = undefined,
    cardClasses = undefined,
    cardGold = Nothing,
    cardLight = Nothing,
    cardVictoryPoints = Nothing,
    cardStrength = Nothing,
    cardPrice = Nothing,
    cardXP = Nothing,
    cardHealth = Nothing,
    cardWeight = Nothing,
    cardLevelUp = Nothing,
    cardText = [],
    cardGlossary = []
    }

-- Thunderstone cards:

thunderstoneDetails :: ThunderstoneCard -> CardDetails ThunderstoneCard

thunderstoneDetails StoneOfMystery = initCardDetails {
    cardName = "Stone of Mystery",
    cardSource = ThunderstoneBase,
    cardType = StoneOfMystery,
    cardIcon = CardIconBasic,
    cardCount = 1,
    cardClasses = [ClassThunderstone],
    cardVictoryPoints = Just 3,
    cardText = ["...and the thunder of the wind shouts back."],
    cardGlossary =
       ["Stone of Mystery: This is the only Thunderstone card "
        ++ "in the basic set.  Some expansions will include "
        ++ "other Stones, each with its own powers.  You are "
        ++ "welcome to use any Thunderstone card during setup."]
    }

thunderstoneDetails StoneOfAgony = initCardDetails {
    cardName = "Stone of Agony",
    cardSource = WrathOfTheElements,
    cardType = StoneOfAgony,
    cardIcon = CardIconBasic,
    cardCount = 1,
    cardClasses = [ClassThunderstone],
    cardVictoryPoints = Just 1,
    cardText = ["...thunderbolt from hell shattering aloud."],
    cardGlossary = []
    }

thunderstoneDetails StoneOfAvarice = initCardDetails {
    cardName = "Stone of Avarice",
    cardSource = DoomgateLegion,
    cardType = StoneOfAvarice,
    cardIcon = CardIconBasic,
    cardCount = 1,
    cardClasses = [ClassThunderstone],
    cardGold = Just 2,
    cardVictoryPoints = Just 1,
    cardText = ["...as greed worms its way into the souls of mortals."],
    cardGlossary = []
    }

thunderstoneDetails StoneOfTerror = initCardDetails {
    cardName = "Stone of Terror",
    cardSource = Dragonspire,
    cardType = StoneOfTerror,
    cardIcon = CardIconBasic,
    cardCount = 1,
    cardClasses = [ClassThunderstone],
    cardVictoryPoints = Just 0,
    cardText = ["Gain 2 Victory Points for each other Thunderstone you "
                ++ "have at the end of the game.",
                "DUNGEON: Draw 2 cards.",
                "Thunder shatters the courage of mortals..."],
    cardGlossary = []
    }

thunderstoneDetails StoneOfScorn = initCardDetails {
    cardName = "Stone of Scorn",
    cardSource = Dragonspire,
    cardType = StoneOfScorn,
    cardIcon = CardIconBasic,
    cardCount = 1,
    cardClasses = [ClassThunderstone],
    cardVictoryPoints = Just 1,
    cardText = ["Worth an additional 4 Victory Points at the end of "
                ++ "the game if each other player has more Monster "
                ++ "cards than you do.",
                "...as lightning lashes its disdain on the lands below."],
    cardGlossary = []
    }

-- Hero cards:

heroDetails :: HeroCard -> CardDetails HeroType

heroDetails Militia = initCardDetails {
    cardName = "Militia",
    cardSource = ThunderstoneBase,
    cardType = HeroMilitia,
    cardIcon = CardIconBasic,
    cardCount = 30,
    cardClasses = [ClassMilitia,ClassHero],
    cardGold = Just 0,
    cardStrength = Just 2,
    cardPrice = Just 0,
    cardLevelUp = Just 3,
    cardText = ["ATTACK +1"],
    cardGlossary =
       ["Militia: Militia are considered Heroes for all "
        ++ "purposes.  Militia have a gold value of 0.  "
        ++ "This is a basic card included in every game."]
    }

heroDetails AmazonArcher = initCardDetails {
    cardName = "Amazon Archer",
    cardSource = ThunderstoneBase,
    cardType = HeroAmazon,
    cardIcon = CardIconHero 1,
    cardCount = 6,
    cardClasses = [ClassFighter,ClassArcher],
    cardStrength = Just 4,
    cardPrice = Just 6,
    cardLevelUp = Just 2,
    cardText = ["ATTACK +1","Additional ATTACK +2 at Rank 2 or 3."],
    cardGlossary =
        ["Amazon: This Hero's Dungeon Effect is an Attack that "
         ++ "is in addition to the Amazon's normal Attack."]
    }

heroDetails AmazonHuntress = initCardDetails {
    cardName = "Amazon Huntress",
    cardSource = ThunderstoneBase,
    cardType = HeroAmazon,
    cardIcon = CardIconHero 2,
    cardCount = 4,
    cardClasses = [ClassFighter,ClassArcher],
    cardStrength = Just 5,
    cardPrice = Just 9,
    cardLevelUp = Just 3,
    cardText = ["ATTACK +2","Additional ATTACK +3 at Rank 2 or 3."],
    cardGlossary =
        ["Amazon: This Hero's Dungeon Effect is an Attack that "
         ++ "is in addition to the Amazon's normal Attack."]
    }

heroDetails AmazonQueen = initCardDetails {
    cardName = "Amazon Queen",
    cardSource = ThunderstoneBase,
    cardType = HeroAmazon,
    cardIcon = CardIconHero 3,
    cardCount = 2,
    cardClasses = [ClassFighter,ClassArcher],
    cardVictoryPoints = Just 2,
    cardStrength = Just 6,
    cardPrice = Just 11,
    cardText = ["ATTACK +2","Additional ATTACK +4 at Rank 2 or 3."],
    cardGlossary =
        ["Amazon: This Hero's Dungeon Effect is an Attack that "
         ++ "is in addition to the Amazon's normal Attack."]
    }

heroDetails ChaliceQuester = initCardDetails {
    cardName = "Chalice Quester",
    cardSource = ThunderstoneBase,
    cardType = HeroChalice,
    cardIcon = CardIconHero 1,
    cardCount = 6,
    cardClasses = [ClassFighter,ClassCleric],
    cardStrength = Just 5,
    cardPrice = Just 7,
    cardLevelUp = Just 2,
    cardText = ["ATTACK +2",
                "REPEAT DUNGEON: Destroy one Disease to draw one card."],
    cardGlossary =
        ["Chalice Quester and Defender: You man continue to destroy "
         ++ "Disease cards and draw new cards until you choose "
         ++ "which Monster to attack."]
    }

heroDetails ChaliceDefender = initCardDetails {
    cardName = "Chalice Defender",
    cardSource = ThunderstoneBase,
    cardType = HeroChalice,
    cardIcon = CardIconHero 2,
    cardCount = 4,
    cardClasses = [ClassFighter,ClassCleric],
    cardStrength = Just 6,
    cardPrice = Just 10,
    cardLevelUp = Just 3,
    cardText = ["ATTACK +3",
                "DUNGEON: ATTACK +1 for each Item that produces Light.",
                "DUNGEON: Draw one card.",
                "REPEAT DUNGEON: Destroy one Disease to draw one card."],
    cardGlossary =
        ["Chalice Quester and Defender: You man continue to destroy "
         ++ "Disease cards and draw new cards until you choose which "
         ++ "Monster to attack.",
         "Chalice Defender: Only Items (not Weapons) that provide a "
         ++ "Light bonus increase the Defender's Attack Value."]
    }

heroDetails ChalicePaladin = initCardDetails {
    cardName = "Chalice Paladin",
    cardSource = ThunderstoneBase,
    cardType = HeroChalice,
    cardIcon = CardIconHero 3,
    cardCount = 2,
    cardClasses = [ClassFighter,ClassCleric],
    cardVictoryPoints = Just 2,
    cardStrength = Just 7,
    cardPrice = Just 12,
    cardText = ["ATTACK +4","DUNGEON: Draw one card.","Spoils (Village)."],
    cardGlossary =
        ["Chalic Paladin: You may purchase any one Village card "
         ++ "(including Basic and Hero cards) from the Village "
         ++ "after a victorious battle, using the gold in your hand."]
    }

heroDetails DwarfGuardian = initCardDetails {
    cardName = "Dwarf Guardian",
    cardSource = ThunderstoneBase,
    cardType = HeroDwarf,
    cardIcon = CardIconHero 1,
    cardCount = 6,
    cardClasses = [ClassFighter],
    cardStrength = Just 5,
    cardPrice = Just 6,
    cardLevelUp = Just 2,
    cardText = ["ATTACK +1",
                "Additional ATTACK +3 when equipped with an Edged Weapon."],
    cardGlossary =
        ["Dwarf Guardian: His total Attack Value if an Edged Weapon "
         ++ "is equipped is +4.  This bonus is part of the Dwarf's "
         ++ "ability which he retains even if the Weapon later "
         ++ "becomes useless (due to a Monster's Battle Effect, for "
         ++ "instance)."]
    }

heroDetails DwarfJanissary = initCardDetails {
    cardName = "Dwarf Janissary",
    cardSource = ThunderstoneBase,
    cardType = HeroDwarf,
    cardIcon = CardIconHero 2,
    cardCount = 4,
    cardClasses = [ClassFighter],
    cardStrength = Just 6,
    cardPrice = Just 9,
    cardLevelUp = Just 3,
    cardText = ["ATTACK +2",
                "Additional ATTACK +4 when equipped with an Edged Weapon.",
                "Spoils (Weapon)."],
    cardGlossary =
        ["Dwarf Janissary: If revealed during a Dungeon action, you "
         ++ "may purchase one Weapon card from the Village after a "
         ++ "victorious battle, using the gold in your hand.  His "
         ++ "total Attack Value if an Edged Weapon is equipped is +6."]
    }

heroDetails DwarfSentinel = initCardDetails {
    cardName = "Dwarf Sentinel",
    cardSource = ThunderstoneBase,
    cardType = HeroDwarf,
    cardIcon = CardIconHero 3,
    cardCount = 2,
    cardClasses = [ClassFighter],
    cardVictoryPoints = Just 2,
    cardStrength = Just 8,
    cardPrice = Just 12,
    cardText = ["ATTACK +3",
                "Additional ATTACK +5 when equipped with an Edged Weapon."],
    cardGlossary =
        ["Dwarf Sentinel: His total Attack Value with an Edged Weapon "
         ++ "equipped is +8."]
    }

heroDetails ElfWizard = initCardDetails {
    cardName = "Elf Wizard",
    cardSource = ThunderstoneBase,
    cardType = HeroElf,
    cardIcon = CardIconHero 1,
    cardCount = 6,
    cardClasses = [ClassWizard],
    cardLight = Just 1,
    cardStrength = Just 3,
    cardPrice = Just 5,
    cardLevelUp = Just 2,
    cardText = ["MAGIC ATTACK +2"]
    }

heroDetails ElfSorcerer = initCardDetails {
    cardName = "Elf Sorcerer",
    cardSource = ThunderstoneBase,
    cardType = HeroElf,
    cardIcon = CardIconHero 2,
    cardCount = 4,
    cardClasses = [ClassWizard],
    cardLight = Just 2,
    cardStrength = Just 3,
    cardPrice = Just 8,
    cardLevelUp = Just 2,
    cardText = ["MAGIC ATTACK +3",
                "You may return one Monster to the bottom of the deck "
                ++ "after defeating a monster.  (Refill the hall.)"],
    cardGlossary =
        ["Elf Sorcerer/Archmage: When a Monster is returned to the "
         ++ "bottom of the monster deck, refill the Dungeon Hall.  "
         ++ "If this results in a Breach effect, resolve it "
         ++ "immediately.  If the Thunderstone moves to Rank 1 of "
         ++ "the Dungeon Hall, the game ends immediately; you do "
         ++ "not collect the Thunderstone."]
    }

heroDetails ElfArchmage = initCardDetails {
    cardName = "Elf Archmage",
    cardSource = ThunderstoneBase,
    cardType = HeroElf,
    cardIcon = CardIconHero 3,
    cardCount = 2,
    cardClasses = [ClassWizard],
    cardLight = Just 2,
    cardVictoryPoints = Just 2,
    cardStrength = Just 4,
    cardPrice = Just 10,
    cardText = ["MAGIC ATTACK +4",
                "DUNGEON You may return one Monster to the bottom of "
                ++ "the deck and refill the hall before the beginning of "
                ++ "a battle."],
    cardGlossary =
        ["Elf Sorcerer/Archmage: When a Monster is returned to the "
         ++ "bottom of the monster deck, refill the Dungeon Hall.  "
         ++ "If this results in a Breach effect, resolve it "
         ++ "immediately.  If the Thunderstone moves to Rank 1 of "
         ++ "the Dungeon Hall, the game ends immediately; you do "
         ++ "not collect the Thunderstone."]
    }

heroDetails FeaynArcher = initCardDetails {
    cardName = "Feayn Archer",
    cardSource = ThunderstoneBase,
    cardType = HeroFeayn,
    cardIcon = CardIconHero 1,
    cardCount = 6,
    cardClasses = [ClassFighter,ClassArcher],
    cardLight = Just 1,
    cardStrength = Just 4,
    cardPrice = Just 7,
    cardLevelUp = Just 2,
    cardText = ["Cannot attack Rank 1.","ATTACK +2"],
    cardGlossary =
        ["Feayn: If a Dungeon Actions causes you to attack a Monster "
         ++ "in Rank 1, do not add the Feayn's Attack bonus to your "
         ++ "Attack Value.  If Feayn does not attack, his Light bonus "
         ++ "is lost."]
    }

heroDetails FeaynMarksman = initCardDetails {
    cardName = "Feayn Marksman",
    cardSource = ThunderstoneBase,
    cardType = HeroFeayn,
    cardIcon = CardIconHero 2,
    cardCount = 4,
    cardClasses = [ClassFighter,ClassArcher],
    cardLight = Just 1,
    cardStrength = Just 5,
    cardPrice = Just 10,
    cardLevelUp = Just 3,
    cardText = ["Cannot attack Rank 1.","ATTACK +3"],
    cardGlossary =
        ["Feayn: If a Dungeon Actions causes you to attack a "
         ++ "Monster in Rank 1, do not add the Feayn's Attack "
         ++ "bonus to your Attack Value.  If Feayn does not "
         ++ "attack, his Light bonus is lost."]
    }

heroDetails FeaynSniper = initCardDetails {
    cardName = "Feayn Sniper",
    cardSource = ThunderstoneBase,
    cardType = HeroFeayn,
    cardIcon = CardIconHero 3,
    cardCount = 2,
    cardClasses = [ClassFighter,ClassArcher],
    cardLight = Just 2,
    cardVictoryPoints = Just 2,
    cardStrength = Just 6,
    cardPrice = Just 12,
    cardText = ["Cannot attack Rank 1.",
                "ATTACK +4",
                "Gain +1 XP if you defeat a Monster in Rank 3."],
    cardGlossary =
        ["Feayn: If a Dungeon Actions causes you to attack a Monster "
         ++ "in Rank 1, do not add the Feayn's Attack bonus to your "
         ++ "Attack Value.  If Feayn does not attack, his Light bonus "
         ++ "is lost."]
    }

heroDetails LoriggThief = initCardDetails {
    cardName = "Lorigg Thief",
    cardSource = ThunderstoneBase,
    cardType = HeroLorigg,
    cardIcon = CardIconHero 1,
    cardCount = 6,
    cardClasses = [ClassThief],
    cardGold = Just 2,
    cardLight = Just 1,
    cardStrength = Just 4,
    cardPrice = Just 5,
    cardLevelUp = Just 2,
    cardText = ["ATTACK +1"]
    }

heroDetails LoriggRogue = initCardDetails {
    cardName = "Lorigg Rogue",
    cardSource = ThunderstoneBase,
    cardType = HeroLorigg,
    cardIcon = CardIconHero 2,
    cardCount = 4,
    cardClasses = [ClassThief],
    cardGold = Just 3,
    cardLight = Just 1,
    cardStrength = Just 4,
    cardPrice = Just 8,
    cardLevelUp = Just 3,
    cardText = ["ATTACK +2","DUNGEON: All other players discard one card."],
    cardGlossary =
        ["Lorigg Outlaw or Rogue: Regardless of whether the battle "
         ++ "is victorious or not, all other players must discard "
         ++ "cards when this Hero enters the Dungeon."]
    }

heroDetails LoriggOutlaw = initCardDetails {
    cardName = "Lorigg Outlaw",
    cardSource = ThunderstoneBase,
    cardType = HeroLorigg,
    cardIcon = CardIconHero 3,
    cardCount = 2,
    cardClasses = [ClassThief],
    cardGold = Just 4,
    cardLight = Just 2,
    cardVictoryPoints = Just 1,
    cardStrength = Just 5,
    cardPrice = Just 10,
    cardText = ["ATTACK +2","DUNGEON: All other players discard one card."],
    cardGlossary =
        ["Lorigg Outlaw or Rogue: Regardless of whether the battle "
         ++ "is victorious or not, all other players must discard "
         ++ "cards when this Hero enters the Dungeon."]
    }

heroDetails OutlandsWarrior = initCardDetails {
    cardName = "Outlands Warrior",
    cardSource = ThunderstoneBase,
    cardType = HeroOutlands,
    cardIcon = CardIconHero 1,
    cardCount = 6,
    cardClasses = [ClassFighter],
    cardStrength = Just 7,
    cardPrice = Just 8,
    cardLevelUp = Just 2,
    cardText = ["ATTACK +3",
                "DUNGEON: Destroy one Food for an additional ATTACK +3."]
    }

heroDetails OutlandsSlayer = initCardDetails {
    cardName = "Outlands Slayer",
    cardSource = ThunderstoneBase,
    cardType = HeroOutlands,
    cardIcon = CardIconHero 2,
    cardCount = 4,
    cardClasses = [ClassFighter],
    cardStrength = Just 8,
    cardPrice = Just 11,
    cardLevelUp = Just 3,
    cardText = ["ATTACK +5",
                "DUNGEON: Gain +1 ATTACK for each Monster card "
                ++ "revealed from your hand.",
                "REPEAT DUNGEON: Destroy one Food for an additional "
                ++ "ATTACK +3."],
    cardGlossary =
        ["Outlands Slayer or Khan: The Hero gains an Attack bonus "
         ++ "for each Monster card revealed in your hand before the battle."]
    }

heroDetails OutlandsKhan = initCardDetails {
    cardName = "Outlands Khan",
    cardSource = ThunderstoneBase,
    cardType = HeroOutlands,
    cardIcon = CardIconHero 3,
    cardCount = 2,
    cardClasses = [ClassFighter],
    cardVictoryPoints = Just 3,
    cardStrength = Just 8,
    cardPrice = Just 13,
    cardText = ["ATTACK +7",
                "DUNGEON: ATTACK +2 for each Monster card revealed "
                ++ "from your hand."],
    cardGlossary =
        ["Outlands Slayer or Khan: The Hero gains an Attack bonus for "
         ++ "each Monster card revealed in your hand before the battle."]
    }

heroDetails RedbladeKiller = initCardDetails {
    cardName = "Redblade Killer",
    cardSource = ThunderstoneBase,
    cardType = HeroRedblade,
    cardIcon = CardIconHero 1,
    cardCount = 6,
    cardClasses = [ClassFighter,ClassThief],
    cardGold = Just 1,
    cardStrength = Just 5,
    cardPrice = Just 5,
    cardLevelUp = Just 2,
    cardText = ["ATTACK +2"]
    }

heroDetails RedbladePoisoner = initCardDetails {
    cardName = "Redblade Poisoner",
    cardSource = ThunderstoneBase,
    cardType = HeroRedblade,
    cardIcon = CardIconHero 2,
    cardCount = 4,
    cardClasses = [ClassFighter,ClassThief],
    cardGold = Just 2,
    cardStrength = Just 5,
    cardPrice = Just 8,
    cardLevelUp = Just 3,
    cardText = ["ATTACK +3",
                "DUNGEON: All other players discard one card."],
    cardGlossary =
        ["Redblade Assassin or Poisoner: Regardless of whether the "
         ++ "battle is victorious or not, all other players must "
         ++ "discard cards when this Hero enters the Dungeon."]
    }

heroDetails RedbladeAssassin = initCardDetails {
    cardName = "Redblade Assassin",
    cardSource = ThunderstoneBase,
    cardType = HeroRedblade,
    cardIcon = CardIconHero 3,
    cardCount = 2,
    cardClasses = [ClassFighter,ClassThief],
    cardGold = Just 2,
    cardVictoryPoints = Just 2,
    cardStrength = Just 6,
    cardPrice = Just 9,
    cardText = ["ATTACK +4",
                "DUNGEON: All other players discard one Hero or two cards."],
    cardGlossary =
        ["Redblade Assassin or Poisoner: Regardless of whether the "
         ++ "battle is victorious or not, all other players must "
         ++ "discard cards when this Hero enters the Dungeon."]
    }

heroDetails RegianCleric = initCardDetails {
    cardName = "Regian Cleric",
    cardSource = ThunderstoneBase,
    cardType = HeroRegian,
    cardIcon = CardIconHero 1,
    cardCount = 6,
    cardClasses = [ClassCleric],
    cardStrength = Just 4,
    cardPrice = Just 5,
    cardLevelUp = Just 2,
    cardText = ["MAGIC ATTACK +1",
                "REPEAT DUNGEON: Destroy one Disease to draw one card."],
    cardGlossary =
        ["Regian: You may continue to destroy Disease cards and draw "
         ++ "new cards ntil the battle begins."]
    }

heroDetails RegianPriest = initCardDetails {
    cardName = "Regian Priest",
    cardSource = ThunderstoneBase,
    cardType = HeroRegian,
    cardIcon = CardIconHero 2,
    cardCount = 4,
    cardClasses = [ClassCleric],
    cardStrength = Just 4,
    cardPrice = Just 8,
    cardLevelUp = Just 3,
    cardText = ["MAGIC ATTACK +2","DUNGEON: Draw one card.",
                "REPEAT DUNGEON: Destroy one Disease to draw one card."],
    cardGlossary =
        ["Regian: You may continue to destroy Disease cards and draw "
         ++ "new cards until the battle begins."]
    }

heroDetails RegianBishop = initCardDetails {
    cardName = "Regian Bishop",
    cardSource = ThunderstoneBase,
    cardType = HeroRegian,
    cardIcon = CardIconHero 3,
    cardCount = 2,
    cardClasses = [ClassCleric],
    cardVictoryPoints = Just 2,
    cardStrength = Just 5,
    cardPrice = Just 11,
    cardText = ["MAGIC ATTACK +3","DUNGEON: Draw two cards.",
                "REPEAT DUNGEON: Destroy one Disease to draw one card."],
    cardGlossary =
        ["Regian: You may continue to destroy Disease cards and draw "
         ++ "new cards until the battle begins."]
    }

heroDetails SelurinMagician = initCardDetails {
    cardName = "Selurin Magician",
    cardSource = ThunderstoneBase,
    cardType = HeroSelurin,
    cardIcon = CardIconHero 1,
    cardCount = 6,
    cardClasses = [ClassWizard],
    cardStrength = Just 2,
    cardPrice = Just 8,
    cardLevelUp = Just 2,
    cardText = ["MAGIC ATTACK +2",
                "All Items and Magic Attack Spells gain MAGIC ATTACK +1."],
    cardGlossary =
        ["Selurin: Each Spell with a Magic Attack bonus gains a "
         ++ "Magic Attack bonus of +1.  Each Item (with the Item "
         ++ "keyword), regardless of whether it has an Attack bonus "
         ++ "or not, gains a Magic Attack bonus of +1."]
    }

heroDetails SelurinWarlock = initCardDetails {
    cardName = "Selurin Warlock",
    cardSource = ThunderstoneBase,
    cardType = HeroSelurin,
    cardIcon = CardIconHero 2,
    cardCount = 4,
    cardClasses = [ClassWizard],
    cardStrength = Just 2,
    cardPrice = Just 10,
    cardLevelUp = Just 3,
    cardText = ["MAGIC ATTACK +2","Total MAGIC ATTACK x2* (apply last)"],
    cardGlossary =
        ["Selurin Theurge or Warlock: The x2 multiplier of the "
         ++ "Selurin Wizard affects only Magic Attack bonuses, "
         ++ "and is applied after all Magic Attack bonuses have "
         ++ "been calculated.  Multiple Wizards multiply together "
         ++ "(two become x4, three become x8, etc.)."]
    }

heroDetails SelurinTheurge = initCardDetails {
    cardName = "Selurin Theurge",
    cardSource = ThunderstoneBase,
    cardType = HeroSelurin,
    cardIcon = CardIconHero 3,
    cardCount = 2,
    cardClasses = [ClassWizard],
    cardVictoryPoints = Just 1,
    cardStrength = Just 3,
    cardPrice = Just 13,
    cardText = ["MAGIC ATTACK +2","Total MAGIC ATTACK x2* (apply last)",
                "DUNGEON: Each player discards one Hero or shows they "
                ++ "have none.  You may borrow one of those discarded "
                ++ "Heroes for the battle, returning it at the end."],
    cardGlossary =
        ["Selurin Theurge or Warlock: The x2 multiplier of the "
         ++ "Selurin Wizard affects only Magic Attack bonuses, and is "
         ++ "applied after all Magic Attack bonuses have been "
         ++ "calculated.  Multiple Wizards multiply together (two "
         ++ "become x4, three become x8, etc.).",
         "Selurin Theurge: If the borrowed Hero is destroyed by a "
         ++ "Battle Effect, it is not returned to the original owner.  "
         ++ "Instead, destroy the card."]
    }

heroDetails ThyrianSquire = initCardDetails {
    cardName = "Thyrian Squire",
    cardSource = ThunderstoneBase,
    cardType = HeroThyrian,
    cardIcon = CardIconHero 1,
    cardCount = 6,
    cardClasses = [ClassFighter],
    cardStrength = Just 6,
    cardPrice = Just 7,
    cardLevelUp = Just 2,
    cardText = ["ATTACK +2",
                "DUNGEON: Destroy one Food for additional ATTACK +2."],
    cardGlossary =
        ["Thyrian: Food destroyed by this Dungeon Effect cannot also "
         ++ "be used to gain a Strength bonus or for any other effect."]
    }

heroDetails ThyrianKnight = initCardDetails {
    cardName = "Thyrian Knight",
    cardSource = ThunderstoneBase,
    cardType = HeroThyrian,
    cardIcon = CardIconHero 2,
    cardCount = 4,
    cardClasses = [ClassFighter],
    cardStrength = Just 8,
    cardPrice = Just 9,
    cardLevelUp = Just 3,
    cardText = ["ATTACK +4","All Militia gain ATTACK +1.",
                "DUNGEON: Destroy one Food for additional ATTACK +2."],
    cardGlossary =
        ["Thyrian: Food destroyed by this Dungeon Effect cannot also "
         ++ "be used to gain a Strength bonus or for any other effect."]
    }

heroDetails ThyrianLord = initCardDetails {
    cardName = "Thyrian Lord",
    cardSource = ThunderstoneBase,
    cardType = HeroThyrian,
    cardIcon = CardIconHero 3,
    cardCount = 2,
    cardClasses = [ClassFighter],
    cardVictoryPoints = Just 2,
    cardStrength = Just 9,
    cardPrice = Just 11,
    cardText = ["ATTACK +4","All Heroes other than Fighters gain ATTACK +2.",
                "DUNGEON: Destroy one Food to place one Monster from "
                ++ "the hall worth 1 or 2 VP into your discard pile.  "
                ++ "Refill the hall."],
    cardGlossary =
        ["Thyrian: Food destroyed by this Dungeon Effect cannot also "
         ++ "be used to gain a Strength bonus or for any other effect.",
         "Thyrian Lord: You may only select a Monster with 1 or 2 VP, "
         ++ "and not 0 VP.  When a Monster is placed in your discard pile, "
         ++ "refill the Dungeon Hall.  If this results in a Breach effect, "
         ++ "resolve it immediately.  If the Thunderstone moves to Rank 1 "
         ++ "of the Dungeon Hall, the game ends immediately; you do not "
         ++ "collect the Thunderstone.  You do not earn any Experience "
         ++ "Points for the Effect."]
    }

heroDetails BlindNeophyte = initCardDetails {
    cardName = "Blind Neophyte",
    cardSource = WrathOfTheElements,
    cardType = HeroBlind,
    cardIcon = CardIconHero 1,
    cardCount = 6,
    cardClasses = [ClassFighter,ClassCleric],
    cardStrength = Just 4,
    cardPrice = Just 6,
    cardLevelUp = Just 2,
    cardText = ["ATTACK +2",
                "REPEAT DUNGEON: Destroy one Light Item to gain "
                ++ "MAGIC ATTACK +3."],
    cardGlossary = []
    }

heroDetails BlindMonk = initCardDetails {
    cardName = "Blind Monk",
    cardSource = WrathOfTheElements,
    cardType = HeroBlind,
    cardIcon = CardIconHero 2,
    cardCount = 4,
    cardClasses = [ClassFighter,ClassCleric],
    cardStrength = Just 4,
    cardPrice = Just 9,
    cardLevelUp = Just 3,
    cardText = ["ATTACK +3",
                "REPEAT DUNGEON: Destroy one Light Item to gain "
                ++ "MAGIC ATTACK +4."],
    cardGlossary = []
    }

heroDetails BlindGrandmaster = initCardDetails {
    cardName = "Blind Grandmaster",
    cardSource = WrathOfTheElements,
    cardType = HeroBlind,
    cardIcon = CardIconHero 3,
    cardCount = 2,
    cardClasses = [ClassFighter,ClassCleric],
    cardStrength = Just 5,
    cardPrice = Just 12,
    cardVictoryPoints = Just 3,
    cardText = ["ATTACK +3",
                "Ignore all Light Penalties",
                "MAGIC ATTACK +3 if no Light Items are revealed."],
    cardGlossary = []
    }

heroDetails DiinIllusionist = initCardDetails {
    cardName = "Diin Illusionist",
    cardSource = WrathOfTheElements,
    cardType = HeroDiin,
    cardIcon = CardIconHero 1,
    cardCount = 6,
    cardClasses = [ClassWizard],
    cardStrength = Just 3,
    cardPrice = Just 5,
    cardLevelUp = Just 2,
    cardText = ["MAGIC ATTACK +2",
                "If another player causes you to discard this card, "
                ++ "return it to your hand."],
    cardGlossary = []
    }

heroDetails DiinBeguiler = initCardDetails {
    cardName = "Diin Beguiler",
    cardSource = WrathOfTheElements,
    cardType = HeroDiin,
    cardIcon = CardIconHero 2,
    cardCount = 4,
    cardClasses = [ClassWizard],
    cardStrength = Just 3,
    cardPrice = Just 8,
    cardLevelUp = Just 3,
    cardText = ["MAGIC ATTACK +2",
                "DUNGEON: One Militia duplicates all abilities of one "
                ++ "other Hero in your hand."],
    cardGlossary = []
    }

heroDetails DiinEnchanter = initCardDetails {
    cardName = "Diin Enchanter",
    cardSource = WrathOfTheElements,
    cardType = HeroDiin,
    cardIcon = CardIconHero 3,
    cardCount = 2,
    cardClasses = [ClassWizard],
    cardStrength = Just 4,
    cardPrice = Just 11,
    cardVictoryPoints = Just 2,
    cardText = ["MAGIC ATTACK +3",
                "DUNGEON: Discard one Militia to draw the top Hero from "
                ++ "any Village stack.  That Hero joins the battle.  "
                ++ "Destroy the Hero at the end of the battle."],
    cardGlossary = []
    }

heroDetails DivineHealer = initCardDetails {
    cardName = "Divine Healer",
    cardSource = WrathOfTheElements,
    cardType = HeroDivine,
    cardIcon = CardIconHero 1,
    cardCount = 6,
    cardClasses = [ClassCleric],
    cardStrength = Just 4,
    cardPrice = Just 5,
    cardLevelUp = Just 2,
    cardText = ["ATTACK +2",
                "VILLAGE/DUNGEON: Destroy one Disease card to "
                ++ "draw one card."],
    cardGlossary = []
    }

heroDetails DivineChaplain = initCardDetails {
    cardName = "Divine Chaplain",
    cardSource = WrathOfTheElements,
    cardType = HeroDivine,
    cardIcon = CardIconHero 2,
    cardCount = 4,
    cardClasses = [ClassCleric],
    cardStrength = Just 5,
    cardPrice = Just 8,
    cardLevelUp = Just 3,
    cardText = ["ATTACK +2",
                "DUNGEON: Gain Attack +1 for each Disease or Monster "
                ++ "card revealed from your hand.",
                "VILLAGE/DUNGEON: Draw one card."],
    cardGlossary = []
    }

heroDetails DivineProphet = initCardDetails {
    cardName = "Divine Prophet",
    cardSource = WrathOfTheElements,
    cardType = HeroDivine,
    cardIcon = CardIconHero 3,
    cardCount = 2,
    cardClasses = [ClassCleric],
    cardStrength = Just 6,
    cardPrice = Just 11,
    cardVictoryPoints = Just 2,
    cardText = ["ATTACK +3",
                "VILLAGE/DUNGEON: Destroy one Disease card to "
                ++ "draw two cards.",
                "VILLAGE/DUNGEON: Draw two card cards for each Monster "
                ++ "card revealed from your hand."],
    cardGlossary = []
    }

heroDetails GanglandThug = initCardDetails {
    cardName = "Gangland Thug",
    cardSource = WrathOfTheElements,
    cardType = HeroGangland,
    cardIcon = CardIconHero 1,
    cardCount = 6,
    cardClasses = [ClassThief],
    cardStrength = Just 4,
    cardPrice = Just 6,
    cardLevelUp = Just 2,
    cardText = ["ATTACK +1",
                "VILLAGE: Gain +1 gold or every other Hero revealed from "
                ++ "your hand."],
    cardGlossary = []
    }

heroDetails GanglandHeavy = initCardDetails {
    cardName = "Gangland Heavy",
    cardSource = WrathOfTheElements,
    cardType = HeroGangland,
    cardIcon = CardIconHero 2,
    cardCount = 4,
    cardClasses = [ClassThief],
    cardGold = Just 1,
    cardStrength = Just 4,
    cardPrice = Just 9,
    cardLevelUp = Just 3,
    cardText = ["ATTACK +2",
                "VILLAGE: Gain +1 gold or every other Hero revealed from "
                ++ "your hand.",
                "DUNGEON: All other players discard one card with a gold "
                ++ "value or reveals a hand containing none."],
    cardGlossary = []
    }

heroDetails GanglandCrook = initCardDetails {
    cardName = "Gangland Crook",
    cardSource = WrathOfTheElements,
    cardType = HeroGangland,
    cardIcon = CardIconHero 3,
    cardCount = 2,
    cardClasses = [ClassThief],
    cardGold = Just 1,
    cardStrength = Just 5,
    cardPrice = Just 11,
    cardVictoryPoints = Just 1,
    cardText = ["ATTACK +3",
                "DUNGEON: Additional ATTACK +4 if your revealed cards' "
                ++ "gold value total five or more."],
    cardGlossary = []
    }

heroDetails GohlenTrapper = initCardDetails {
    cardName = "Gohlen Trapper",
    cardSource = WrathOfTheElements,
    cardType = HeroGohlen,
    cardIcon = CardIconHero 1,
    cardCount = 6,
    cardClasses = [ClassFighter],
    cardStrength = Just 5,
    cardPrice = Just 7,
    cardLevelUp = Just 2,
    cardText = ["ATTACK +2",
                "DUNGEON: ATTACK +2 if you reveal a Monster card "
                ++ "from your hand."],
    cardGlossary = []
    }

heroDetails GohlenTracker = initCardDetails {
    cardName = "Gohlen Tracker",
    cardSource = WrathOfTheElements,
    cardType = HeroGohlen,
    cardIcon = CardIconHero 2,
    cardCount = 4,
    cardClasses = [ClassFighter],
    cardStrength = Just 6,
    cardPrice = Just 10,
    cardLevelUp = Just 3,
    cardText = ["ATTACK +3",
                "DUNGEON: ATTACK +3 if you reveal a Monster card "
                ++ "from your hand."],
    cardGlossary = []
    }

heroDetails GohlenHunter = initCardDetails {
    cardName = "Gohlen Hunter",
    cardSource = WrathOfTheElements,
    cardType = HeroGohlen,
    cardIcon = CardIconHero 3,
    cardCount = 2,
    cardClasses = [ClassFighter],
    cardStrength = Just 6,
    cardPrice = Just 13,
    cardVictoryPoints = Just 1,
    cardText = ["ATTACK +4",
                "DUNGEON: Each player discards one Monster card or reveals "
                ++ "a hand containing none.  If you defeat a Monster this "
                ++ "turn, you may place one of the discarded Monsters "
                ++ "(worth up to 5 VP) onto your discard pile if its type "
                ++ "matches that of the Monster you defeated."],
    cardGlossary = []
    }

heroDetails RunespawnAdept = initCardDetails {
    cardName = "Runespawn Adept",
    cardSource = WrathOfTheElements,
    cardType = HeroRunespawn,
    cardIcon = CardIconHero 1,
    cardCount = 6,
    cardClasses = [ClassThief,ClassWizard],
    cardStrength = Just 5,
    cardPrice = Just 7,
    cardLevelUp = Just 2,
    cardText = ["MAGIC ATTACK +2",
                "DUNGEON: All other Heroes gain Strength +2."],
    cardGlossary = []
    }

heroDetails RunespawnSiren = initCardDetails {
    cardName = "Runespawn Siren",
    cardSource = WrathOfTheElements,
    cardType = HeroRunespawn,
    cardIcon = CardIconHero 2,
    cardCount = 4,
    cardClasses = [ClassThief,ClassWizard],
    cardStrength = Just 6,
    cardPrice = Just 10,
    cardLevelUp = Just 3,
    cardText = ["MAGIC ATTACK +3",
                "DUNGEON: All other Heroes gain Strength +3.",
                "DUNGEON: Draw one card."],
    cardGlossary = []
    }

heroDetails RunespawnWitch = initCardDetails {
    cardName = "Runespawn Witch",
    cardSource = WrathOfTheElements,
    cardType = HeroRunespawn,
    cardIcon = CardIconHero 3,
    cardCount = 2,
    cardClasses = [ClassThief,ClassWizard],
    cardStrength = Just 8,
    cardPrice = Just 13,
    cardVictoryPoints = Just 2,
    cardText = ["MAGIC ATTACK +3",
                "DUNGEON: All other Heroes gain Strength +4.",
                "DUNGEON: All other players discard one card.  "
                ++ "Runespawn Witch gains Magic Attack +2 unless at least "
                ++ "one Spell was discarded."],
    cardGlossary = []
    }

heroDetails TorynScrapper = initCardDetails {
    cardName = "Toryn Scrapper",
    cardSource = WrathOfTheElements,
    cardType = HeroToryn,
    cardIcon = CardIconHero 1,
    cardCount = 6,
    cardClasses = [ClassFighter],
    cardStrength = Just 7,
    cardPrice = Just 7,
    cardLevelUp = Just 2,
    cardText = ["ATTACK +2",
                "This Hero may equip any number of Weapons."],
    cardGlossary = []
    }

heroDetails TorynDuelist = initCardDetails {
    cardName = "Toryn Duelist",
    cardSource = WrathOfTheElements,
    cardType = HeroToryn,
    cardIcon = CardIconHero 2,
    cardCount = 4,
    cardClasses = [ClassFighter],
    cardStrength = Just 8,
    cardPrice = Just 9,
    cardLevelUp = Just 3,
    cardText = ["ATTACK +3",
                "This Hero may equip any number of Weapons."],
    cardGlossary = []
    }

heroDetails TorynGladiator = initCardDetails {
    cardName = "Toryn Gladiator",
    cardSource = WrathOfTheElements,
    cardType = HeroToryn,
    cardIcon = CardIconHero 3,
    cardCount = 2,
    cardClasses = [ClassFighter],
    cardStrength = Just 9,
    cardPrice = Just 12,
    cardVictoryPoints = Just 2,
    cardText = ["ATTACK +4",
                "This Hero may equip any number of Weapons.",
                "DUNGEON: Each player discards one Weapon or reveals a "
                ++ "hand containing none.  You may equip one discarded "
                ++ "Weapon to this Hero, returning it to its owner after "
                ++ "the battle."],
    cardGlossary = []
    }

heroDetails DeepMiner = initCardDetails {
    cardName = "Deep Miner",
    cardSource = DoomgateLegion,
    cardType = HeroDeep,
    cardIcon = CardIconHero 1,
    cardCount = 6,
    cardClasses = [ClassFighter],
    cardStrength = Just 7,
    cardPrice = Just 6,
    cardLevelUp = Just 2,
    cardText = ["ATTACK +3",
                "ATTACK -2 if Deep Miner has a weapon equipped.",
                "ATTACK +1 for each mercenary revealed."],
    cardGlossary = []
    }

heroDetails DeepDigger = initCardDetails {
    cardName = "Deep Digger",
    cardSource = DoomgateLegion,
    cardType = HeroDeep,
    cardIcon = CardIconHero 2,
    cardCount = 4,
    cardClasses = [ClassFighter],
    cardStrength = Just 8,
    cardPrice = Just 9,
    cardLevelUp = Just 3,
    cardText = ["ATTACK +4",
                "ATTACK -3 if Deep Digger has a weapon equipped.",
                "ATTACK +1 for each mercenary revealed."],
    cardGlossary = []
    }

heroDetails DeepWrecker = initCardDetails {
    cardName = "Deep Wrecker",
    cardSource = DoomgateLegion,
    cardType = HeroDeep,
    cardIcon = CardIconHero 3,
    cardCount = 2,
    cardClasses = [ClassFighter],
    cardStrength = Just 9,
    cardPrice = Just 11,
    cardVictoryPoints = Just 3,
    cardText = ["ATTACK +5",
                "ATTACK -3 if Deep Wrecker has a weapon equipped.",
                "DUNGEON: Draw two cards for each mercenary revealed."],
    cardGlossary = []
    }

heroDetails DrunariOrphan = initCardDetails {
    cardName = "Drunari Orphan",
    cardSource = DoomgateLegion,
    cardType = HeroDrunari,
    cardIcon = CardIconHero 1,
    cardCount = 6,
    cardClasses = [ClassThief],
    cardGold = Just 1,
    cardStrength = Just 3,
    cardPrice = Just 5,
    cardLight = Just 1,
    cardLevelUp = Just 2,
    cardText = ["ATTACK +1"],
    cardGlossary = []
    }

heroDetails DrunariVagabond = initCardDetails {
    cardName = "Drunari Vagabond",
    cardSource = DoomgateLegion,
    cardType = HeroDrunari,
    cardIcon = CardIconHero 2,
    cardCount = 4,
    cardClasses = [ClassThief],
    cardGold = Just 2,
    cardStrength = Just 4,
    cardPrice = Just 8,
    cardLight = Just 1,
    cardLevelUp = Just 3,
    cardText = ["ATTACK +2",
                "ATTACK +1 for each revealed card worth 3 or more Gold.",
                 "Spoils (Villager)."],
    cardGlossary = []
    }

heroDetails DrunariGypsy = initCardDetails {
    cardName = "Drunari Gypsy",
    cardSource = DoomgateLegion,
    cardType = HeroDrunari,
    cardIcon = CardIconHero 3,
    cardCount = 2,
    cardClasses = [ClassThief],
    cardGold = Just 3,
    cardStrength = Just 5,
    cardPrice = Just 11,
    cardLight = Just 2,
    cardVictoryPoints = Just 2,
    cardText = ["ATTACK +3",
                "DUNGEON: Draw one card.",
                "SPOILS: Draw one extra card when you refill your hand."],
    cardGlossary = []
    }

heroDetails SidheNatural = initCardDetails {
    cardName = "Sidhe Natural",
    cardSource = DoomgateLegion,
    cardType = HeroSidhe,
    cardIcon = CardIconHero 1,
    cardCount = 6,
    cardClasses = [ClassCleric],
    cardStrength = Just 3,
    cardPrice = Just 6,
    cardLevelUp = Just 2,
    cardText = ["MAGIC ATTACK +1",
                "When you gain a Disease, place it in another player's "
                ++ "discard pile instead."],
    cardGlossary = ["If chosen, use the Special Disease deck."]
    }

heroDetails SidheDruid = initCardDetails {
    cardName = "Sidhe Druid",
    cardSource = DoomgateLegion,
    cardType = HeroSidhe,
    cardIcon = CardIconHero 2,
    cardCount = 4,
    cardClasses = [ClassCleric],
    cardStrength = Just 4,
    cardPrice = Just 9,
    cardLevelUp = Just 3,
    cardText = ["MAGIC ATTACK +2",
                "When you gain a Disease, place it in another player's "
                ++ "discard pile instead.",
                "VILLAGE/DUNGEON: Discard one Disease to draw one card."],
    cardGlossary = ["If chosen, use the Special Disease deck."]
    }

heroDetails SidheSpirit = initCardDetails {
    cardName = "Sidhe Spirit",
    cardSource = DoomgateLegion,
    cardType = HeroSidhe,
    cardIcon = CardIconHero 3,
    cardCount = 2,
    cardClasses = [ClassCleric],
    cardStrength = Just 5,
    cardPrice = Just 12,
    cardVictoryPoints = Just 2,
    cardText = ["MAGIC ATTACK +3",
                "DUNGEON: Place one Disease each under any monsters.  "
                ++ "Add +1 Health to the monster for each Disease.  "
                ++ "If a player attacks a monster with Diseases, the "
                ++ "player gains the Diseases at the end of the battle."],
    cardGlossary = ["If chosen, use the Special Disease deck."]
    }

heroDetails SlynnBowman = initCardDetails {
    cardName = "Slynn Bowman",
    cardSource = DoomgateLegion,
    cardType = HeroSlynn,
    cardIcon = CardIconHero 1,
    cardCount = 6,
    cardClasses = [ClassFighter,ClassArcher],
    cardStrength = Just 5,
    cardPrice = Just 6,
    cardLevelUp = Just 2,
    cardText = ["ATTACK +2",
                "ATTACK +3 against rank 2 or 3 if you reveal a monster "
                ++ "of the same type as the one attacked."],
    cardGlossary = []
    }

heroDetails SlynnBowmaster = initCardDetails {
    cardName = "Slynn Bowmaster",
    cardSource = DoomgateLegion,
    cardType = HeroSlynn,
    cardIcon = CardIconHero 2,
    cardCount = 4,
    cardClasses = [ClassFighter,ClassArcher],
    cardStrength = Just 6,
    cardPrice = Just 9,
    cardLevelUp = Just 3,
    cardText = ["ATTACK +2",
                "ATTACK +3 against rank 2 or 3 if you reveal a monster "
                ++ "of the same type as the one attacked."],
    cardGlossary = []
    }

heroDetails SlynnLongbowman = initCardDetails {
    cardName = "Slynn Longbowman",
    cardSource = DoomgateLegion,
    cardType = HeroSlynn,
    cardIcon = CardIconHero 3,
    cardCount = 2,
    cardClasses = [ClassFighter,ClassArcher],
    cardStrength = Just 6,
    cardPrice = Just 12,
    cardVictoryPoints = Just 2,
    cardText = ["ATTACK +3",
                "ATTACK +5 against rank 2 or 3",
                "DUNGEON: You may swap a monster from you hand with a "
                ++ "monster worth up to 1 VP more from the hall."],
    cardGlossary = []
    }

heroDetails TempestAvenger = initCardDetails {
    cardName = "Tempest Avenger",
    cardSource = DoomgateLegion,
    cardType = HeroTempest,
    cardIcon = CardIconHero 1,
    cardCount = 6,
    cardClasses = [ClassFighter],
    cardStrength = Just 6,
    cardPrice = Just 5,
    cardLevelUp = Just 2,
    cardText = ["ATTACK +2"],
    cardGlossary = []
    }

heroDetails TempestReaver = initCardDetails {
    cardName = "Tempest Reaver",
    cardSource = DoomgateLegion,
    cardType = HeroTempest,
    cardIcon = CardIconHero 2,
    cardCount = 4,
    cardClasses = [ClassFighter],
    cardStrength = Just 7,
    cardPrice = Just 8,
    cardLevelUp = Just 3,
    cardText = ["ATTACK +3",
                "If you defeat a monster worth less than 4 VP, you may "
                ++ "put this Hero on top of your deck instead of "
                ++ "discarding it at the end of the turn."],
    cardGlossary = []
    }

heroDetails TempestWarden = initCardDetails {
    cardName = "Tempest Warden",
    cardSource = DoomgateLegion,
    cardType = HeroTempest,
    cardIcon = CardIconHero 3,
    cardCount = 2,
    cardClasses = [ClassFighter],
    cardStrength = Just 8,
    cardPrice = Just 12,
    cardVictoryPoints = Just 2,
    cardText = ["ATTACK +4",
                "If you defeat a monster worth less than 4 VP, you may "
                ++ "add this Hero to your newly drawn hand instead of "
                ++ "discarding it at the end of the turn."],
    cardGlossary = []
    }

heroDetails TholisMedium = initCardDetails {
    cardName = "Tholis Medium",
    cardSource = DoomgateLegion,
    cardType = HeroTholis,
    cardIcon = CardIconHero 1,
    cardCount = 6,
    cardClasses = [ClassThief,ClassWizard],
    cardGold = Just 1,
    cardStrength = Just 3,
    cardPrice = Just 7,
    cardLevelUp = Just 2,
    cardText = ["MAGIC ATTACK +1",
                "DUNGEON: Look at the top three cards of the Dungeon "
                ++ "deck and return them in any order."],
    cardGlossary = []
    }

heroDetails TholisClairvoyant = initCardDetails {
    cardName = "Tholis Clairvoyant",
    cardSource = DoomgateLegion,
    cardType = HeroTholis,
    cardIcon = CardIconHero 2,
    cardCount = 4,
    cardClasses = [ClassThief,ClassWizard],
    cardGold = Just 1,
    cardStrength = Just 4,
    cardPrice = Just 10,
    cardLevelUp = Just 3,
    cardText = ["MAGIC ATTACK +2",
                "DUNGEON: Look at the top three cards of the Dungeon "
                ++ "deck.  You may replace the rank 1 monster with one "
                ++ "of these and return the remaining cards to the top "
                ++ "of the deck in any order."],
    cardGlossary = []
    }

heroDetails TholisOracle = initCardDetails {
    cardName = "Tholis Oracle",
    cardSource = DoomgateLegion,
    cardType = HeroTholis,
    cardIcon = CardIconHero 3,
    cardCount = 2,
    cardClasses = [ClassThief,ClassWizard],
    cardGold = Just 1,
    cardStrength = Just 4,
    cardPrice = Just 13,
    cardVictoryPoints = Just 2,
    cardText = ["MAGIC ATTACK +3",
                "DUNGEON: Each other player reveals the top five cards of "
                ++ "their deck and discards any Heroes, then places the "
                ++ "remaining cards on the top of his deck in any order."],
    cardGlossary = []
    }

heroDetails VerdanMinstrel = initCardDetails {
    cardName = "Verdan Minstrel",
    cardSource = DoomgateLegion,
    cardType = HeroVerdan,
    cardIcon = CardIconHero 1,
    cardCount = 6,
    cardClasses = [ClassFighter,ClassThief],
    cardGold = Just 1,
    cardStrength = Just 5,
    cardPrice = Just 7,
    cardLight = Just 1,
    cardLevelUp = Just 2,
    cardText = ["ATTACK +2",
                "DUNGEON: Every other player with 6 or more cards discards "
                ++ "one card at random."],
    cardGlossary = []
    }

heroDetails VerdanBard = initCardDetails {
    cardName = "Verdan Bard",
    cardSource = DoomgateLegion,
    cardType = HeroVerdan,
    cardIcon = CardIconHero 2,
    cardCount = 4,
    cardClasses = [ClassFighter,ClassThief],
    cardGold = Just 2,
    cardStrength = Just 5,
    cardPrice = Just 10,
    cardLight = Just 1,
    cardLevelUp = Just 3,
    cardText = ["ATTACK +3",
                "DUNGEON: Destroy 2 XP to place the rank 1 monster on the "
                ++ "bottom of the Dungeon Deck and refill the hall.",
                "DUNGEON: Every other player reveals one spell or discards "
                ++ "one card."],
    cardGlossary = []
    }

heroDetails VerdanTroubadour = initCardDetails {
    cardName = "Verdan Troubador",
    cardSource = DoomgateLegion,
    cardType = HeroVerdan,
    cardIcon = CardIconHero 3,
    cardCount = 2,
    cardClasses = [ClassFighter,ClassThief],
    cardGold = Just 2,
    cardStrength = Just 5,
    cardPrice = Just 12,
    cardLight = Just 1,
    cardVictoryPoints = Just 1,
    cardText = ["ATTACK +4",
                "DUNGEON: Destroy 2 XP to place any monster from the "
                ++ "Dungeon Hall on the bottom of the Dungeon Deck and "
                ++ "refill the hall.",
                "DUNGEON: Each other player loses one XP."],
    cardGlossary = []
    }

heroDetails PhalanxFootman = initCardDetails {
    cardName = "Phalanx Footman",
    cardSource = Dragonspire,
    cardType = HeroPhalanx,
    cardIcon = CardIconHero 1,
    cardCount = 12,
    cardClasses = [ClassFighter],
    cardStrength = Just 5,
    cardPrice = Just 5,
    cardLevelUp = Just 6,
    cardText = ["ATTACK +2",
                "Additional ATTACK +1 and STRENGTH +1 for each other "
                ++ "Phalanx you reveal."],
    cardGlossary = []
    }

heroDetails PhalanxOfficer = initCardDetails {
    cardName = "Phalanx Officer",
    cardSource = Dragonspire,
    cardType = HeroPhalanx,
    cardIcon = CardIconHero 2,
    cardCount = 3,
    cardClasses = [ClassFighter],
    cardStrength = Just 7,
    cardPrice = Just 13,
    cardVictoryPoints = Just 1,
    cardText = ["ATTACK +4",
                "VILLAGE/DUNGEON: Draw 1 card for each other Phalanx "
                ++ "you reveal."],
    cardGlossary = []
    }

heroDetails BelzurCurate = initCardDetails {
    cardName = "Belzur Curate",
    cardSource = Dragonspire,
    cardType = HeroBelzur,
    cardIcon = CardIconHero 1,
    cardCount = 6,
    cardClasses = [ClassCleric],
    cardStrength = Just 4,
    cardPrice = Just 5,
    cardLevelUp = Just 2,
    cardText = ["MAGIC ATTACK +1",
                "DUNGEON: Draw 1 card.",
                "VILLAGE: Destroy 1 Disease to gain 2 gold."],
    cardGlossary = []
    }

heroDetails BelzurBishop = initCardDetails {
    cardName = "Belzur Bishop",
    cardSource = Dragonspire,
    cardType = HeroBelzur,
    cardIcon = CardIconHero 2,
    cardCount = 4,
    cardClasses = [ClassCleric],
    cardStrength = Just 5,
    cardPrice = Just 8,
    cardLevelUp = Just 3,
    cardText = ["MAGIC ATTACK +2",
                "If an opponent's effect would target you, reveal "
                ++ "this Hero and the effect targets the active player "
                ++ "instead.",
                "DUNGEON: Draw 1 card."],
    cardGlossary = []
    }

heroDetails BelzurCardinal = initCardDetails {
    cardName = "Belzur Cardinal",
    cardSource = Dragonspire,
    cardType = HeroBelzur,
    cardIcon = CardIconHero 3,
    cardCount = 2,
    cardClasses = [ClassCleric],
    cardStrength = Just 5,
    cardPrice = Just 11,
    cardVictoryPoints = Just 2,
    cardText = ["MAGIC ATTACK +3",
                "DUNGEON: Draw 1 card.",
                "VILLAGE/DUNGEON: Destroy a Disease to draw 3 cards."],
    cardGlossary = []
    }

heroDetails CabalAstrologer = initCardDetails {
    cardName = "Cabal Astrologer",
    cardSource = Dragonspire,
    cardType = HeroCabal,
    cardIcon = CardIconHero 1,
    cardCount = 6,
    cardClasses = [ClassWizard],
    cardStrength = Just 3,
    cardPrice = Just 7,
    cardLevelUp = Just 2,
    cardText = ["MAGIC ATTACK +1",
                "Additional MAGIC ATTACK +1 for each Spell you play."],
    cardGlossary = []
    }

heroDetails CabalSage = initCardDetails {
    cardName = "Cabal Sage",
    cardSource = Dragonspire,
    cardType = HeroCabal,
    cardIcon = CardIconHero 2,
    cardCount = 4,
    cardClasses = [ClassWizard],
    cardStrength = Just 3,
    cardPrice = Just 10,
    cardLevelUp = Just 3,
    cardText = ["MAGIC ATTACK +2",
                "Additional MAGIC ATTACK +2 for each Spell you play."],
    cardGlossary = []
    }

heroDetails CabalMaster = initCardDetails {
    cardName = "Cabal Master",
    cardSource = Dragonspire,
    cardType = HeroCabal,
    cardIcon = CardIconHero 3,
    cardCount = 2,
    cardClasses = [ClassWizard],
    cardStrength = Just 4,
    cardPrice = Just 13,
    cardVictoryPoints = Just 2,
    cardText = ["MAGIC ATTACK +3",
                "Additional MAGIC ATTACK +3 for each Spell you play.",
                "SPOILS: Buy a Spell."],
    cardGlossary = []
    }

heroDetails ChulianRat = initCardDetails {
    cardName = "Chulian Rat",
    cardSource = Dragonspire,
    cardType = HeroChulian,
    cardIcon = CardIconHero 1,
    cardCount = 6,
    cardClasses = [ClassThief],
    cardGold = Just 1,
    cardStrength = Just 4,
    cardPrice = Just 5,
    cardLight = Just 1,
    cardLevelUp = Just 2,
    cardText = ["ATTACK +1"],
    cardGlossary = []
    }

heroDetails ChulianScavenger = initCardDetails {
    cardName = "Chulian Scavenger",
    cardSource = Dragonspire,
    cardType = HeroChulian,
    cardIcon = CardIconHero 2,
    cardCount = 4,
    cardClasses = [ClassThief],
    cardGold = Just 1,
    cardStrength = Just 4,
    cardPrice = Just 8,
    cardLight = Just 2,
    cardLevelUp = Just 3,
    cardText = ["ATTACK +2",
                "DUNGEON: Each other player discards a Weapon or "
                ++ "reveals and hand containing none."],
    cardGlossary = []
    }

heroDetails ChulianLooter = initCardDetails {
    cardName = "Chulian Looter",
    cardSource = Dragonspire,
    cardType = HeroChulian,
    cardIcon = CardIconHero 3,
    cardCount = 2,
    cardClasses = [ClassThief],
    cardGold = Just 2,
    cardStrength = Just 5,
    cardPrice = Just 11,
    cardLight = Just 2,
    cardVictoryPoints = Just 2,
    cardText = ["ATTACK +3",
                "DUNGEON: Each other player discards a Weapon or "
                ++ "reveals and hand containing none.  Additional "
                ++ "ATTACK +1 for each Weapon discarded in this manner."],
    cardGlossary = []
    }

heroDetails EvokerAdept = initCardDetails {
    cardName = "Evoker Adept",
    cardSource = Dragonspire,
    cardType = HeroEvoker,
    cardIcon = CardIconHero 1,
    cardCount = 6,
    cardClasses = [ClassArcher,ClassWizard],
    cardStrength = Just 4,
    cardPrice = Just 6,
    cardLevelUp = Just 2,
    cardText = ["ATTACK +2 against Rank 2 or higher.",
                "LIGHT +1 if you have a Light Item."],
    cardGlossary = []
    }

heroDetails EvokerScorcher = initCardDetails {
    cardName = "Evoker Scorcher",
    cardSource = Dragonspire,
    cardType = HeroEvoker,
    cardIcon = CardIconHero 2,
    cardCount = 4,
    cardClasses = [ClassArcher,ClassWizard],
    cardStrength = Just 5,
    cardPrice = Just 9,
    cardLevelUp = Just 3,
    cardText = ["ATTACK +3 against Rank 2 or higher.",
                "DUNGEON: Destroy 1 XP for MAGIC ATTACK +2 and LIGHT +2."],
    cardGlossary = []
    }

heroDetails EvokerPyroclast = initCardDetails {
    cardName = "Evoker Pyroclast",
    cardSource = Dragonspire,
    cardType = HeroEvoker,
    cardIcon = CardIconHero 3,
    cardCount = 2,
    cardClasses = [ClassArcher,ClassWizard],
    cardStrength = Just 6,
    cardPrice = Just 11,
    cardVictoryPoints = Just 2,
    cardText = ["ATTACK +4 against Rank 2 or higher.",
                "DUNGEON: Destroy 1 XP for MAGIC ATTACK +4 and LIGHT +3."],
    cardGlossary = []
    }

heroDetails FlameWatch = initCardDetails {
    cardName = "Flame Watch",
    cardSource = Dragonspire,
    cardType = HeroFlame,
    cardIcon = CardIconHero 1,
    cardCount = 6,
    cardClasses = [ClassFighter,ClassWizard],
    cardStrength = Just 5,
    cardPrice = Just 6,
    cardLight = Just 1,
    cardLevelUp = Just 2,
    cardText = ["ATTACK +2"],
    cardGlossary = []
    }

heroDetails FlameGuard = initCardDetails {
    cardName = "Flame Guard",
    cardSource = Dragonspire,
    cardType = HeroFlame,
    cardIcon = CardIconHero 2,
    cardCount = 4,
    cardClasses = [ClassFighter,ClassWizard],
    cardStrength = Just 5,
    cardPrice = Just 9,
    cardLight = Just 1,
    cardLevelUp = Just 3,
    cardText = ["ATTACK +3",
                "Additional MAGIC ATTACK +1 for each extra Light you "
                ++ "have above penalties."],
    cardGlossary = []
    }

heroDetails FlameHero = initCardDetails {
    cardName = "Flame Hero",
    cardSource = Dragonspire,
    cardType = HeroFlame,
    cardIcon = CardIconHero 3,
    cardCount = 2,
    cardClasses = [ClassFighter,ClassWizard],
    cardStrength = Just 6,
    cardPrice = Just 12,
    cardLight = Just 2,
    cardVictoryPoints = Just 1,
    cardText = ["ATTACK +3",
                "Additional MAGIC ATTACK +2 for each extra Light you "
                ++ "have above penalties.",
                "SPOILS: Buy a Light Item."],
    cardGlossary = []
    }

heroDetails GorinthAmateur = initCardDetails {
    cardName = "Gorinth Amateur",
    cardSource = Dragonspire,
    cardType = HeroGorinth,
    cardIcon = CardIconHero 1,
    cardCount = 6,
    cardClasses = [ClassWizard],
    cardStrength = Just 3,
    cardPrice = Just 5,
    cardLevelUp = Just 2,
    cardText = ["MAGIC ATTACK +2"],
    cardGlossary = []
    }

heroDetails GorinthHoarder = initCardDetails {
    cardName = "Gorinth Hoarder",
    cardSource = Dragonspire,
    cardType = HeroGorinth,
    cardIcon = CardIconHero 2,
    cardCount = 4,
    cardClasses = [ClassWizard],
    cardStrength = Just 4,
    cardPrice = Just 8,
    cardLevelUp = Just 3,
    cardText = ["MAGIC ATTACK +3",
                "DUNGEON: Destroy 1 card to gain MAGIC ATTACK equal "
                ++ "to its Gold Value."],
    cardGlossary = []
    }

heroDetails GorinthMiser = initCardDetails {
    cardName = "Gorinth Miser",
    cardSource = Dragonspire,
    cardType = HeroGorinth,
    cardIcon = CardIconHero 3,
    cardCount = 2,
    cardClasses = [ClassWizard],
    cardGold = Just 1,
    cardStrength = Just 5,
    cardPrice = Just 11,
    cardVictoryPoints = Just 2,
    cardText = ["MAGIC ATTACK +1 for each card with a Gold Value that "
                ++ "you reveal (including Miser).",
                "DUNGEON: Destroy 1 card without a Gold Value "
                ++ "to draw 2 cards."],
    cardGlossary = []
    }

heroDetails HalfOrcRaider = initCardDetails {
    cardName = "Half-Orc Raider",
    cardSource = Dragonspire,
    cardType = HeroHalfOrc,
    cardIcon = CardIconHero 1,
    cardCount = 6,
    cardClasses = [ClassFighter,ClassThief],
    cardGold = Just 1,
    cardStrength = Just 4,
    cardPrice = Just 7,
    cardLevelUp = Just 2,
    cardText = ["ATTACK +2"],
    cardGlossary = []
    }

heroDetails HalfOrcMarauder = initCardDetails {
    cardName = "Half-Orc Marauder",
    cardSource = Dragonspire,
    cardType = HeroHalfOrc,
    cardIcon = CardIconHero 2,
    cardCount = 4,
    cardClasses = [ClassFighter,ClassThief],
    cardGold = Just 1,
    cardStrength = Just 5,
    cardPrice = Just 10,
    cardLevelUp = Just 3,
    cardText = ["ATTACK +2",
                "DUNGEON: Destroy a Food to gain ATTACK +2, and to force "
                ++ "each other player to discard 1 card."],
    cardGlossary = []
    }

heroDetails HalfOrcDervish = initCardDetails {
    cardName = "Half-Orc Dervish",
    cardSource = Dragonspire,
    cardType = HeroHalfOrc,
    cardIcon = CardIconHero 3,
    cardCount = 2,
    cardClasses = [ClassFighter,ClassThief],
    cardGold = Just 2,
    cardStrength = Just 6,
    cardPrice = Just 13,
    cardVictoryPoints = Just 3,
    cardText = ["ATTACK +3",
                "DUNGEON: Destroy an Item to gain ATTACK +2, and to force "
                ++ "each other player to discard 1 card.",
                "DUNGEON: Discard a card for an additional ATTACK + 2."],
    cardGlossary = []
    }

heroDetails StoneguardBrute = initCardDetails {
    cardName = "Stoneguard Brute",
    cardSource = Dragonspire,
    cardType = HeroStoneguard,
    cardIcon = CardIconHero 1,
    cardCount = 6,
    cardClasses = [ClassFighter],
    cardStrength = Just 6,
    cardPrice = Just 8,
    cardLevelUp = Just 2,
    cardText = ["ATTACK +3","Additional ATTACK +2 if equipped."],
    cardGlossary = []
    }

heroDetails StoneguardBruiser = initCardDetails {
    cardName = "Stoneguard Bruiser",
    cardSource = Dragonspire,
    cardType = HeroStoneguard,
    cardIcon = CardIconHero 2,
    cardCount = 4,
    cardClasses = [ClassFighter],
    cardStrength = Just 7,
    cardPrice = Just 11,
    cardLevelUp = Just 3,
    cardText = ["ATTACK +4",
                "Additional ATTACK +3 if equipped.",
                "Additional ATTACK +1 per Mercenary you reveal."],
    cardGlossary = []
    }

heroDetails StoneguardTanker = initCardDetails {
    cardName = "Stoneguard Tanker",
    cardSource = Dragonspire,
    cardType = HeroStoneguard,
    cardIcon = CardIconHero 3,
    cardCount = 2,
    cardClasses = [ClassFighter],
    cardStrength = Just 8,
    cardPrice = Just 13,
    cardVictoryPoints = Just 2,
    cardText = ["ATTACK +6",
                "Additional ATTACK +4 if equipped.",
                "Additional ATTACK +1 per Mercenary revealed.",
                "SPOILS: Set aside any revealed Mercenaries and "
                ++ "add them to your new hand after you refill."],
    cardGlossary = []
    }

heroDetails TerakianDefender = initCardDetails {
    cardName = "Terakian Defender",
    cardSource = Dragonspire,
    cardType = HeroTerakian,
    cardIcon = CardIconHero 1,
    cardCount = 6,
    cardClasses = [ClassFighter,ClassCleric],
    cardStrength = Just 3,
    cardPrice = Just 6,
    cardLevelUp = Just 2,
    cardText = ["ATTACK +2",
                "VILLAGE/DUNGEON: Discard a Disease to draw 1 card."],
    cardGlossary = []
    }

heroDetails TerakianPeer = initCardDetails {
    cardName = "Terakian Peer",
    cardSource = Dragonspire,
    cardType = HeroTerakian,
    cardIcon = CardIconHero 2,
    cardCount = 4,
    cardClasses = [ClassFighter,ClassCleric],
    cardStrength = Just 4,
    cardPrice = Just 9,
    cardLevelUp = Just 3,
    cardText = ["ATTACK +3",
                "DUNGEON: Discard a Disease to draw 1 card and gain an "
                ++ "additional ATTACK +1."],
    cardGlossary = []
    }

heroDetails TerakianTemplar = initCardDetails {
    cardName = "Terakian Templar",
    cardSource = Dragonspire,
    cardType = HeroTerakian,
    cardIcon = CardIconHero 3,
    cardCount = 2,
    cardClasses = [ClassFighter,ClassCleric],
    cardStrength = Just 5,
    cardPrice = Just 13,
    cardLight = Just 1,
    cardVictoryPoints = Just 2,
    cardText = ["ATTACK +4",
                "DUNGEON: Discard a Disease or Villager to draw 2 cards "
                ++ "and gain an additional ATTACK +2.",
                "SPOILS: Buy a Villager."],
    cardGlossary = []
    }

heroDetails VeteranWarrior = initCardDetails {
    cardName = "Veteran Warrior",
    cardSource = Dragonspire,
    cardType = HeroVeteran,
    cardIcon = CardIconHero 1,
    cardCount = 8,
    cardClasses = [ClassFighter],
    cardStrength = Just 6,
    cardPrice = Just 8,
    cardLevelUp = Just 2,
    cardText = ["ATTACK +3"],
    cardGlossary = []
    }

heroDetails VeteranBerserker = initCardDetails {
    cardName = "Veteran Berserker",
    cardSource = Dragonspire,
    cardType = HeroVeteran,
    cardIcon = CardIconHero 2,
    cardCount = 5,
    cardClasses = [ClassFighter],
    cardStrength = Just 7,
    cardPrice = Just 11,
    cardLevelUp = Just 3,
    cardText = ["ATTACK +4",
                "DUNGEON: Destroy a Hero for an additional ATTACK +2."],
    cardGlossary = []
    }

heroDetails VeteranReaver = initCardDetails {
    cardName = "Veteran Reaver",
    cardSource = Dragonspire,
    cardType = HeroVeteran,
    cardIcon = CardIconHero 3,
    cardCount = 3,
    cardClasses = [ClassFighter],
    cardStrength = Just 8,
    cardPrice = Just 13,
    cardLevelUp = Just 5,
    cardVictoryPoints = Just 2,
    cardText = ["ATTACK +5",
                "DUNGEON: Destroy a Hero for an additional ATTACK +4.",
                "SPOILS: Gain 1 XP."],
    cardGlossary = []
    }

heroDetails VeteranWarmonger = initCardDetails {
    cardName = "Veteran Warmonger",
    cardSource = Dragonspire,
    cardType = HeroVeteran,
    cardIcon = CardIconHero 4,
    cardCount = 2,
    cardClasses = [ClassFighter],
    cardStrength = Just 10,
    cardPrice = Just 18,
    cardVictoryPoints = Just 4,
    cardText = ["ATTACK +6",
                "Additional ATTACK +10 if you reveal only Level 3 "
                ++ "or higher Heroes.",
                "SPOILS: Gain 2 XP."],
    cardGlossary = []
    }

heroDetails ClanSergeant = initCardDetails {
    cardName = "Clan Sergeant",
    cardSource = WrathOfTheElements,
    cardType = HeroClan,
    cardIcon = CardIconHero 1,
    cardCount = 6,
    cardClasses = [ClassFighter],
    cardStrength = Just 6,
    cardPrice = Just 6,
    cardLevelUp = Just 2,
    cardText = ["ATTACK +2",
                "DUNGEON: Your other equipped heroes gain +1 Attack."],
    cardGlossary = []
    }

heroDetails ClanCommander = initCardDetails {
    cardName = "Clan Commander",
    cardSource = WrathOfTheElements,
    cardType = HeroClan,
    cardIcon = CardIconHero 2,
    cardCount = 4,
    cardClasses = [ClassFighter],
    cardStrength = Just 7,
    cardPrice = Just 9,
    cardLevelUp = Just 3,
    cardText = ["ATTACK +3",
                "DUNGEON: Your other equipped heroes gain +2 Attack."],
    cardGlossary = []
    }

heroDetails ClanChampion = initCardDetails {
    cardName = "Clan Champion",
    cardSource = WrathOfTheElements,
    cardType = HeroClan,
    cardIcon = CardIconHero 3,
    cardCount = 2,
    cardClasses = [ClassFighter],
    cardStrength = Just 8,
    cardPrice = Just 12,
    cardVictoryPoints = Just 3,
    cardText = ["ATTACK +4",
                "DUNGEON: Your other equipped heroes gain +3 Attack.",
                "Spoils (Weapon)."],
    cardGlossary = []
    }

heroDetails HarruliInitiate = initCardDetails {
    cardName = "Harruli Initiate",
    cardSource = Promotional,
    cardType = HeroHarruli,
    cardIcon = CardIconHero 1,
    cardCount = 6,
    cardClasses = [ClassFighter,ClassWizard],
    cardStrength = Just 5,
    cardPrice = Just 6,
    cardLevelUp = Just 2,
    cardText = ["ATTACK +1","DUNGEON: +1 MAGIC ATTACK per revealed spell."],
    cardGlossary = []
    }

heroDetails HarruliSpellsword = initCardDetails {
    cardName = "Harruli Spellsword",
    cardSource = Promotional,
    cardType = HeroHarruli,
    cardIcon = CardIconHero 2,
    cardCount = 4,
    cardClasses = [ClassFighter,ClassWizard],
    cardStrength = Just 6,
    cardPrice = Just 9,
    cardLevelUp = Just 3,
    cardText = ["ATTACK +2","DUNGEON: +2 MAGIC ATTACK per revealed spell."],
    cardGlossary = []
    }

heroDetails HarruliAvatar = initCardDetails {
    cardName = "Harruli Avatar",
    cardSource = Promotional,
    cardType = HeroHarruli,
    cardIcon = CardIconHero 3,
    cardCount = 2,
    cardClasses = [ClassFighter,ClassWizard],
    cardStrength = Just 7,
    cardPrice = Just 12,
    cardVictoryPoints = Just 2,
    cardText = ["ATTACK +4",
                "DUNGEON: Reveal the top three cards of your deck.  "
                ++ "Draw one spell card (if any) into your hand.  "
                ++ "Discard the rest."],
    cardGlossary = []
    }




monsterDetails :: MonsterCard -> CardDetails MonsterType

monsterDetails ArchdukeOfPain = initCardDetails {
    cardName = "Archduke of Pain",
    cardSource = ThunderstoneBase,
    cardType = MonsterAbyssal,
    cardIcon = CardIconDungeon,
    cardCount = 1,
    cardClasses = [ClassAbyssal],
    cardGold = Just 3,
    cardVictoryPoints = Just 8,
    cardHealth = Just 10,
    cardXP = Just 3,
    cardText = ["Magic Attack Required",
                "BREACH: Destroy the top two cards from each Hero deck "
                ++ "in the Village.",
                "BATTLE: Destroy all Clerics and Wizards."],
    cardGlossary =
        ["Archduke of Pain: You must have a Magic Attack of at least "
         ++ "+1 in order to defeat the Archduke of Pain.  You may still "
         ++ "choose to attack the Archduke, even without Magic Attack "
         ++ "present.  If there are no Cleric and/or Wizard cards in "
         ++ "the battle, there is no effect.  When the Archduke reaches "
         ++ "Rank 1 of he Dungeon Hall, destroy the top two cards from "
         ++ "each Hero stack in the Village, including Militia."]
    }

monsterDetails Grudgebeast = initCardDetails {
    cardName = "Grudgebeast",
    cardSource = ThunderstoneBase,
    cardType = MonsterAbyssal,
    cardIcon = CardIconDungeon,
    cardCount = 2,
    cardClasses = [ClassAbyssal],
    cardGold = Just 1,
    cardVictoryPoints = Just 2,
    cardHealth = Just 4,
    cardXP = Just 1,
    cardGlossary = ["Grudgebeast: This Monster has no special Effects."]
    }

monsterDetails Succubus = initCardDetails {
    cardName = "Succubus",
    cardSource = ThunderstoneBase,
    cardType = MonsterAbyssal,
    cardIcon = CardIconDungeon,
    cardCount = 2,
    cardClasses = [ClassAbyssal],
    cardGold = Just 2,
    cardVictoryPoints = Just 4,
    cardHealth = Just 6,
    cardXP = Just 2,
    cardText = ["HALF-ATTACK without MAGIC ATTACK present"],
    cardGlossary =
        ["Succubus: If you do not have at least +1 Magic Attack, the "
         ++ "total Attack Value is halved, round down."]
    }

monsterDetails Tormentor = initCardDetails {
    cardName = "Tormentor",
    cardSource = ThunderstoneBase,
    cardType = MonsterAbyssal,
    cardIcon = CardIconDungeon,
    cardCount = 2,
    cardClasses = [ClassAbyssal],
    cardGold = Just 2,
    cardVictoryPoints = Just 6,
    cardHealth = Just 8,
    cardXP = Just 3,
    cardText = ["HALF-ATTACK without a Weapon present",
                "BATTLE: Destroy one Cleric."],
    cardGlossary =
        ["Tormentor: If you do not have at least one equipped "
         ++ "Weapon in the battle, the total Attack Value is halved, "
         ++ "round down.  If there are no Cleric cards in the battle, "
         ++ "there is no effect.  Destroyed Cleric cards remain until "
         ++ "the end of the battle."]
    }

monsterDetails TheUnchained = initCardDetails {
    cardName = "The Unchained",
    cardSource = ThunderstoneBase,
    cardType = MonsterAbyssal,
    cardIcon = CardIconDungeon,
    cardCount = 3,
    cardClasses = [ClassAbyssal],
    cardGold = Just 1,
    cardVictoryPoints = Just 3,
    cardHealth = Just 5,
    cardXP = Just 1,
    cardText = ["BATTLE: Gain one Disease.","* MAGIC ATTACK +1"],
    cardGlossary =
        ["Unchained, the: Disease cards gained in battle go directly "
         ++ "to your discard pile."]
    }

monsterDetails Darkness = initCardDetails {
    cardName = "Darkness",
    cardSource = ThunderstoneBase,
    cardType = MonsterDoomknightHumanoid,
    cardIcon = CardIconDungeon,
    cardCount = 2,
    cardClasses = [ClassDoomknight,ClassHumanoid],
    cardGold = Just 1,
    cardVictoryPoints = Just 4,
    cardHealth = Just 6,
    cardXP = Just 2,
    cardText = ["Unequipped Heroes cannot attack","Light -1"],
    cardGlossary =
        ["Darkness: Heroes without a Weapon equipped have an "
         ++ "Attack Bonus of 0, but are still affected by Battle "
         ++ "Effects.  Militia are Heroes.  Increase the Light "
         ++ "Penalty by 1.  Light Penalties are applied before "
         ++ "the battle."]
    }

monsterDetails Judgement = initCardDetails {
    cardName = "Judgement",
    cardSource = ThunderstoneBase,
    cardType = MonsterDoomknightHumanoid,
    cardIcon = CardIconDungeon,
    cardCount = 2,
    cardClasses = [ClassDoomknight,ClassHumanoid],
    cardGold = Just 1,
    cardVictoryPoints = Just 3,
    cardHealth = Just 5,
    cardXP = Just 1,
    cardText = ["BATTLE: All Heroes suffer Strength -2 and ATTACK -1."],
    cardGlossary =
        ["Judgement: You may choose to decrease Attack or Magic Attack "
         ++ "for this Battle Effect.  Affected Strength can cause "
         ++ "Weapons to become unequipped.  Militia are Heroes."]
    }

monsterDetails Knightmare = initCardDetails {
    cardName = "Knightmare",
    cardSource = ThunderstoneBase,
    cardType = MonsterDoomknightHumanoid,
    cardIcon = CardIconDungeon,
    cardCount = 2,
    cardClasses = [ClassDoomknight,ClassHumanoid],
    cardGold = Just 1,
    cardVictoryPoints = Just 6,
    cardHealth = Just 8,
    cardXP = Just 3,
    cardText = ["Light -2","BATTLE: Destroy one Fighter."],
    cardGlossary =
        ["Knightmare: Increase the Light Penalty of the Rank Knightmare "
         ++ "is in by 2.  Light Penalties are applied before the battle.  "
         ++ "If there are no Fighters in the battle, there is no effect."]
    }

monsterDetails LordMortis = initCardDetails {
    cardName = "Lord Mortis",
    cardSource = ThunderstoneBase,
    cardType = MonsterDoomknightHumanoid,
    cardIcon = CardIconDungeon,
    cardCount = 2,
    cardClasses = [ClassDoomknight,ClassHumanoid],
    cardGold = Just 1,
    cardVictoryPoints = Just 2,
    cardHealth = Just 4,
    cardXP = Just 1,
    cardText = ["Light -1"],
    cardGlossary =
        ["Lord Mortis: Increase the Light Penalty of the Rank "
         ++ "Lord Mortis is in by 2.  Light Penalties are applied "
         ++ "before the battle."]
    }

monsterDetails ThePrince = initCardDetails {
    cardName = "The Prince",
    cardSource = ThunderstoneBase,
    cardType = MonsterDoomknightHumanoid,
    cardIcon = CardIconDungeon,
    cardCount = 2,
    cardClasses = [ClassDoomknight,ClassHumanoid],
    cardGold = Just 1,
    cardVictoryPoints = Just 5,
    cardHealth = Just 7,
    cardXP = Just 2,
    cardText = ["BATTLE: All Heroes suffer Strength -2.",
                "BATTLE: Destroy one Fighter."],
    cardGlossary =
        ["Prince, the: If there are no Fighter cards in the battle, "
         ++ "there is no effect.  Reduced Strength can cause Weapons "
         ++ "to become unequipped."]
    }

monsterDetails EbonFume = initCardDetails {
    cardName = "Ebon Fume",
    cardSource = ThunderstoneBase,
    cardType = MonsterDragon,
    cardIcon = CardIconDungeon,
    cardCount = 1,
    cardClasses = [ClassDragon,ClassBlack],
    cardGold = Just 1,
    cardVictoryPoints = Just 8,
    cardHealth = Just 11,
    cardXP = Just 3,
    cardText = ["Immune to Magic Attack",
                "BATTLE: Destroy one Hero with the highest Strength.",
                "* ATTACK +3"],
    cardGlossary =
        ["Ebon Fume: Magic Attacks against Ebon Fume do not count "
         ++ "towards the total Attack Value.  If two Heroes have "
         ++ "the highest (modified) Strength, you choose which to "
         ++ "destroy.  Militia are considered Heroes."]
    }

monsterDetails Mythlurian = initCardDetails {
    cardName = "Mythlurian",
    cardSource = ThunderstoneBase,
    cardType = MonsterDragon,
    cardIcon = CardIconDungeon,
    cardCount = 2,
    cardClasses = [ClassDragon,ClassGreen],
    cardGold = Just 3,
    cardVictoryPoints = Just 4,
    cardHealth = Just 8,
    cardXP = Just 2,
    cardText = ["BATTLE: Destroy one Hero."],
    cardGlossary =
        ["Mythlurian: If there are no Hero cards in the battle, "
         ++ "there is no effect.  Destroyed Hero cards remain until "
         ++ "the end of the battle.  Militia are considered Heroes."]
    }

monsterDetails Skaladak = initCardDetails {
    cardName = "Skaladak",
    cardSource = ThunderstoneBase,
    cardType = MonsterDragon,
    cardIcon = CardIconDungeon,
    cardCount = 3,
    cardClasses = [ClassDragon,ClassWhite],
    cardGold = Just 3,
    cardVictoryPoints = Just 3,
    cardHealth = Just 7,
    cardXP = Just 2,
    cardText = ["BATTLE: Destroy one Weapon."],
    cardGlossary =
        ["Skaladak: If there are no Weapon cards in the battle, "
         ++ "there is no effect.  Destroyed Weapons remain until "
         ++ "the end of the battle."]
    }

monsterDetails TyxrTheOld = initCardDetails {
    cardName = "Tyxr the Old",
    cardSource = ThunderstoneBase,
    cardType = MonsterDragon,
    cardIcon = CardIconDungeon,
    cardCount = 2,
    cardClasses = [ClassDragon,ClassRed],
    cardGold = Just 2,
    cardVictoryPoints = Just 6,
    cardHealth = Just 10,
    cardXP = Just 3,
    cardText = ["BREACH: Each player must discard two cards.",
                "BATTLE: Destroy one Hero.",
                "* MAGIC ATTACK +2"],
    cardGlossary =
        ["Tyxr the Old: If there are no Hero cards in the battle, "
         ++ "there is no effect.  Destroyed Hero cards remain until "
         ++ "the end of the battle.  Militia are Heroes.  When Tyxr "
         ++ "reaches Rank 1 of the Dungeon Hall, all players must "
         ++ "discard two cards each, including the active player.  "
         ++ "This takes place before the active player refills his hand."]
    }

monsterDetails UyrilUnending = initCardDetails {
    cardName = "Uyril Unending",
    cardSource = ThunderstoneBase,
    cardType = MonsterDragon,
    cardIcon = CardIconDungeon,
    cardCount = 2,
    cardClasses = [ClassDragon,ClassBlue],
    cardGold = Just 2,
    cardVictoryPoints = Just 5,
    cardHealth = Just 9,
    cardXP = Just 2,
    cardText = ["HALF-ATTACK without MAGIC ATTACK present",
                "BATTLE: Destroy one Militia.",
                "* MAGIC ATTACK +1"],
    cardGlossary =
        ["Uyril Unending: If you do not have at least +1 Magic Attack, "
         ++ "the total Attack Value is halved, round down.  If there "
         ++ "are no Militia cards in the battle, there is no effect.  "
         ++ "Destroyed Militia remain until the end of the battle."]
    }

monsterDetails BlinkDog = initCardDetails {
    cardName = "Blink Dog",
    cardSource = ThunderstoneBase,
    cardType = MonsterEnchanted,
    cardIcon = CardIconDungeon,
    cardCount = 2,
    cardClasses = [ClassEnchanted],
    cardGold = Just 1,
    cardVictoryPoints = Just 2,
    cardHealth = Just 3,
    cardXP = Just 1,
    cardText = ["Light -1",
                "Cannot be attacked if a Light Penalty persists."],
    cardGlossary =
        ["Blink Dog: If you have a Light Penalty of 1 or more, you "
         ++ "cannot choose to attack the Blink Dog.  Therefore, without "
         ++ "sufficient light, you may not choose to attack it merely to "
         ++ "move it to the bottom of the Dungeon Deck.  Light Penalties "
         ++ "are applied before the battle."]
    }

monsterDetails Griffon = initCardDetails {
    cardName = "Griffon",
    cardSource = ThunderstoneBase,
    cardType = MonsterEnchanted,
    cardIcon = CardIconDungeon,
    cardCount = 2,
    cardClasses = [ClassEnchanted],
    cardGold = Just 1,
    cardVictoryPoints = Just 4,
    cardHealth = Just 7,
    cardXP = Just 2,
    cardText = ["* MAGIC ATTACK +1"],
    cardGlossary = ["Griffon: The Magic Attack bonus is a Trophy Effect."]
    }

monsterDetails Nixie = initCardDetails {
    cardName = "Nixie",
    cardSource = ThunderstoneBase,
    cardType = MonsterEnchanted,
    cardIcon = CardIconDungeon,
    cardCount = 2,
    cardClasses = [ClassEnchanted],
    cardGold = Just 4,
    cardVictoryPoints = Just 1,
    cardHealth = Just 5,
    cardXP = Just 1,
    cardGlossary = ["Nixie: This Monster has no special Effects."]
    }

monsterDetails Pegasus = initCardDetails {
    cardName = "Pegasus",
    cardSource = ThunderstoneBase,
    cardType = MonsterEnchanted,
    cardIcon = CardIconDungeon,
    cardCount = 3,
    cardClasses = [ClassEnchanted],
    cardGold = Just 2,
    cardVictoryPoints = Just 2,
    cardHealth = Just 6,
    cardXP = Just 1,
    cardText = ["* ATTACK +1"],
    cardGlossary = ["Pegasus: The Attack bonus is a Trophy Effect."]
    }

monsterDetails Sphinx = initCardDetails {
    cardName = "Sphinx",
    cardSource = ThunderstoneBase,
    cardType = MonsterEnchanted,
    cardIcon = CardIconDungeon,
    cardCount = 1,
    cardClasses = [ClassEnchanted],
    cardGold = Just 2,
    cardVictoryPoints = Just 7,
    cardHealth = Just 8,
    cardXP = Just 3,
    cardText = ["Magic Attack Only",
                "Spoils (Reveal six cards from your deck and destroy "
                ++ "any of these cards you choose.  Discard the rest.)",
                "* MAGIC ATTACK +2"],
    cardGlossary =
        ["Sphinx: Only Magic Attack bonuses count against the Sphinx.  "
         ++ "The six cards revealed from your deck do not affect or "
         ++ "replace your hand, and are drawn before any Breach or "
         ++ "Trap Effects are resolved.  This is a Spoils effect."]
    }

monsterDetails BloodskullOrc = initCardDetails {
    cardName = "Bloodskull Orc",
    cardSource = ThunderstoneBase,
    cardType = MonsterHumanoid,
    cardIcon = CardIconDungeon,
    cardCount = 2,
    cardClasses = [ClassHumanoid],
    cardGold = Just 3,
    cardHealth = Just 5,
    cardXP = Just 1,
    cardGlossary =
        ["Bloodskull Orc: This monster has no special Effects."]
    }

monsterDetails DeadboneTroll = initCardDetails {
    cardName = "Deadbone Troll",
    cardSource = ThunderstoneBase,
    cardType = MonsterHumanoid,
    cardIcon = CardIconDungeon,
    cardCount = 2,
    cardClasses = [ClassHumanoid],
    cardGold = Just 2,
    cardVictoryPoints = Just 5,
    cardHealth = Just 7,
    cardXP = Just 2,
    cardGlossary =
        ["Deadbone Troll: Disease cards gained in battle go directly "
         ++ "to your discard pile."]
    }

monsterDetails FirebrandCyclops = initCardDetails {
    cardName = "Firebrand Cyclops",
    cardSource = ThunderstoneBase,
    cardType = MonsterHumanoid,
    cardIcon = CardIconDungeon,
    cardCount = 2,
    cardClasses = [ClassHumanoid],
    cardGold = Just 1,
    cardVictoryPoints = Just 4,
    cardHealth = Just 6,
    cardXP = Just 2,
    cardGlossary =
        ["Firebrand Cyclops: This Monster has no special Effects."]
    }

monsterDetails GrayskinLizard = initCardDetails {
    cardName = "Grayskin Lizard",
    cardSource = ThunderstoneBase,
    cardType = MonsterHumanoid,
    cardIcon = CardIconDungeon,
    cardCount = 2,
    cardClasses = [ClassHumanoid],
    cardGold = Just 2,
    cardVictoryPoints = Just 3,
    cardHealth = Just 6,
    cardXP = Just 1,
    cardGlossary =
        ["Grayskin Lizard: This Battle Effect makes the vulnerable "
         ++ "to Weapons.  They lack thick skin."]
    }

monsterDetails GriknackGoblin = initCardDetails {
    cardName = "Griknack Goblin",
    cardSource = ThunderstoneBase,
    cardType = MonsterHumanoid,
    cardIcon = CardIconDungeon,
    cardCount = 2,
    cardClasses = [ClassHumanoid],
    cardGold = Just 1,
    cardVictoryPoints = Just 1,
    cardHealth = Just 4,
    cardXP = Just 1,
    cardGlossary =
        ["Griknack Goblin: This Monster has no special Effects."]
    }

monsterDetails BlackSlime = initCardDetails {
    cardName = "Black Slime",
    cardSource = ThunderstoneBase,
    cardType = MonsterOoze,
    cardIcon = CardIconDungeon,
    cardCount = 2,
    cardClasses = [ClassOoze],
    cardGold = Just 1,
    cardVictoryPoints = Just 2,
    cardHealth = Just 5,
    cardXP = Just 1,
    cardText = ["BATTLE: Destroy one Militia."],
    cardGlossary =
        ["Black Slime: If there are no Militia cards in the battle, "
         ++ "there is no effect.  Destroyed Militia remain until the "
         ++ "end of the battle."]
    }

monsterDetails GrayOoze = initCardDetails {
    cardName = "Gray Ooze",
    cardSource = ThunderstoneBase,
    cardType = MonsterOoze,
    cardIcon = CardIconDungeon,
    cardCount = 2,
    cardClasses = [ClassOoze],
    cardGold = Just 1,
    cardVictoryPoints = Just 3,
    cardHealth = Just 6,
    cardXP = Just 2,
    cardText = ["BATTLE: Destroy one Hero unless at least one Weapon "
                ++ "is attached to the Party.",
                "Spoils (Food)"],
    cardGlossary =
        ["Gray Oooze: If at least one Hero has a Weapon equipped (or "
         ++ "there are not Heroes), there is no effect.  Destroyed "
         ++ "Hero cards remain until the end of the battle.  Militia "
         ++ "are Heroes, and can be destroyed.  After a victorious "
         ++ "battle, you may purchase one Food card from the Village, "
         ++ "using the gold in your hand."]
    }

monsterDetails GreenBlob = initCardDetails {
    cardName = "Green Blob",
    cardSource = ThunderstoneBase,
    cardType = MonsterOoze,
    cardIcon = CardIconDungeon,
    cardCount = 2,
    cardClasses = [ClassOoze],
    cardGold = Just 1,
    cardVictoryPoints = Just 2,
    cardHealth = Just 5,
    cardXP = Just 1,
    cardText = ["BATTLE: Destroy one Food."],
    cardGlossary =
        ["Green Blob: If there are no Food cards in the battle, "
         ++ "there is no effect."]
    }

monsterDetails NoxiousSlag = initCardDetails {
    cardName = "Noxious Slag",
    cardSource = ThunderstoneBase,
    cardType = MonsterOoze,
    cardIcon = CardIconDungeon,
    cardCount = 2,
    cardClasses = [ClassOoze],
    cardGold = Just 1,
    cardVictoryPoints = Just 6,
    cardHealth = Just 7,
    cardXP = Just 3,
    cardText = ["HALF-ATTACK from MAGIC ATTACK","Immune to Edged Weapons"],
    cardGlossary =
        ["Noxious Slag: Edged Weapons do not add to your total "
         ++ "Attack Value against the Noxious Slag.  After "
         ++ "calculating your total Magic Attack Value, cut the "
         ++ "total in half (round down)."]
    }

monsterDetails RedJelly = initCardDetails {
    cardName = "Red Jelly",
    cardSource = ThunderstoneBase,
    cardType = MonsterOoze,
    cardIcon = CardIconDungeon,
    cardCount = 2,
    cardClasses = [ClassOoze],
    cardGold = Just 2,
    cardVictoryPoints = Just 2,
    cardHealth = Just 6,
    cardXP = Just 2,
    cardText = ["BATTLE: Destroy one Weapon."],
    cardGlossary =
        ["Red Jelly: If there are no Weapon cards in the battle, "
         ++ "there is no effect.  The Light bonus is a Trophy Effect.  "
         ++ "It only applies when the defeated Red Jelly is revealed "
         ++ "in your hand.  It is not a Battle Effect."]
    }

monsterDetails Famine = initCardDetails {
    cardName = "Famine",
    cardSource = ThunderstoneBase,
    cardType = MonsterUndeadDoom,
    cardIcon = CardIconDungeon,
    cardCount = 2,
    cardClasses = [ClassUndead,ClassDoom],
    cardGold = Just 1,
    cardVictoryPoints = Just 2,
    cardHealth = Just 4,
    cardXP = Just 1,
    cardText = ["BATTLE: Gain one Disease."],
    cardGlossary =
        ["Famine: Disease cards gained in battle go directly to "
         ++ "your discard pile."]
    }

monsterDetails Harbinger = initCardDetails {
    cardName = "Harbinger",
    cardSource = ThunderstoneBase,
    cardType = MonsterUndeadDoom,
    cardIcon = CardIconDungeon,
    cardCount = 2,
    cardClasses = [ClassUndead,ClassDoom],
    cardGold = Just 2,
    cardVictoryPoints = Just 1,
    cardHealth = Just 3,
    cardXP = Just 1,
    cardText = ["BATTLE: Destroy one Spell."],
    cardGlossary =
        ["Harbinger: If there are no Spell cards in the battle, "
         ++ "there is no effect.  Destroyed Spell cards remain "
         ++ "until the end of the battle."]
    }

monsterDetails Kingdom = initCardDetails {
    cardName = "Kingdom",
    cardSource = ThunderstoneBase,
    cardType = MonsterUndeadDoom,
    cardIcon = CardIconDungeon,
    cardCount = 3,
    cardClasses = [ClassUndead,ClassDoom],
    cardGold = Just 2,
    cardVictoryPoints = Just 3,
    cardHealth = Just 5,
    cardXP = Just 1,
    cardText = ["BATTLE: Gain one Disease."],
    cardGlossary =
        ["Kingdom: Disease cards gained in battle go directly to "
         ++ "your dicard pile."]
    }

monsterDetails LordOfDeath = initCardDetails {
    cardName = "Lord of Death",
    cardSource = ThunderstoneBase,
    cardType = MonsterUndeadDoom,
    cardIcon = CardIconDungeon,
    cardCount = 1,
    cardClasses = [ClassUndead,ClassDoom],
    cardGold = Just 2,
    cardVictoryPoints = Just 7,
    cardHealth = Just 9,
    cardXP = Just 3,
    cardText = ["BATTLE: Gain two Diseases."],
    cardGlossary =
        ["Lord of Death: Disease cards gained in battle go directly "
         ++ "to your discard pile."]
    }

monsterDetails Suffering = initCardDetails {
    cardName = "Suffering",
    cardSource = ThunderstoneBase,
    cardType = MonsterUndeadDoom,
    cardIcon = CardIconDungeon,
    cardCount = 2,
    cardClasses = [ClassUndead,ClassDoom],
    cardGold = Just 2,
    cardVictoryPoints = Just 4,
    cardHealth = Just 6,
    cardXP = Just 2,
    cardText = ["BATTLE: All Heroes suffer Strength -2.",
                "BATTLE: Gain one Disease."],
    cardGlossary =
        ["Suffering: Reduced Strength can cause Weapons to become "
         ++ "unequipped.  Disease cards gained in battle go directly "
         ++ "to your discard pile."]
    }

monsterDetails Ghost = initCardDetails {
    cardName = "Ghost",
    cardSource = ThunderstoneBase,
    cardType = MonsterUndeadSpirit,
    cardIcon = CardIconDungeon,
    cardCount = 2,
    cardClasses = [ClassUndead,ClassSpirit],
    cardGold = Just 1,
    cardVictoryPoints = Just 4,
    cardHealth = Just 6,
    cardXP = Just 1,
    cardText = ["HALF-ATTACK without MAGIC ATTACK present",
                "BATTLE: All Heroes suffer Strength -2."],
    cardGlossary =
        ["Ghost: If you do not have at least +1 Magic Attack, "
         ++ "the total Attack Value is halved, rounded down.  "
         ++ "Reduced Strength can cause Weapons to become unequipped."]
    }

monsterDetails Haunt = initCardDetails {
    cardName = "Haunt",
    cardSource = ThunderstoneBase,
    cardType = MonsterUndeadSpirit,
    cardIcon = CardIconDungeon,
    cardCount = 2,
    cardClasses = [ClassUndead,ClassSpirit],
    cardGold = Just 1,
    cardVictoryPoints = Just 2,
    cardHealth = Just 4,
    cardXP = Just 1,
    cardText = ["BATTLE: One Hero cannot attack."],
    cardGlossary =
        ["Haunt: Choose one Hero (and any equipped Weapon).  "
         ++ "All Light, Attack, and Magic Attack of the Hero "
         ++ "(and Weapon) are reduced to 0.  Militia are Heroes."]
    }

monsterDetails Revenant = initCardDetails {
    cardName = "Revenant",
    cardSource = ThunderstoneBase,
    cardType = MonsterUndeadSpirit,
    cardIcon = CardIconDungeon,
    cardCount = 2,
    cardClasses = [ClassUndead,ClassSpirit],
    cardGold = Just 2,
    cardVictoryPoints = Just 5,
    cardHealth = Just 7,
    cardXP = Just 2,
    cardText = ["Magic Attack Required",
                "BATTLE: All Heroes suffer Strength -4.  Any Heroes "
                ++ "with Strength 0 or less are Destroyed."],
    cardGlossary =
        ["Revenant: You must have at least +1 Magic Attack in order "
         ++ "to kill the Revenant.  Heroes destroyed by the Revenant "
         ++ "die at the end of the battle.  Reduce Strength can cause "
         ++ "Weapons to become unequipped."]
    }

monsterDetails Spectre = initCardDetails {
    cardName = "Spectre",
    cardSource = ThunderstoneBase,
    cardType = MonsterUndeadSpirit,
    cardIcon = CardIconDungeon,
    cardCount = 2,
    cardClasses = [ClassUndead,ClassSpirit],
    cardGold = Just 1,
    cardVictoryPoints = Just 3,
    cardHealth = Just 5,
    cardXP = Just 1,
    cardText = ["BATTLE: Destroy one Militia."],
    cardGlossary =
        ["Spectre: If there are no Militia cards in the battle, there "
         ++ "is no effect.  Destroyed Militia remain until the end of "
         ++ "the battle."]
    }

monsterDetails Wraith = initCardDetails {
    cardName = "Wraith",
    cardSource = ThunderstoneBase,
    cardType = MonsterUndeadSpirit,
    cardIcon = CardIconDungeon,
    cardCount = 2,
    cardClasses = [ClassUndead,ClassSpirit],
    cardGold = Just 2,
    cardVictoryPoints = Just 4,
    cardHealth = Just 6,
    cardXP = Just 2,
    cardText = ["BATTLE: Destroy one Militia."],
    cardGlossary =
        ["Wraith: If there are no Militia cards in the battle, "
         ++ "there is no effect.  Destroyed Militia remain until "
         ++ "the end of the battle."]
    }

monsterDetails AirWrath = initCardDetails {
    cardName = "Air Wrath",
    cardSource = WrathOfTheElements,
    cardType = MonsterElementalNature,
    cardIcon = CardIconDungeon,
    cardCount = 2,
    cardClasses = [ClassElemental,ClassNature],
    cardHealth = Just 5,
    cardGold = Just 1,
    cardXP = Just 1,
    cardVictoryPoints = Just 2,
    cardText = ["Cannot be attacked if a Spell has been used this turn."],
    cardGlossary = []
    }

monsterDetails EarthWrath = initCardDetails {
    cardName = "Earth Wrath",
    cardSource = WrathOfTheElements,
    cardType = MonsterElementalNature,
    cardIcon = CardIconDungeon,
    cardCount = 2,
    cardClasses = [ClassElemental,ClassNature],
    cardHealth = Just 7,
    cardGold = Just 1,
    cardXP = Just 2,
    cardVictoryPoints = Just 7,
    cardText = ["Magic Attack Required",
                "BATTLE: Half-Damage unless an Elemental is revealed "
                ++ "from your hand."],
    cardGlossary = []
    }

monsterDetails FireWrath = initCardDetails {
    cardName = "Fire Wrath",
    cardSource = WrathOfTheElements,
    cardType = MonsterElementalNature,
    cardIcon = CardIconDungeon,
    cardCount = 2,
    cardClasses = [ClassElemental,ClassNature],
    cardHealth = Just 6,
    cardGold = Just 1,
    cardLight = Just 1,
    cardXP = Just 2,
    cardVictoryPoints = Just 4,
    cardText = ["Immune to Spells",
                "Immune to unequipped Heroes"],
    cardGlossary = []
    }

monsterDetails ThunderWrath = initCardDetails {
    cardName = "Thunder Wrath",
    cardSource = WrathOfTheElements,
    cardType = MonsterElementalNature,
    cardIcon = CardIconDungeon,
    cardCount = 2,
    cardClasses = [ClassElemental,ClassNature],
    cardHealth = Just 9,
    cardGold = Just 1,
    cardXP = Just 3,
    cardVictoryPoints = Just 8,
    cardText = ["Cannot be attacked is a Spell is activated.",
                "Cannot be attacked if a Weapon is equipped.",
                "BATTLE: At end of battle, shuffle your hand and reveal "
                ++ "a card at random.  If the revealed card is not an "
                ++ "Elemental, destroy it."],
    cardGlossary = []
    }

monsterDetails WaterWrath = initCardDetails {
    cardName = "Water Wrath",
    cardSource = WrathOfTheElements,
    cardType = MonsterElementalNature,
    cardIcon = CardIconDungeon,
    cardCount = 2,
    cardClasses = [ClassElemental,ClassNature],
    cardHealth = Just 5,
    cardGold = Just 1,
    cardXP = Just 1,
    cardVictoryPoints = Just 5,
    cardText = ["Light Penalties Cannot be Reduced",
                "BATTLE: All Heroes without equipped Weapons or "
                ++ "Magic Attack are destroyed."],
    cardGlossary = []
    }

monsterDetails BloodTorment = initCardDetails {
    cardName = "Blood Torment",
    cardSource = WrathOfTheElements,
    cardType = MonsterElementalPain,
    cardIcon = CardIconDungeon,
    cardCount = 1,
    cardClasses = [ClassElemental,ClassPain],
    cardHealth = Just 9,
    cardGold = Just 3,
    cardXP = Just 3,
    cardVictoryPoints = Just 8,
    cardText = ["BREACH: Each player reveals and discards the top "
                ++ "five cards from their decks.  Each player destroys "
                ++ "one revealed card with a Victory Point value (if any).",
                "BATTLE: Each player reveals their hands and destroys "
                ++ "one revealed monster card."],
    cardGlossary = []
    }

monsterDetails LavaTorment = initCardDetails {
    cardName = "Lave Torment",
    cardSource = WrathOfTheElements,
    cardType = MonsterElementalPain,
    cardIcon = CardIconDungeon,
    cardCount = 2,
    cardClasses = [ClassElemental,ClassPain],
    cardHealth = Just 7,
    cardGold = Just 2,
    cardXP = Just 2,
    cardVictoryPoints = Just 5,
    cardText = ["BREACH: Each player reveals and discards the top "
                ++ "five cards from their decks.  Each player destroys "
                ++ "one revealed Level 1 Hero (if any)."],
    cardGlossary = []
    }

monsterDetails ShadowTorment = initCardDetails {
    cardName = "Shadow Torment",
    cardSource = WrathOfTheElements,
    cardType = MonsterElementalPain,
    cardIcon = CardIconDungeon,
    cardCount = 2,
    cardClasses = [ClassElemental,ClassPain],
    cardHealth = Just 7,
    cardGold = Just 2,
    cardXP = Just 2,
    cardVictoryPoints = Just 6,
    cardText = ["Magic Attack Only",
                "BREACH: Each player reveals and discards the top "
                ++ "five cards from their decks.  Each player destroys "
                ++ "one revealed Level 2 Hero (if any)."],
    cardGlossary = []
    }

monsterDetails SmokeTorment = initCardDetails {
    cardName = "Smoke Torment",
    cardSource = WrathOfTheElements,
    cardType = MonsterElementalPain,
    cardIcon = CardIconDungeon,
    cardCount = 2,
    cardClasses = [ClassElemental,ClassPain],
    cardHealth = Just 6,
    cardGold = Just 1,
    cardXP = Just 1,
    cardVictoryPoints = Just 4,
    cardText = ["Magic Attack Only",
                "BREACH: Each player reveals and discards the top "
                ++ "five cards from their decks.  Each player destroys "
                ++ "one revealed Militia (if any)."],
    cardGlossary = []
    }

monsterDetails SteamTorment = initCardDetails {
    cardName = "Steam Torment",
    cardSource = WrathOfTheElements,
    cardType = MonsterElementalPain,
    cardIcon = CardIconDungeon,
    cardCount = 3,
    cardClasses = [ClassElemental,ClassPain],
    cardHealth = Just 6,
    cardGold = Just 1,
    cardXP = Just 1,
    cardVictoryPoints = Just 3,
    cardText = ["GLOBAL EFFECT: Monsters must be defeated to leave "
                ++ "the Dungeon Hall.",
                "* ATTACK +1"],
    cardGlossary = []
    }

monsterDetails BronzeGolem = initCardDetails {
    cardName = "Bronze Golem",
    cardSource = WrathOfTheElements,
    cardType = MonsterGolem,
    cardIcon = CardIconDungeon,
    cardCount = 3,
    cardClasses = [ClassGolem],
    cardHealth = Just 5,
    cardGold = Just 2,
    cardXP = Just 1,
    cardVictoryPoints = Just 3,
    cardText = ["Heroes with Strength less than 4 cannot attack."],
    cardGlossary = []
    }

monsterDetails ClayGolem = initCardDetails {
    cardName = "Clay Golem",
    cardSource = WrathOfTheElements,
    cardType = MonsterGolem,
    cardIcon = CardIconDungeon,
    cardCount = 3,
    cardClasses = [ClassGolem],
    cardHealth = Just 6,
    cardGold = Just 2,
    cardXP = Just 2,
    cardVictoryPoints = Just 4,
    cardText = ["Heroes with Strength less than 5 cannot attack.",
                "* DUNGEON: One Hero gains Strength +2."],
    cardGlossary = []
    }

monsterDetails Colossus = initCardDetails {
    cardName = "Colossus",
    cardSource = WrathOfTheElements,
    cardType = MonsterGolem,
    cardIcon = CardIconDungeon,
    cardCount = 1,
    cardClasses = [ClassGolem],
    cardHealth = Just 10,
    cardGold = Just 2,
    cardXP = Just 3,
    cardVictoryPoints = Just 8,
    cardText = ["Immune to Magic Attack",
                "Heroes with Strength less than 8 cannot attack."],
    cardGlossary = []
    }

monsterDetails IronGolem = initCardDetails {
    cardName = "Iron Golem",
    cardSource = WrathOfTheElements,
    cardType = MonsterGolem,
    cardIcon = CardIconDungeon,
    cardCount = 1,
    cardClasses = [ClassGolem],
    cardHealth = Just 9,
    cardGold = Just 2,
    cardXP = Just 3,
    cardVictoryPoints = Just 6,
    cardText = ["Half-Magic Attack",
                "Heroes with Strength less than 7 cannot attack."],
    cardGlossary = []
    }

monsterDetails StoneGolem = initCardDetails {
    cardName = "Stone Golem",
    cardSource = WrathOfTheElements,
    cardType = MonsterGolem,
    cardIcon = CardIconDungeon,
    cardCount = 2,
    cardClasses = [ClassGolem],
    cardHealth = Just 8,
    cardGold = Just 2,
    cardXP = Just 2,
    cardVictoryPoints = Just 5,
    cardText = ["Heroes with Strength less than 6 cannot attack."],
    cardGlossary = []
    }

monsterDetails HordePlaceholder = initCardDetails {
    cardName = "Horde",
    cardSource = WrathOfTheElements,
    cardType = MonsterHordeHumanoid,
    cardIcon = CardIconDungeon,
    cardCount = 10,
    cardClasses = [ClassHorde,ClassHumanoid],
    cardText = ["Replace with the top card from the Horde Deck."],
    cardGlossary = []
    }

monsterDetails Horde3 = initCardDetails {
    cardName = "Horde",
    cardSource = WrathOfTheElements,
    cardType = MonsterHordeHumanoid,
    cardIcon = CardIconDungeon,
    cardCount = 0,
    cardClasses = [ClassHorde,ClassHumanoid],
    cardHealth = Just 3,
    cardGold = Just 1,
    cardXP = Just 1,
    cardText = ["Each Horde is worth a number of Victory Points equal "
                ++ "to the number of Horde in your Party Deck "
                ++ "(including this one) at the end of the game."],
    cardGlossary = []
    }

monsterDetails Horde4 = initCardDetails {
    cardName = "Horde",
    cardSource = WrathOfTheElements,
    cardType = MonsterHordeHumanoid,
    cardIcon = CardIconDungeon,
    cardCount = 0,
    cardClasses = [ClassHorde,ClassHumanoid],
    cardHealth = Just 4,
    cardGold = Just 1,
    cardXP = Just 1,
    cardText = ["Each Horde is worth a number of Victory Points equal "
                ++ "to the number of Horde in your Party Deck "
                ++ "(including this one) at the end of the game."],
    cardGlossary = []
    }

monsterDetails Horde5 = initCardDetails {
    cardName = "Horde",
    cardSource = WrathOfTheElements,
    cardType = MonsterHordeHumanoid,
    cardIcon = CardIconDungeon,
    cardCount = 0,
    cardClasses = [ClassHorde,ClassHumanoid],
    cardHealth = Just 5,
    cardGold = Just 1,
    cardXP = Just 1,
    cardText = ["Each Horde is worth a number of Victory Points equal "
                ++ "to the number of Horde in your Party Deck "
                ++ "(including this one) at the end of the game."],
    cardGlossary = []
    }

monsterDetails Horde6 = initCardDetails {
    cardName = "Horde",
    cardSource = WrathOfTheElements,
    cardType = MonsterHordeHumanoid,
    cardIcon = CardIconDungeon,
    cardCount = 0,
    cardClasses = [ClassHorde,ClassHumanoid],
    cardHealth = Just 6,
    cardGold = Just 1,
    cardXP = Just 1,
    cardText = ["Each Horde is worth a number of Victory Points equal "
                ++ "to the number of Horde in your Party Deck "
                ++ "(including this one) at the end of the game."],
    cardGlossary = []
    }

monsterDetails Horde7 = initCardDetails {
    cardName = "Horde",
    cardSource = WrathOfTheElements,
    cardType = MonsterHordeHumanoid,
    cardIcon = CardIconDungeon,
    cardCount = 0,
    cardClasses = [ClassHorde,ClassHumanoid],
    cardHealth = Just 7,
    cardGold = Just 1,
    cardXP = Just 1,
    cardText = ["Each Horde is worth a number of Victory Points equal "
                ++ "to the number of Horde in your Party Deck "
                ++ "(including this one) at the end of the game."],
    cardGlossary = []
    }

monsterDetails Horde8 = initCardDetails {
    cardName = "Horde",
    cardSource = WrathOfTheElements,
    cardType = MonsterHordeHumanoid,
    cardIcon = CardIconDungeon,
    cardCount = 0,
    cardClasses = [ClassHorde,ClassHumanoid],
    cardHealth = Just 8,
    cardGold = Just 1,
    cardXP = Just 1,
    cardText = ["Each Horde is worth a number of Victory Points equal "
                ++ "to the number of Horde in your Party Deck "
                ++ "(including this one) at the end of the game."],
    cardGlossary = []
    }

monsterDetails Horde9 = initCardDetails {
    cardName = "Horde",
    cardSource = WrathOfTheElements,
    cardType = MonsterHordeHumanoid,
    cardIcon = CardIconDungeon,
    cardCount = 0,
    cardClasses = [ClassHorde,ClassHumanoid],
    cardHealth = Just 9,
    cardGold = Just 1,
    cardXP = Just 1,
    cardText = ["Each Horde is worth a number of Victory Points equal "
                ++ "to the number of Horde in your Party Deck "
                ++ "(including this one) at the end of the game."],
    cardGlossary = []
    }

monsterDetails Horde10 = initCardDetails {
    cardName = "Horde",
    cardSource = WrathOfTheElements,
    cardType = MonsterHordeHumanoid,
    cardIcon = CardIconDungeon,
    cardCount = 0,
    cardClasses = [ClassHorde,ClassHumanoid],
    cardHealth = Just 10,
    cardGold = Just 1,
    cardXP = Just 1,
    cardText = ["Each Horde is worth a number of Victory Points equal "
                ++ "to the number of Horde in your Party Deck "
                ++ "(including this one) at the end of the game."],
    cardGlossary = []
    }

monsterDetails Horde11 = initCardDetails {
    cardName = "Horde",
    cardSource = WrathOfTheElements,
    cardType = MonsterHordeHumanoid,
    cardIcon = CardIconDungeon,
    cardCount = 0,
    cardClasses = [ClassHorde,ClassHumanoid],
    cardHealth = Just 11,
    cardGold = Just 1,
    cardXP = Just 1,
    cardText = ["Each Horde is worth a number of Victory Points equal "
                ++ "to the number of Horde in your Party Deck "
                ++ "(including this one) at the end of the game."],
    cardGlossary = []
    }

monsterDetails Horde12 = initCardDetails {
    cardName = "Horde",
    cardSource = WrathOfTheElements,
    cardType = MonsterHordeHumanoid,
    cardIcon = CardIconDungeon,
    cardCount = 0,
    cardClasses = [ClassHorde,ClassHumanoid],
    cardHealth = Just 12,
    cardGold = Just 1,
    cardXP = Just 1,
    cardText = ["Each Horde is worth a number of Victory Points equal "
                ++ "to the number of Horde in your Party Deck "
                ++ "(including this one) at the end of the game."],
    cardGlossary = []
    }

monsterDetails TheBloodless = initCardDetails {
    cardName = "The Bloodless",
    cardSource = DoomgateLegion,
    cardType = MonsterAbyssalThunderspawn,
    cardIcon = CardIconDungeon,
    cardCount = 2,
    cardClasses = [ClassAbyssal,ClassThunderspawn],
    cardHealth = Just 7,
    cardGold = Just 2,
    cardXP = Just 2,
    cardVictoryPoints = Just 4,
    cardText = ["BREACH: Each player reveals the top five cards of "
                ++ "their decks and destroys all revealed Wizards.",
                "BATTLE: Destroy the Hero with the highest Total "
                ++ "Attack Value."],
    cardGlossary = []
    }

monsterDetails Razorback = initCardDetails {
    cardName = "Rezorback",
    cardSource = DoomgateLegion,
    cardType = MonsterAbyssalThunderspawn,
    cardIcon = CardIconDungeon,
    cardCount = 2,
    cardClasses = [ClassAbyssal,ClassThunderspawn],
    cardHealth = Just 5,
    cardGold = Just 1,
    cardXP = Just 1,
    cardVictoryPoints = Just 3,
    cardText = ["BREACH: Each player reveals the top five cards of "
                ++ "their decks and destroys all revealed Archers.",
                "BATTLE: Destroy all Archers."],
    cardGlossary = []
    }

monsterDetails Regicide = initCardDetails {
    cardName = "Regicide",
    cardSource = DoomgateLegion,
    cardType = MonsterAbyssalThunderspawn,
    cardIcon = CardIconDungeon,
    cardCount = 2,
    cardClasses = [ClassAbyssal,ClassThunderspawn],
    cardHealth = Just 6,
    cardGold = Just 2,
    cardXP = Just 2,
    cardVictoryPoints = Just 3,
    cardText = ["Immune to Magic Attack",
                "BREACH: Each player reveals the top five cards of "
                ++ "their decks and destroys all revealed Clerics."],
    cardGlossary = []
    }

monsterDetails TendrilMinion = initCardDetails {
    cardName = "Tendril Minion",
    cardSource = DoomgateLegion,
    cardType = MonsterAbyssalThunderspawn,
    cardIcon = CardIconDungeon,
    cardCount = 2,
    cardClasses = [ClassAbyssal,ClassThunderspawn],
    cardHealth = Just 4,
    cardGold = Just 1,
    cardXP = Just 1,
    cardVictoryPoints = Just 2,
    cardText = ["BATTLE: Gain one Disease, plus one additional Disease "
                ++ "if you have any revealed Disease cards."],
    cardGlossary = []
    }

monsterDetails Usurper = initCardDetails {
    cardName = "Usurper",
    cardSource = DoomgateLegion,
    cardType = MonsterAbyssalThunderspawn,
    cardIcon = CardIconDungeon,
    cardCount = 2,
    cardClasses = [ClassAbyssal,ClassThunderspawn],
    cardHealth = Just 8,
    cardGold = Just 2,
    cardXP = Just 3,
    cardVictoryPoints = Just 6,
    cardText = ["Immune to Magic Attack",
                "BATTLE: Destroy all but two revealed Heroes.",
                "Spoils (Village)."],
    cardGlossary = []
    }

monsterDetails TheAuthority = initCardDetails {
    cardName = "The Authority",
    cardSource = DoomgateLegion,
    cardType = MonsterCultistHumanoid,
    cardIcon = CardIconDungeon,
    cardCount = 2,
    cardClasses = [ClassCultist,ClassHumanoid],
    cardHealth = Just 7,
    cardGold = Just 2,
    cardXP = Just 2,
    cardVictoryPoints = Just 5,
    cardText = ["BATTLE: The Authority gains +3 Health for each "
                ++ "revealed level 2 Hero.",
                "BATTLE: Destroy one Level 1 Hero."],
    cardGlossary = []
    }

monsterDetails TheCleansed = initCardDetails {
    cardName = "The Cleansed",
    cardSource = DoomgateLegion,
    cardType = MonsterCultistHumanoid,
    cardIcon = CardIconDungeon,
    cardCount = 2,
    cardClasses = [ClassCultist,ClassHumanoid],
    cardHealth = Just 6,
    cardGold = Just 2,
    cardXP = Just 2,
    cardVictoryPoints = Just 4,
    cardText = ["BATTLE: The Cleansed gains +2 Health for each "
                ++ "revealed level 1 Hero.",
                "BATTLE: Destroy all Militia."],
    cardGlossary = []
    }

monsterDetails TheDevout = initCardDetails {
    cardName = "The Devout",
    cardSource = DoomgateLegion,
    cardType = MonsterCultistHumanoid,
    cardIcon = CardIconDungeon,
    cardCount = 3,
    cardClasses = [ClassCultist,ClassHumanoid],
    cardHealth = Just 4,
    cardGold = Just 1,
    cardXP = Just 1,
    cardVictoryPoints = Just 2,
    cardText = ["BATTLE: Destroy one Item."],
    cardGlossary = []
    }

monsterDetails TheFaithful = initCardDetails {
    cardName = "The Faithful",
    cardSource = DoomgateLegion,
    cardType = MonsterCultistHumanoid,
    cardIcon = CardIconDungeon,
    cardCount = 2,
    cardClasses = [ClassCultist,ClassHumanoid],
    cardHealth = Just 5,
    cardGold = Just 1,
    cardXP = Just 1,
    cardVictoryPoints = Just 3,
    cardText = ["BATTLE: The Faithful gains +1 Health for each "
                ++ "revealed Militia.",
                "BATTLE: Destroy one weapon or one spell."],
    cardGlossary = []
    }

monsterDetails TheVoice = initCardDetails {
    cardName = "The Voice",
    cardSource = DoomgateLegion,
    cardType = MonsterCultistHumanoid,
    cardIcon = CardIconDungeon,
    cardCount = 1,
    cardClasses = [ClassCultist,ClassHumanoid],
    cardHealth = Just 8,
    cardGold = Just 2,
    cardXP = Just 3,
    cardVictoryPoints = Just 7,
    cardText = ["BATTLE: The Voice gains +4 Health if a level 3 Hero "
                ++ "is revealed.",
                "BATTLE: Destroy one Level 2 Hero."],
    cardGlossary = []
    }

monsterDetails Arachnea = initCardDetails {
    cardName = "Arachnea",
    cardSource = DoomgateLegion,
    cardType = MonsterEvilDruid,
    cardIcon = CardIconDungeon,
    cardCount = 2,
    cardClasses = [ClassEvilDruid,ClassHumanoid],
    cardHealth = Just 8,
    cardGold = Just 2,
    cardXP = Just 2,
    cardVictoryPoints = Just 6,
    cardText = ["BATTLE: Each player gains one Disease.",
                "BATTLE: Arachnea gains +2 Health for each "
                ++ "Disease revealed.",
                "BATTLE: Destroy one Cleric."],
    cardGlossary = ["If chosen, use the Special Disease deck."]
    }

monsterDetails CrowTalker = initCardDetails {
    cardName = "Crow Talker",
    cardSource = DoomgateLegion,
    cardType = MonsterEvilDruid,
    cardIcon = CardIconDungeon,
    cardCount = 2,
    cardClasses = [ClassEvilDruid,ClassHumanoid],
    cardHealth = Just 6,
    cardGold = Just 1,
    cardXP = Just 1,
    cardVictoryPoints = Just 2,
    cardText = ["BATTLE: Gain one Disease.",
                "* MAGIC ATTACK +1"],
    cardGlossary = ["If chosen, use the Special Disease deck."]
    }

monsterDetails GaiasCurse = initCardDetails {
    cardName = "Gaia's Curse",
    cardSource = DoomgateLegion,
    cardType = MonsterEvilDruid,
    cardIcon = CardIconDungeon,
    cardCount = 2,
    cardClasses = [ClassEvilDruid,ClassHumanoid],
    cardHealth = Just 10,
    cardGold = Just 3,
    cardXP = Just 3,
    cardVictoryPoints = Just 8,
    cardText = ["BATTLE: Each player gains one Disease.",
                "BATTLE: Gaia's Curse gains +1 Health for each "
                ++ "XP player has.",
                "BATTLE: Destroy one Hero of Level 1 or higher."],
    cardGlossary = ["If chosen, use the Special Disease deck."]
    }

monsterDetails MonarchDruid = initCardDetails {
    cardName = "Monarch Druid",
    cardSource = DoomgateLegion,
    cardType = MonsterEvilDruid,
    cardIcon = CardIconDungeon,
    cardCount = 2,
    cardClasses = [ClassEvilDruid,ClassHumanoid],
    cardHealth = Just 6,
    cardGold = Just 1,
    cardXP = Just 1,
    cardVictoryPoints = Just 3,
    cardText = ["BATTLE: Monarch Druid gains +3 Health if an Evil Druid "
                ++ "is revealed or another Evil Druid is in the "
                ++ "Dungeon Hall.",
                "BATTLE: Gain one Disease."],
    cardGlossary = ["If chosen, use the Special Disease deck."]
    }

monsterDetails NaturesMistress = initCardDetails {
    cardName = "Nature's Mistress",
    cardSource = DoomgateLegion,
    cardType = MonsterEvilDruid,
    cardIcon = CardIconDungeon,
    cardCount = 2,
    cardClasses = [ClassEvilDruid,ClassHumanoid],
    cardHealth = Just 7,
    cardGold = Just 1,
    cardXP = Just 1,
    cardVictoryPoints = Just 4,
    cardText = ["BREACH: Each player gains one Disease.",
                "* DUNGEON: Destroy one Disease."],
    cardGlossary = ["If chosen, use the Special Disease deck."]
    }

monsterDetails SwarmPlaceholder = initCardDetails {
    cardName = "The Swarm",
    cardSource = DoomgateLegion,
    cardType = MonsterTheSwarmAnimal,
    cardIcon = CardIconDungeon,
    cardCount = 10,
    cardClasses = [ClassTheSwarm,ClassAnimal],
    cardText = ["Replace with the top card of the Swarm stack."],
    cardGlossary = ["If chosen, use the Special Disease deck."]
    }

monsterDetails Swarm4 = initCardDetails {
    cardName = "The Swarm",
    cardSource = DoomgateLegion,
    cardType = MonsterTheSwarmAnimal,
    cardIcon = CardIconDungeon,
    cardCount = 0,
    cardClasses = [ClassTheSwarm,ClassAnimal],
    cardHealth = Just 4,
    cardGold = Just 2,
    cardXP = Just 1,
    cardText = ["The Swarm are worth 1 VP for each copy in your deck.  "
                ++ "(Maximum 5 VP each)",
                "BATTLE: Gain one Disease.",
                "* ATTACK +1"],
    cardGlossary = ["If chosen, use the Special Disease deck."]
    }

monsterDetails Swarm5 = initCardDetails {
    cardName = "The Swarm",
    cardSource = DoomgateLegion,
    cardType = MonsterTheSwarmAnimal,
    cardIcon = CardIconDungeon,
    cardCount = 0,
    cardClasses = [ClassTheSwarm,ClassAnimal],
    cardHealth = Just 5,
    cardGold = Just 2,
    cardXP = Just 1,
    cardText = ["The Swarm are worth 1 VP for each copy in your deck.  "
                ++ "(Maximum 5 VP each)",
                "BATTLE: Gain one Disease.",
                "* ATTACK +1"],
    cardGlossary = ["If chosen, use the Special Disease deck."]
    }

monsterDetails Swarm6 = initCardDetails {
    cardName = "The Swarm",
    cardSource = DoomgateLegion,
    cardType = MonsterTheSwarmAnimal,
    cardIcon = CardIconDungeon,
    cardCount = 0,
    cardClasses = [ClassTheSwarm,ClassAnimal],
    cardHealth = Just 6,
    cardGold = Just 2,
    cardXP = Just 1,
    cardText = ["The Swarm are worth 1 VP for each copy in your deck.  "
                ++ "(Maximum 5 VP each)",
                "BATTLE: Gain one Disease.",
                "* ATTACK +1"],
    cardGlossary = ["If chosen, use the Special Disease deck."]
    }

monsterDetails Swarm7 = initCardDetails {
    cardName = "The Swarm",
    cardSource = DoomgateLegion,
    cardType = MonsterTheSwarmAnimal,
    cardIcon = CardIconDungeon,
    cardCount = 0,
    cardClasses = [ClassTheSwarm,ClassAnimal],
    cardHealth = Just 7,
    cardGold = Just 2,
    cardXP = Just 1,
    cardText = ["The Swarm are worth 1 VP for each copy in your deck.  "
                ++ "(Maximum 5 VP each)",
                "BATTLE: Gain one Disease.",
                "* ATTACK +1"],
    cardGlossary = ["If chosen, use the Special Disease deck."]
    }

monsterDetails Swarm8 = initCardDetails {
    cardName = "The Swarm",
    cardSource = DoomgateLegion,
    cardType = MonsterTheSwarmAnimal,
    cardIcon = CardIconDungeon,
    cardCount = 0,
    cardClasses = [ClassTheSwarm,ClassAnimal],
    cardHealth = Just 8,
    cardGold = Just 2,
    cardXP = Just 1,
    cardText = ["The Swarm are worth 1 VP for each copy in your deck.  "
                ++ "(Maximum 5 VP each)",
                "BATTLE: Gain one Disease.",
                "* ATTACK +1"],
    cardGlossary = ["If chosen, use the Special Disease deck."]
    }

monsterDetails Swarm9 = initCardDetails {
    cardName = "The Swarm",
    cardSource = DoomgateLegion,
    cardType = MonsterTheSwarmAnimal,
    cardIcon = CardIconDungeon,
    cardCount = 0,
    cardClasses = [ClassTheSwarm,ClassAnimal],
    cardHealth = Just 9,
    cardGold = Just 2,
    cardXP = Just 1,
    cardText = ["The Swarm are worth 1 VP for each copy in your deck.  "
                ++ "(Maximum 5 VP each)",
                "BATTLE: Gain one Disease.",
                "* ATTACK +1"],
    cardGlossary = ["If chosen, use the Special Disease deck."]
    }

monsterDetails Swarm10 = initCardDetails {
    cardName = "The Swarm",
    cardSource = DoomgateLegion,
    cardType = MonsterTheSwarmAnimal,
    cardIcon = CardIconDungeon,
    cardCount = 0,
    cardClasses = [ClassTheSwarm,ClassAnimal],
    cardHealth = Just 10,
    cardGold = Just 2,
    cardXP = Just 1,
    cardText = ["The Swarm are worth 1 VP for each copy in your deck.  "
                ++ "(Maximum 5 VP each)",
                "BATTLE: Gain one Disease.",
                "* ATTACK +1"],
    cardGlossary = ["If chosen, use the Special Disease deck."]
    }

monsterDetails Swarm11 = initCardDetails {
    cardName = "The Swarm",
    cardSource = DoomgateLegion,
    cardType = MonsterTheSwarmAnimal,
    cardIcon = CardIconDungeon,
    cardCount = 0,
    cardClasses = [ClassTheSwarm,ClassAnimal],
    cardHealth = Just 11,
    cardGold = Just 2,
    cardXP = Just 1,
    cardText = ["The Swarm are worth 1 VP for each copy in your deck.  "
                ++ "(Maximum 5 VP each)",
                "BATTLE: Gain one Disease.",
                "* ATTACK +1"],
    cardGlossary = ["If chosen, use the Special Disease deck."]
    }

monsterDetails Swarm12 = initCardDetails {
    cardName = "The Swarm",
    cardSource = DoomgateLegion,
    cardType = MonsterTheSwarmAnimal,
    cardIcon = CardIconDungeon,
    cardCount = 0,
    cardClasses = [ClassTheSwarm,ClassAnimal],
    cardHealth = Just 12,
    cardGold = Just 2,
    cardXP = Just 1,
    cardText = ["The Swarm are worth 1 VP for each copy in your deck.  "
                ++ "(Maximum 5 VP each)",
                "BATTLE: Gain one Disease.",
                "* ATTACK +1"],
    cardGlossary = ["If chosen, use the Special Disease deck."]
    }

monsterDetails Swarm13 = initCardDetails {
    cardName = "The Swarm",
    cardSource = DoomgateLegion,
    cardType = MonsterTheSwarmAnimal,
    cardIcon = CardIconDungeon,
    cardCount = 0,
    cardClasses = [ClassTheSwarm,ClassAnimal],
    cardHealth = Just 13,
    cardGold = Just 2,
    cardXP = Just 1,
    cardText = ["The Swarm are worth 1 VP for each copy in your deck.  "
                ++ "(Maximum 5 VP each)",
                "BATTLE: Gain one Disease.",
                "* ATTACK +1"],
    cardGlossary = ["If chosen, use the Special Disease deck."]
    }

monsterDetails Deathchill = initCardDetails {
    cardName = "Deathchill",
    cardSource = DoomgateLegion,
    cardType = MonsterUndeadStormwraith,
    cardIcon = CardIconDungeon,
    cardCount = 2,
    cardClasses = [ClassUndead,ClassStormwraith],
    cardHealth = Just 7,
    cardGold = Just 2,
    cardXP = Just 2,
    cardVictoryPoints = Just 6,
    cardText = ["Immune to Magic Attack",
                "BATTLE: All Heroes suffer Strength -3.  Any Heroes with "
                ++ "Strength 0 or less at any point are Destroyed."],
    cardGlossary = []
    }

monsterDetails Hellstorm = initCardDetails {
    cardName = "Hellstorm",
    cardSource = DoomgateLegion,
    cardType = MonsterUndeadStormwraith,
    cardIcon = CardIconDungeon,
    cardCount = 2,
    cardClasses = [ClassUndead,ClassStormwraith],
    cardHealth = Just 8,
    cardGold = Just 2,
    cardXP = Just 3,
    cardVictoryPoints = Just 7,
    cardText = ["Immune to Magic Attack",
                "BREACH: Each player reveals five cards from his deck.  "
                ++ "Destroy all revealed Heroes with MAGIC ATTACK.",
                "* DUNGEON: Draw one card."],
    cardGlossary = []
    }

monsterDetails LightningsGaze = initCardDetails {
    cardName = "Lightning's Gaze",
    cardSource = DoomgateLegion,
    cardType = MonsterUndeadStormwraith,
    cardIcon = CardIconDungeon,
    cardCount = 2,
    cardClasses = [ClassUndead,ClassStormwraith],
    cardHealth = Just 6,
    cardGold = Just 2,
    cardXP = Just 2,
    cardVictoryPoints = Just 5,
    cardText = ["Immune to Magic Attack",
                "BATTLE: Destroy one Magic item or Magic Weapon.",
                "* DUNGEON: Draw one card."],
    cardGlossary = []
    }

monsterDetails MurderWind = initCardDetails {
    cardName = "Murder Wind",
    cardSource = DoomgateLegion,
    cardType = MonsterUndeadStormwraith,
    cardIcon = CardIconDungeon,
    cardCount = 2,
    cardClasses = [ClassUndead,ClassStormwraith],
    cardHealth = Just 4,
    cardGold = Just 2,
    cardXP = Just 1,
    cardVictoryPoints = Just 2,
    cardText = ["Immune to Magic Attack"],
    cardGlossary = []
    }

monsterDetails Rage = initCardDetails {
    cardName = "Rage",
    cardSource = DoomgateLegion,
    cardType = MonsterUndeadStormwraith,
    cardIcon = CardIconDungeon,
    cardCount = 2,
    cardClasses = [ClassUndead,ClassStormwraith],
    cardHealth = Just 5,
    cardGold = Just 2,
    cardXP = Just 1,
    cardVictoryPoints = Just 3,
    cardText = ["Immune to Magic Attack","BATTLE: Destroy one spell."],
    cardGlossary = []
    }

monsterDetails Assassin = initCardDetails {
    cardName = "Assassin",
    cardSource = Dragonspire,
    cardType = MonsterBanditHumanoid,
    cardIcon = CardIconDungeon,
    cardCount = 1,
    cardClasses = [ClassBandit,ClassHumanoid],
    cardHealth = Just 8,
    cardGold = Just 3,
    cardXP = Just 3,
    cardVictoryPoints = Just 8,
    cardText = ["Cannot be defeated unless 5 or more Heroes are present.",
                "* ATTACK +2"],
    cardGlossary = []
    }

monsterDetails Cutthroat = initCardDetails {
    cardName = "Cutthroat",
    cardSource = Dragonspire,
    cardType = MonsterBanditHumanoid,
    cardIcon = CardIconDungeon,
    cardCount = 2,
    cardClasses = [ClassBandit,ClassHumanoid],
    cardHealth = Just 6,
    cardGold = Just 2,
    cardXP = Just 2,
    cardVictoryPoints = Just 5,
    cardText = ["Cannot be defeated unless 4 or more Heroes are present.",
                "* DUNGEON: Draw a card."],
    cardGlossary = []
    }

monsterDetails Highwayman = initCardDetails {
    cardName = "Highwayman",
    cardSource = Dragonspire,
    cardType = MonsterBanditHumanoid,
    cardIcon = CardIconDungeon,
    cardCount = 2,
    cardClasses = [ClassBandit,ClassHumanoid],
    cardHealth = Just 5,
    cardGold = Just 2,
    cardXP = Just 1,
    cardVictoryPoints = Just 3,
    cardText = ["Cannot be defeated unless 3 or more Heroes are present.",
                "* Reveal to avoid a Trap."],
    cardGlossary = []
    }

monsterDetails Stalker = initCardDetails {
    cardName = "Stalker",
    cardSource = Dragonspire,
    cardType = MonsterBanditHumanoid,
    cardIcon = CardIconDungeon,
    cardCount = 3,
    cardClasses = [ClassBandit,ClassHumanoid],
    cardHealth = Just 4,
    cardGold = Just 2,
    cardXP = Just 1,
    cardVictoryPoints = Just 2,
    cardText = ["Cannot be defeated unless 3 or more Heroes are present."],
    cardGlossary = []
    }

monsterDetails Thug = initCardDetails {
    cardName = "Thug",
    cardSource = Dragonspire,
    cardType = MonsterBanditHumanoid,
    cardIcon = CardIconDungeon,
    cardCount = 2,
    cardClasses = [ClassBandit,ClassHumanoid],
    cardHealth = Just 7,
    cardGold = Just 3,
    cardXP = Just 2,
    cardVictoryPoints = Just 6,
    cardText = ["Cannot be defeated unless 4 or more Heroes are present."],
    cardGlossary = []
    }

monsterDetails Basilisk = initCardDetails {
    cardName = "Basilisk",
    cardSource = Dragonspire,
    cardType = MonsterDarkEnchanted,
    cardIcon = CardIconDungeon,
    cardCount = 2,
    cardClasses = [ClassDarkEnchanted],
    cardHealth = Just 5,
    cardGold = Just 2,
    cardXP = Just 1,
    cardVictoryPoints = Just 3,
    cardText = ["BATTLE: Destroy 1 Militia."],
    cardGlossary = []
    }

monsterDetails Harpy = initCardDetails {
    cardName = "Harpy",
    cardSource = Dragonspire,
    cardType = MonsterDarkEnchanted,
    cardIcon = CardIconDungeon,
    cardCount = 2,
    cardClasses = [ClassDarkEnchanted],
    cardHealth = Just 4,
    cardGold = Just 2,
    cardXP = Just 1,
    cardVictoryPoints = Just 1,
    cardText = [],
    cardGlossary = []
    }

monsterDetails Manticore = initCardDetails {
    cardName = "Manticore",
    cardSource = Dragonspire,
    cardType = MonsterDarkEnchanted,
    cardIcon = CardIconDungeon,
    cardCount = 2,
    cardClasses = [ClassDarkEnchanted],
    cardHealth = Just 6,
    cardGold = Just 2,
    cardXP = Just 1,
    cardVictoryPoints = Just 3,
    cardText = ["BATTLE: Destroy all Archers."],
    cardGlossary = []
    }

monsterDetails Medusa = initCardDetails {
    cardName = "Medusa",
    cardSource = Dragonspire,
    cardType = MonsterDarkEnchanted,
    cardIcon = CardIconDungeon,
    cardCount = 2,
    cardClasses = [ClassDarkEnchanted],
    cardHealth = Just 7,
    cardGold = Just 3,
    cardXP = Just 2,
    cardVictoryPoints = Just 5,
    cardText = ["BATTLE: Destroy 1 Hero without an Edged Weapon before "
                ++ "determining your total Attack Value.  At the end of "
                ++ "battle, destroy all remaining Heroes without "
                ++ "Edged Weapons equipped."],
    cardGlossary = []
    }

monsterDetails Minotaur = initCardDetails {
    cardName = "Minotaur",
    cardSource = Dragonspire,
    cardType = MonsterDarkEnchanted,
    cardIcon = CardIconDungeon,
    cardCount = 2,
    cardClasses = [ClassDarkEnchanted],
    cardHealth = Just 8,
    cardGold = Just 3,
    cardXP = Just 3,
    cardVictoryPoints = Just 6,
    cardText = ["Magic Attack only."],
    cardGlossary = []
    }

monsterDetails Blaze = initCardDetails {
    cardName = "Blaze",
    cardSource = Dragonspire,
    cardType = MonsterElementalFire,
    cardIcon = CardIconDungeon,
    cardCount = 2,
    cardClasses = [ClassElemental,ClassFire],
    cardHealth = Just 7,
    cardGold = Just 2,
    cardLight = Just 2,
    cardXP = Just 2,
    cardVictoryPoints = Just 5,
    cardText = ["GLOBAL: LIGHT +2",
                "Cannot be defeated unless a Spell or Item is used.",
                "* DUNGEON: Destroy 1 Disease from your hand."],
    cardGlossary = []
    }

monsterDetails ChokingSmoke = initCardDetails {
    cardName = "Choking Smoke",
    cardSource = Dragonspire,
    cardType = MonsterElementalFire,
    cardIcon = CardIconDungeon,
    cardCount = 2,
    cardClasses = [ClassElemental,ClassFire],
    cardHealth = Just 8,
    cardGold = Just 2,
    cardLight = Just (-2),
    cardXP = Just 2,
    cardVictoryPoints = Just 6,
    cardText = ["GLOBAL: LIGHT -2",
                "BATTLE: All players gain 1 Disease."],
    cardGlossary = []
    }

monsterDetails Ember = initCardDetails {
    cardName = "Ember",
    cardSource = Dragonspire,
    cardType = MonsterElementalFire,
    cardIcon = CardIconDungeon,
    cardCount = 2,
    cardClasses = [ClassElemental,ClassFire],
    cardHealth = Just 5,
    cardGold = Just 2,
    cardLight = Just 1,
    cardXP = Just 1,
    cardVictoryPoints = Just 2,
    cardText = ["LIGHT +1"],
    cardGlossary = []
    }

monsterDetails Flare = initCardDetails {
    cardName = "Flare",
    cardSource = Dragonspire,
    cardType = MonsterElementalFire,
    cardIcon = CardIconDungeon,
    cardCount = 2,
    cardClasses = [ClassElemental,ClassFire],
    cardHealth = Just 6,
    cardGold = Just 2,
    cardLight = Just 1,
    cardXP = Just 1,
    cardVictoryPoints = Just 3,
    cardText = ["GLOBAL: LIGHT +1","BATTLE: Destroy one card."],
    cardGlossary = []
    }

monsterDetails Inferno = initCardDetails {
    cardName = "Inferno",
    cardSource = Dragonspire,
    cardType = MonsterElementalFire,
    cardIcon = CardIconDungeon,
    cardCount = 2,
    cardClasses = [ClassElemental,ClassFire],
    cardHealth = Just 10,
    cardGold = Just 3,
    cardLight = Just 3,
    cardXP = Just 3,
    cardVictoryPoints = Just 7,
    cardText = ["GLOBAL: LIGHT +3",
                "Immune to Magic Attack",
                "SPOILS: Reveal the top 3 cards of each opponent's deck.  "
                ++ "Put all Monsters revealed this way work 3 or fewer "
                ++ "VP into the active player's discard pile."],
    cardGlossary = []
    }

monsterDetails FireGiant = initCardDetails {
    cardName = "Fire Giant",
    cardSource = Dragonspire,
    cardType = MonsterGiant,
    cardIcon = CardIconDungeon,
    cardCount = 2,
    cardClasses = [ClassGiant],
    cardHealth = Just 5,
    cardGold = Just 2,
    cardXP = Just 1,
    cardVictoryPoints = Just 2,
    cardText = ["BATTLE: Destroy 1 Hero with the lowest Strength.",
                "* DUNGEON: One Hero gains STRENGTH +3."],
    cardGlossary = []
    }

monsterDetails FrostGiant = initCardDetails {
    cardName = "Frost Giant",
    cardSource = Dragonspire,
    cardType = MonsterGiant,
    cardIcon = CardIconDungeon,
    cardCount = 2,
    cardClasses = [ClassGiant],
    cardHealth = Just 7,
    cardGold = Just 2,
    cardXP = Just 2,
    cardVictoryPoints = Just 5,
    cardText = ["BATTLE: Destroy 2 Heroes with Strength 5 or less.  "
                ++ "Destroy 1 Weapon with the lowest Weight."],
    cardGlossary = []
    }

monsterDetails MountainGiant = initCardDetails {
    cardName = "Mountain Giant",
    cardSource = Dragonspire,
    cardType = MonsterGiant,
    cardIcon = CardIconDungeon,
    cardCount = 2,
    cardClasses = [ClassGiant],
    cardHealth = Just 6,
    cardGold = Just 2,
    cardXP = Just 1,
    cardVictoryPoints = Just 3,
    cardText = ["BATTLE: Destroy 1 Hero with the lowest Strength."],
    cardGlossary = []
    }

monsterDetails StoneGiant = initCardDetails {
    cardName = "Stone Giant",
    cardSource = Dragonspire,
    cardType = MonsterGiant,
    cardIcon = CardIconDungeon,
    cardCount = 2,
    cardClasses = [ClassGiant],
    cardHealth = Just 6,
    cardGold = Just 2,
    cardXP = Just 2,
    cardVictoryPoints = Just 3,
    cardText = ["BATTLE: Destroy 2 Heroes with Strength 4 or less."],
    cardGlossary = []
    }

monsterDetails Titan = initCardDetails {
    cardName = "Titan",
    cardSource = Dragonspire,
    cardType = MonsterGiant,
    cardIcon = CardIconDungeon,
    cardCount = 2,
    cardClasses = [ClassGiant],
    cardHealth = Just 9,
    cardGold = Just 2,
    cardXP = Just 3,
    cardVictoryPoints = Just 6,
    cardText = ["BATTLE: Destroy all but the 2 strongest Heroes.  "
                ++ "Destroy all but the heaviest Weapon."],
    cardGlossary = []
    }

monsterDetails EarthTempest = initCardDetails {
    cardName = "Earth Tempest",
    cardSource = Dragonspire,
    cardType = MonsterHydraDragon,
    cardIcon = CardIconDungeon,
    cardCount = 2,
    cardClasses = [ClassHydra,ClassDragon],
    cardHealth = Just 8,
    cardGold = Just 3,
    cardXP = Just 3,
    cardVictoryPoints = Just 6,
    cardText = ["Double Health unless the active player reveals "
                ++ "at least 7 gold.",
                "BATTLE: Gain 1 Disease."],
    cardGlossary = []
    }

monsterDetails FlameRage = initCardDetails {
    cardName = "Flame Rage",
    cardSource = Dragonspire,
    cardType = MonsterHydraDragon,
    cardIcon = CardIconDungeon,
    cardCount = 2,
    cardClasses = [ClassHydra,ClassDragon],
    cardHealth = Just 7,
    cardGold = Just 2,
    cardXP = Just 2,
    cardVictoryPoints = Just 4,
    cardText = ["BATTLE: Destroy the Village card with the highest "
                ++ "Gold Value."],
    cardGlossary = []
    }

monsterDetails Hydra = initCardDetails {
    cardName = "Hydra",
    cardSource = Dragonspire,
    cardType = MonsterHydraDragon,
    cardIcon = CardIconDungeon,
    cardCount = 2,
    cardClasses = [ClassHydra,ClassDragon],
    cardHealth = Just 10,
    cardGold = Just 3,
    cardXP = Just 3,
    cardVictoryPoints = Just 7,
    cardText = ["BREACH: Each player discards cards from the top of "
                ++ "their deck until a Hero is revealed, then destroys "
                ++ "that Hero.",
                "BATTLE: Each player reveals 2 Heroes or a hand with "
                ++ "fewer.  Each player destroy their revealed Hero with "
                ++ "the highest Strength."],
    cardGlossary = []
    }

monsterDetails WaterWrathHydra = initCardDetails {
    cardName = "Water Wrath",
    cardSource = Dragonspire,
    cardType = MonsterHydraDragon,
    cardIcon = CardIconDungeon,
    cardCount = 2,
    cardClasses = [ClassHydra,ClassDragon],
    cardHealth = Just 6,
    cardGold = Just 2,
    cardXP = Just 1,
    cardVictoryPoints = Just 4,
    cardText = ["BATTLE: Destroy 1 Weapon."],
    cardGlossary = []
    }

monsterDetails WindFury = initCardDetails {
    cardName = "Wind Fury",
    cardSource = Dragonspire,
    cardType = MonsterHydraDragon,
    cardIcon = CardIconDungeon,
    cardCount = 2,
    cardClasses = [ClassHydra,ClassDragon],
    cardHealth = Just 7,
    cardGold = Just 3,
    cardXP = Just 2,
    cardVictoryPoints = Just 5,
    cardText = ["Double Health unless the active player reveals "
                ++ "at least 5 gold.",
                "BREACH: Remove 2 cards from the top of each village "
                ++ "stack which has a top card with a Gold Value of "
                ++ "3 or greater."],
    cardGlossary = []
    }

monsterDetails HalfOgre = initCardDetails {
    cardName = "Half-Ogre",
    cardSource = Dragonspire,
    cardType = MonsterOrcHumanoid,
    cardIcon = CardIconDungeon,
    cardCount = 2,
    cardClasses = [ClassOrc,ClassHumanoid],
    cardHealth = Just 6,
    cardGold = Just 2,
    cardXP = Just 2,
    cardVictoryPoints = Just 3,
    cardText = [],
    cardGlossary = []
    }

monsterDetails OrcBlademaster = initCardDetails {
    cardName = "Orc Blademaster",
    cardSource = Dragonspire,
    cardType = MonsterOrcHumanoid,
    cardIcon = CardIconDungeon,
    cardCount = 2,
    cardClasses = [ClassOrc,ClassHumanoid],
    cardHealth = Just 5,
    cardGold = Just 3,
    cardXP = Just 1,
    cardVictoryPoints = Just 1,
    cardText = [],
    cardGlossary = []
    }

monsterDetails OrcWarlord = initCardDetails {
    cardName = "Orc Warlord",
    cardSource = Dragonspire,
    cardType = MonsterOrcHumanoid,
    cardIcon = CardIconDungeon,
    cardCount = 2,
    cardClasses = [ClassOrc,ClassHumanoid],
    cardHealth = Just 8,
    cardGold = Just 2,
    cardXP = Just 2,
    cardVictoryPoints = Just 5,
    cardText = ["Cannot be damaged by unequipped Heroes."],
    cardGlossary = []
    }

monsterDetails ShadowKiller = initCardDetails {
    cardName = "Shadow Killer",
    cardSource = Dragonspire,
    cardType = MonsterOrcHumanoid,
    cardIcon = CardIconDungeon,
    cardCount = 2,
    cardClasses = [ClassOrc,ClassHumanoid],
    cardHealth = Just 5,
    cardGold = Just 2,
    cardXP = Just 1,
    cardVictoryPoints = Just 2,
    cardText = ["BATTLE: Destroy 1 Mercenary."],
    cardGlossary = []
    }

monsterDetails StandardBearer = initCardDetails {
    cardName = "Standard Bearer",
    cardSource = Dragonspire,
    cardType = MonsterOrcHumanoid,
    cardIcon = CardIconDungeon,
    cardCount = 2,
    cardClasses = [ClassOrc,ClassHumanoid],
    cardHealth = Just 4,
    cardGold = Just 2,
    cardXP = Just 1,
    cardVictoryPoints = Just 1,
    cardText = ["* ATTACK +1 against Humanoids."],
    cardGlossary = []
    }

monsterDetails Deathbringer = initCardDetails {
    cardName = "Deathbringer",
    cardSource = Dragonspire,
    cardType = MonsterUndeadLich,
    cardIcon = CardIconDungeon,
    cardCount = 2,
    cardClasses = [ClassUndead,ClassLich],
    cardHealth = Just 6,
    cardGold = Just 2,
    cardXP = Just 2,
    cardVictoryPoints = Just 4,
    cardText = ["Immune to Magic Attack.",
                "BATTLE: Destroy 1 Spell."],
    cardGlossary = []
    }

monsterDetails Destiny = initCardDetails {
    cardName = "Destiny",
    cardSource = Dragonspire,
    cardType = MonsterUndeadLich,
    cardIcon = CardIconDungeon,
    cardCount = 2,
    cardClasses = [ClassUndead,ClassLich],
    cardHealth = Just 7,
    cardGold = Just 3,
    cardXP = Just 3,
    cardVictoryPoints = Just 6,
    cardText = ["Immune to Magic Attack.",
                "BATTLE: Destroy 1 Cleric or Wizard."],
    cardGlossary = []
    }

monsterDetails GraveKnight = initCardDetails {
    cardName = "Grave Knight",
    cardSource = Dragonspire,
    cardType = MonsterUndeadLich,
    cardIcon = CardIconDungeon,
    cardCount = 3,
    cardClasses = [ClassUndead,ClassLich],
    cardHealth = Just 4,
    cardGold = Just 2,
    cardXP = Just 1,
    cardVictoryPoints = Just 2,
    cardText = ["Immune to Magic Attack."],
    cardGlossary = []
    }

monsterDetails LichLord = initCardDetails {
    cardName = "Lich Lord",
    cardSource = Dragonspire,
    cardType = MonsterUndeadLich,
    cardIcon = CardIconDungeon,
    cardCount = 1,
    cardClasses = [ClassUndead,ClassLich],
    cardHealth = Just 8,
    cardGold = Just 3,
    cardXP = Just 3,
    cardVictoryPoints = Just 8,
    cardText = ["Immune to Magic Attack.",
                "BATTLE: All Heroes suffer ATTACK -1 and STRENGTH -4."],
    cardGlossary = []
    }

monsterDetails TombHaunt = initCardDetails {
    cardName = "Tomb Haunt",
    cardSource = Dragonspire,
    cardType = MonsterUndeadLich,
    cardIcon = CardIconDungeon,
    cardCount = 2,
    cardClasses = [ClassUndead,ClassLich],
    cardHealth = Just 5,
    cardGold = Just 2,
    cardXP = Just 1,
    cardVictoryPoints = Just 3,
    cardText = ["Immune to Magic Attack.",
                "BATTLE: Gain 1 Disease."],
    cardGlossary = []
    }

monsterDetails HungryDead = initCardDetails {
    cardName = "Hungry Dead",
    cardSource = Dragonspire,
    cardType = MonsterUndeadPlague,
    cardIcon = CardIconDungeon,
    cardCount = 2,
    cardClasses = [ClassUndead,ClassPlague],
    cardHealth = Just 7,
    cardGold = Just 1,
    cardXP = Just 2,
    cardVictoryPoints = Just 4,
    cardText = ["BATTLE: Destroy 1 non-Disease card for each Disease "
                ++ "you revealed.  Gain 1 Disease."],
    cardGlossary = []
    }

monsterDetails Plaguebearer = initCardDetails {
    cardName = "Plaguebearer",
    cardSource = Dragonspire,
    cardType = MonsterUndeadPlague,
    cardIcon = CardIconDungeon,
    cardCount = 3,
    cardClasses = [ClassUndead,ClassPlague],
    cardHealth = Just 5,
    cardGold = Just 1,
    cardXP = Just 2,
    cardVictoryPoints = Just 3,
    cardText = ["GLOBAL: Each player gains 1 Disease whenever "
                ++ "another Monster is defeated.",
                "BATTLE: Gain 1 Disease."],
    cardGlossary = []
    }

monsterDetails PlagueZombie = initCardDetails {
    cardName = "Plague Zombie",
    cardSource = Dragonspire,
    cardType = MonsterUndeadPlague,
    cardIcon = CardIconDungeon,
    cardCount = 2,
    cardClasses = [ClassUndead,ClassPlague],
    cardHealth = Just 3,
    cardGold = Just 1,
    cardXP = Just 1,
    cardVictoryPoints = Just 1,
    cardText = ["BATTLE: Gain 1 Disease."],
    cardGlossary = []
    }

monsterDetails RestlessCorpse = initCardDetails {
    cardName = "Restless Corpse",
    cardSource = Dragonspire,
    cardType = MonsterUndeadPlague,
    cardIcon = CardIconDungeon,
    cardCount = 2,
    cardClasses = [ClassUndead,ClassPlague],
    cardHealth = Just 4,
    cardGold = Just 1,
    cardXP = Just 1,
    cardVictoryPoints = Just 2,
    cardText = ["BATTLE: Gain 1 Disease."],
    cardGlossary = []
    }

monsterDetails WalkingScourge = initCardDetails {
    cardName = "Walking Scourge",
    cardSource = Dragonspire,
    cardType = MonsterUndeadPlague,
    cardIcon = CardIconDungeon,
    cardCount = 1,
    cardClasses = [ClassUndead,ClassPlague],
    cardHealth = Just 9,
    cardGold = Just 1,
    cardXP = Just 3,
    cardVictoryPoints = Just 6,
    cardText = ["BREACH: All players gain 1 Disease.",
                "BATTLE: Gain 2 Diseases.  Each other player gains "
                ++ "1 Disease."],
    cardGlossary = []
    }

monsterDetails TheVision = initCardDetails {
    cardName = "The Vision",
    cardSource = Promotional,
    cardType = MonsterCultistHumanoid,
    cardIcon = CardIconDungeon,
    cardCount = 1,
    cardClasses = [ClassCultist,ClassHumanoid],
    cardHealth = Just 8,
    cardGold = Just 2,
    cardXP = Just 3,
    cardVictoryPoints = Just 7,
    cardText = ["BATTLE: The Vision gains +2 Health for each revealed Hero.",
                "BATTLE: Destroy one card with a Light value."],
    cardGlossary = []
    }

monsterDetails Mammoth = initCardDetails {
    cardName = "Mammoth",
    cardSource = Promotional,
    cardType = MonsterEvilDruid,
    cardIcon = CardIconDungeon,
    cardCount = 1,
    cardClasses = [ClassEvilDruid,ClassFamiliar],
    cardHealth = Just 6,
    cardGold = Just 1,
    cardXP = Just 1,
    cardVictoryPoints = Just 3,
    cardText = ["GLOBAL EFFECT: Other Evil Druids may not be attacked "
                ++ "while any Mammoths are in the Dungeon.",
                "BATTLE: The Mammoth gains +2 Health for each other "
                ++ "Evil Druid in the Dungeon."],
    cardGlossary = []
    }






villageDetails :: VillageCard -> CardDetails VillageCard
villageDetails Dagger = initCardDetails {
    cardName = "Dagger",
    cardSource = ThunderstoneBase,
    cardType = Dagger,
    cardIcon = CardIconBasic,
    cardCount = 15,
    cardClasses = [ClassWeapon,ClassEdged],
    cardGold = Just 1,
    cardWeight = Just 2,
    cardPrice = Just 3,
    cardText = ["ATTACK +1"],
    cardGlossary =
        ["Dagger: This is an Edged Weapon.  It is a Basic card "
         ++ "included in every game."]
    }

villageDetails IronRations = initCardDetails {
    cardName = "Iron Rations",
    cardSource = ThunderstoneBase,
    cardType = IronRations,
    cardIcon = CardIconBasic,
    cardCount = 15,
    cardClasses = [ClassItem,ClassFood],
    cardGold = Just 2,
    cardPrice = Just 2,
    cardText = ["DUNGEON: One Hero gains Strength +2."],
    cardGlossary =
        ["Iron Rations: You can use multiple Iron Rations to "
         ++ "increase the same Hero's Strength.  If a Dungeon Effect "
         ++ "destroys Iron Rations, you cannot use it to gain the "
         ++ "Strength bonus.  It is a Basic card included in every game."]
    }

villageDetails Torch = initCardDetails {
    cardName = "Torch",
    cardSource = ThunderstoneBase,
    cardType = Torch,
    cardIcon = CardIconBasic,
    cardCount = 15,
    cardClasses = [ClassItem,ClassLight],
    cardGold = Just 2,
    cardLight = Just 1,
    cardPrice = Just 3,
    cardGlossary =
        ["Torch: This Item always provides Light +1, even when no "
         ++ "Hero is present.  It is a Basic card included in every game."]
    }

villageDetails ArcaneEnergies = initCardDetails {
    cardName = "Arcane Energies",
    cardSource = ThunderstoneBase,
    cardType = ArcaneEnergies,
    cardIcon = CardIconVillage,
    cardCount = 8,
    cardClasses = [ClassSpell],
    cardPrice = Just 2,
    cardText = ["DUNGEON: All ATTACKS from Heroes with Weapons "
                ++ "equipped become MAGIC ATTACKS.  Draw one card."],
    cardGlossary =
        ["Arcane Energies: You must draw a card when you use this "
         ++ "dungeon ability."]
    }

villageDetails Banish = initCardDetails {
    cardName = "Banish",
    cardSource = ThunderstoneBase,
    cardType = Banish,
    cardIcon = CardIconVillage,
    cardCount = 8,
    cardClasses = [ClassSpell],
    cardPrice = Just 4,
    cardText = ["DUNGEON: Return one Monster to the bottom of the "
                ++ "deck and refill the hall, or rearrange the hall.  "
                ++ "Destroy one card from your hand.  Draw one card."],
    cardGlossary =
        ["Banish: You must declare you are entering the Dungeon "
         ++ "to play Banish, but do not choose which Monster to "
         ++ "attack until after the Hall is refilled.  If Banish "
         ++ "results in a Breach (or Trap) Effect, resolve it "
         ++ "immediately.  You may rearrange the hall so as to "
         ++ "place the Thunderstone in Rank 1 of the Dungeon Hall, "
         ++ "ending the game immediately without collecting the "
         ++ "Thunderstone.  Multiple Banish cards can be used "
         ++ "before choosing which Monster to attack, but each must "
         ++ "be completely resolved before the next can be played.  "
         ++ "You must draw a card when using this Dungeon Ability."]
    }

villageDetails Barkeep = initCardDetails {
    cardName = "Barkeep",
    cardSource = ThunderstoneBase,
    cardType = Barkeep,
    cardIcon = CardIconVillage,
    cardCount = 8,
    cardClasses = [ClassVillager],
    cardGold = Just 1,
    cardPrice = Just 2,
    cardText = ["VILLAGE: You may purchase one additional card this turn.",
                "VILLAGE: Destroy this card to gain 2 Gold."],
    cardGlossary =
        ["Barkeep: Each additional Barkeep allows you to purchase one "
         ++ "additional card.  You do not gain the gold value of the "
         ++ "Barkeep when it is destroyed."]
    }

villageDetails BattleFury = initCardDetails {
    cardName = "BattleFury",
    cardSource = ThunderstoneBase,
    cardType = BattleFury,
    cardIcon = CardIconVillage,
    cardCount = 8,
    cardClasses = [ClassSpell],
    cardPrice = Just 3,
    cardText = ["DUNGEON: All Heroes gain ATTACK +1."],
    cardGlossary =
        ["Battle Fury: Militia are Heroes, and gain the Attack bonus "
         ++ "from this spell."]
    }

villageDetails Feast = initCardDetails {
    cardName = "Feast",
    cardSource = ThunderstoneBase,
    cardType = Feast,
    cardIcon = CardIconVillage,
    cardCount = 8,
    cardClasses = [ClassItem,ClassFood],
    cardGold = Just 3,
    cardPrice = Just 5,
    cardText = ["DUNGEON: All Heroes gain Strength +3 and ATTACK +1."],
    cardGlossary =
        ["Feast: Militia cards are Heroes, so they gain the Attack and "
         ++ "Strength bonuses from this card."]
    }

villageDetails Fireball = initCardDetails {
    cardName = "Fireball",
    cardSource = ThunderstoneBase,
    cardType = Fireball,
    cardIcon = CardIconVillage,
    cardCount = 8,
    cardClasses = [ClassSpell],
    cardLight = Just 1,
    cardPrice = Just 9,
    cardText = ["MAGIC ATTACK +3"],
    cardGlossary =
        ["Fireball: You do not need Heroes present to use this Spell."]
    }

villageDetails FlamingSword = initCardDetails {
    cardName = "Flaming Sword",
    cardSource = ThunderstoneBase,
    cardType = FlamingSword,
    cardIcon = CardIconVillage,
    cardCount = 8,
    cardClasses = [ClassWeapon,ClassEdged],
    cardGold = Just 2,
    cardLight = Just 1,
    cardWeight = Just 5,
    cardPrice = Just 5,
    cardText = ["MAGIC ATTACK +3"],
    cardGlossary =
        ["Flaming Sword: You only gain the Light bonus if the "
         ++ "Flaming Sword is equipped to a Hero."]
    }

villageDetails Goodberries = initCardDetails {
    cardName = "Goodberries",
    cardSource = ThunderstoneBase,
    cardType = Goodberries,
    cardIcon = CardIconVillage,
    cardCount = 8,
    cardClasses = [ClassItem,ClassFood,ClassMagic],
    cardGold = Just 2,
    cardVictoryPoints = Just 1,
    cardPrice = Just 4,
    cardText = ["DUNGEON: One Hero gains Strength +3 and ATTACK "
                ++ "becomes MAGIC ATTACK for that Hero."],
    cardGlossary =
        ["Goodberries: After the final Attack bonus of the Hero is "
         ++ "calculated, its entire bonus becomes Magic Attack.  "
         ++ "Militia are Heroes, and may benefit from this card."]
    }

villageDetails Hatchet = initCardDetails {
    cardName = "Hatchet",
    cardSource = ThunderstoneBase,
    cardType = Hatchet,
    cardIcon = CardIconVillage,
    cardCount = 8,
    cardClasses = [ClassWeapon,ClassEdged],
    cardGold = Just 1,
    cardWeight = Just 3,
    cardPrice = Just 4,
    cardText = ["ATTACK +3"],
    cardGlossary = ["Hatchet: This is an Edged Weapon."]
    }

villageDetails Lantern = initCardDetails {
    cardName = "Lantern",
    cardSource = ThunderstoneBase,
    cardType = Lantern,
    cardIcon = CardIconVillage,
    cardCount = 8,
    cardClasses = [ClassItem,ClassLight],
    cardGold = Just 2,
    cardLight = Just 2,
    cardPrice = Just 4,
    cardGlossary =
        ["Lantern: This Item always provides Light +2, even without "
         ++ "a Hero present."]
    }

villageDetails LightstoneGem = initCardDetails {
    cardName = "Lightstone Gem",
    cardSource = ThunderstoneBase,
    cardType = LightstoneGem,
    cardIcon = CardIconVillage,
    cardCount = 8,
    cardClasses = [ClassItem,ClassLight,ClassMagic],
    cardGold = Just 3,
    cardLight = Just 3,
    cardPrice = Just 6,
    cardGlossary =
        ["Lightstone Gem: This Item always provides Light +3, even "
         ++ "without a Hero present."]
    }

villageDetails MagicalAura = initCardDetails {
    cardName = "Magical Aura",
    cardSource = ThunderstoneBase,
    cardType = MagicalAura,
    cardIcon = CardIconVillage,
    cardCount = 8,
    cardClasses = [ClassSpell],
    cardGold = Just 2,
    cardPrice = Just 4,
    cardText = ["DUNGEON: All Weapons become Weight 0.  Draw one card."],
    cardGlossary =
        ["Magical Aura: When played with a Polearm, the Hero must still "
         ++ "have a Strength of 8 or more to gain the +6 bonus.  You "
         ++ "must draw a card when using this Dungeon Effect."]
    }

villageDetails Pawnbroker = initCardDetails {
    cardName = "Pawnbroker",
    cardSource = ThunderstoneBase,
    cardType = Pawnbroker,
    cardIcon = CardIconVillage,
    cardCount = 8,
    cardClasses = [ClassVillager],
    cardPrice = Just 3,
    cardText = ["VILLAGE: Destroy any card with a gold value to gain "
                ++ "its gold value plus 3 Gold.",
                "VILLAGE: Destroy this card to gain 2 Gold."],
    cardGlossary =
        ["Pawnbroker: You can destroy both the Pawnbroker and another "
         ++ "card to produce X+5 gold in a single turn.  When you "
         ++ "destroy a card with Pawnbroker, do not add its inherent "
         ++ "gold value to your total gold that turn."]
    }

villageDetails Polearm = initCardDetails {
    cardName = "Polearm",
    cardSource = ThunderstoneBase,
    cardType = Polearm,
    cardIcon = CardIconVillage,
    cardCount = 8,
    cardClasses = [ClassWeapon,ClassEdged],
    cardGold = Just 3,
    cardWeight = Just 2,
    cardPrice = Just 7,
    cardText = ["ATTACK +2, or ATTACK +6 when attached to a Hero "
                ++ "with 8 or more Strength."],
    cardGlossary =
        ["Polearm: A Hero with a Strength of 2 can equip the Polearm "
         ++ "for an Attack bonus of +2.  A Hero with a Strength of 8 "
         ++ "or higher gains +6 instead."]
    }

villageDetails ShortSword = initCardDetails {
    cardName = "Short Sword",
    cardSource = ThunderstoneBase,
    cardType = ShortSword,
    cardIcon = CardIconVillage,
    cardCount = 8,
    cardClasses = [ClassWeapon,ClassEdged],
    cardGold = Just 3,
    cardWeight = Just 4,
    cardPrice = Just 6,
    cardText = ["ATTACK +4"],
    cardGlossary = ["Short Sword: This is an Edged Weapon."]
    }

villageDetails Spear = initCardDetails {
    cardName = "Spear",
    cardSource = ThunderstoneBase,
    cardType = Spear,
    cardIcon = CardIconVillage,
    cardCount = 8,
    cardClasses = [ClassWeapon,ClassEdged],
    cardGold = Just 2,
    cardWeight = Just 4,
    cardPrice = Just 4,
    cardText = ["ATTACK +2",
                "DUNGEON: You may Destroy this Spear for an "
                ++ "additional ATTACK +3."],
    cardGlossary =
        ["Spear: If you destroy (throw) the Spear, the Attack bonus "
         ++ "increases by an additional +3, for a total of +5.  "
         ++ "However, the Spear is still considered equipped for "
         ++ "the entire battle, even if you use the effect."]
    }

villageDetails TownGuard = initCardDetails {
    cardName = "Town Guard",
    cardSource = ThunderstoneBase,
    cardType = TownGuard,
    cardIcon = CardIconVillage,
    cardCount = 8,
    cardClasses = [ClassVillager],
    cardPrice = Just 3,
    cardText = ["VILLAGE: Draw two cards.",
                "VILLAGE: Destroy this card to draw three additional "
                ++ "cards."],
    cardGlossary =
        ["Town Guard: Destroying this card allows you to draw a total "
         ++ "of five extra cards."]
    }

villageDetails Trainer = initCardDetails {
    cardName = "Trainer",
    cardSource = ThunderstoneBase,
    cardType = Trainer,
    cardIcon = CardIconVillage,
    cardCount = 8,
    cardClasses = [ClassVillager],
    cardPrice = Just 4,
    cardText = ["VILLAGE: Destroy one Militia to gain 2 XP.",
                "VILLAGE: Destroy this card to gain 2 Gold."],
    cardGlossary =
        ["Trainer: Each Trainer in your hand may only destroy one "
         ++ "Militia each turn."]
    }

villageDetails Warhammer = initCardDetails {
    cardName = "Warhammer",
    cardSource = ThunderstoneBase,
    cardType = Warhammer,
    cardIcon = CardIconVillage,
    cardCount = 8,
    cardClasses = [ClassWeapon,ClassBlunt],
    cardGold = Just 2,
    cardWeight = Just 5,
    cardPrice = Just 4,
    cardText = ["ATTACK +3",
                "Clerics gain an additional ATTACK +3 against "
                ++ "Doomknights and Undead."],
    cardGlossary =
        ["Warhammer: A Cleric attacking a Doomknight or Undead gains "
         ++ "a total Attack bonus of +6."]
    }

villageDetails Ambrosia = initCardDetails {
    cardName = "Ambrosia",
    cardSource = WrathOfTheElements,
    cardType = Ambrosia,
    cardIcon = CardIconVillage,
    cardCount = 8,
    cardClasses = [ClassItem,ClassFood,ClassMagic],
    cardGold = Just 2,
    cardPrice = Just 6,
    cardVictoryPoints = Just 2,
    cardText = ["DUNGEON: All Heroes gain Strength +2 and ATTACK +1.  "
                ++ "If a Hero would be Destroyed during the battle, "
                ++ "discard the Hero instead."],
    cardGlossary = []
    }

villageDetails AmuletOfPower = initCardDetails {
    cardName = "Amulet of Power",
    cardSource = WrathOfTheElements,
    cardType = AmuletOfPower,
    cardIcon = CardIconVillage,
    cardCount = 8,
    cardClasses = [ClassItem,ClassLight,ClassMagic],
    cardGold = Just 1,
    cardPrice = Just 5,
    cardLight = Just 2,
    cardText = ["DUNGEON: Each Hero gains Strength +3."],
    cardGlossary = []
    }

villageDetails Blacksmith = initCardDetails {
    cardName = "Blacksmith",
    cardSource = WrathOfTheElements,
    cardType = Blacksmith,
    cardIcon = CardIconVillage,
    cardCount = 8,
    cardClasses = [ClassVillager],
    cardPrice = Just 3,
    cardText = ["VILLAGE: Draw one card.  You may be one additional card "
                ++ "if you buy a Weapon.",
                "VILLAGE: Destroy this card to gain 2 gold.  Place one "
                ++ "Hero from your hand on top of your deck."],
    cardGlossary = []
    }

villageDetails Claymore = initCardDetails {
    cardName = "Claymore",
    cardSource = WrathOfTheElements,
    cardType = Claymore,
    cardIcon = CardIconVillage,
    cardCount = 8,
    cardClasses = [ClassWeapon,ClassEdged],
    cardGold = Just 3,
    cardWeight = Just 8,
    cardPrice = Just 7,
    cardText = ["ATTACK +5",
                "DUNGEON: Before refilling the Dungeon Hall, move "
                ++ "one Monster card worth 1 or 2 Victory Points from "
                ++ "the Hall into your discard pile.  Do not collect "
                ++ "XP from this Monster."],
    cardGlossary = []
    }

villageDetails CreepingDeath = initCardDetails {
    cardName = "Creeping Death",
    cardSource = WrathOfTheElements,
    cardType = CreepingDeath,
    cardIcon = CardIconVillage,
    cardCount = 8,
    cardClasses = [ClassSpell],
    cardPrice = Just 11,
    cardText = ["DUNGEON: All other players gain one Disease card.  "
                ++ "Reduce the Health of each Monster in the Dungeon "
                ++ "Hall by 2.  If this would reduce a Monster's Health "
                ++ "to 0, place it in your discard pile and refill "
                ++ "the Hall."],
    cardGlossary = []
    }

villageDetails CursedMace = initCardDetails {
    cardName = "Cursed Mace",
    cardSource = WrathOfTheElements,
    cardType = CursedMace,
    cardIcon = CardIconVillage,
    cardCount = 8,
    cardClasses = [ClassWeapon,ClassBlunt],
    cardGold = Just 0,
    cardWeight = Just 5,
    cardPrice = Just 5,
    cardText = ["ATTACK +6",
                "Gain one Disease when equipped."],
    cardGlossary = []
    }

villageDetails ForesightElixir = initCardDetails {
    cardName = "Foresight Elixir",
    cardSource = WrathOfTheElements,
    cardType = ForesightElixir,
    cardIcon = CardIconVillage,
    cardCount = 8,
    cardClasses = [ClassSpell],
    cardGold = Just 2,
    cardPrice = Just 6,
    cardText = ["You may reveal this card at any time to avoid the "
                ++ "effects of a Trap card.",
                "DUNGEON: Look at the top three cards of the Dungeon "
                ++ "Deck.  Return them in any order.  Draw two cards."],
    cardGlossary = []
    }

villageDetails IllusoryBlade = initCardDetails {
    cardName = "Illusory Blade",
    cardSource = WrathOfTheElements,
    cardType = IllusoryBlade,
    cardIcon = CardIconVillage,
    cardCount = 8,
    cardClasses = [ClassSpell],
    cardPrice = Just 3,
    cardText = ["DUNGEON: Draw any one Weapon card from the Village "
                ++ "and equip it to a Hero meeting the Strength "
                ++ "requirement.  Destroy the Weapon after the battle."],
    cardGlossary = []
    }

villageDetails MagiStaff = initCardDetails {
    cardName = "Magi Staff",
    cardSource = WrathOfTheElements,
    cardType = MagiStaff,
    cardIcon = CardIconVillage,
    cardCount = 8,
    cardClasses = [ClassWeapon,ClassBlunt,ClassMagic],
    cardGold = Just 2,
    cardWeight = Just 3,
    cardPrice = Just 4,
    cardText = ["MAGIC ATTACK +1",
                "DUNGEON: Destroy the Magi Staff to move one monster "
                ++ "from any rank to the bottom of the deck.  Refill "
                ++ "the Hall.  Draw one card."],
    cardGlossary = []
    }

villageDetails MagicMissile = initCardDetails {
    cardName = "Magic Missile",
    cardSource = WrathOfTheElements,
    cardType = MagicMissile,
    cardIcon = CardIconVillage,
    cardCount = 8,
    cardClasses = [ClassSpell],
    cardPrice = Just 6,
    cardText = ["MAGIC ATTACK +4","Rank 1 Only."],
    cardGlossary = []
    }

villageDetails Sage = initCardDetails {
    cardName = "Sage",
    cardSource = WrathOfTheElements,
    cardType = Sage,
    cardIcon = CardIconVillage,
    cardCount = 8,
    cardClasses = [ClassVillager],
    cardPrice = Just 4,
    cardText = ["VILLAGE: Destroy one card.  If it is not a Hero, "
                ++ "gain 1 XP.",
                "VILLAGE: Destroy this card.  Gain 1 Gold for each Hero "
                ++ "revealed from your hand."],
    cardGlossary = []
    }

villageDetails ShortBow = initCardDetails {
    cardName = "Short Bow",
    cardSource = WrathOfTheElements,
    cardType = ShortBow,
    cardIcon = CardIconVillage,
    cardCount = 8,
    cardClasses = [ClassWeapon,ClassBow],
    cardGold = Just 3,
    cardWeight = Just 4,
    cardPrice = Just 5,
    cardText = ["ATTACK +3 against ranks 2 and 3 only",
                "DUNGEON: Additional ATTACK +2 when equipped by an Archer."],
    cardGlossary = []
    }

villageDetails TavernBrawl = initCardDetails {
    cardName = "Tavern Brawl",
    cardSource = WrathOfTheElements,
    cardType = TavernBrawl,
    cardIcon = CardIconVillage,
    cardCount = 8,
    cardClasses = [ClassVillager],
    cardPrice = Just 4,
    cardText = ["VILLAGE: Discard one Hero and choose another player.  "
                ++ "Draw and discard cards from the chosen player's "
                ++ "until a Hero is revealed.  If your Hero's strength "
                ++ "is higher, destroy the opponent's Hero and gain 1 XP.",
                "VILLAGE: Destroy this card and one Hero.  All other "
                ++ "players discard two cards from their hands."],
    cardGlossary = []
    }

villageDetails TaxCollector = initCardDetails {
    cardName = "Tax Collector",
    cardSource = WrathOfTheElements,
    cardType = TaxCollector,
    cardIcon = CardIconVillage,
    cardCount = 8,
    cardClasses = [ClassVillager],
    cardPrice = Just 5,
    cardText = ["VILLAGE: Gain 1 Gold for each player.",
                "VILLAGE: Destroy this card.  All other players "
                ++ "discards one card with a gold value or reveals a hand "
                ++ "containing none.  Gain gold equal to the gold value "
                ++ "of all discarded cards."],
    cardGlossary = []
    }

villageDetails BlessedHammer = initCardDetails {
    cardName = "Blessed Hammer",
    cardSource = DoomgateLegion,
    cardType = BlessedHammer,
    cardIcon = CardIconVillage,
    cardCount = 8,
    cardClasses = [ClassWeapon,ClassBlunt],
    cardGold = Just 2,
    cardWeight = Just 2,
    cardPrice = Just 8,
    cardVictoryPoints = Just 1,
    cardText = ["MAGIC ATTACK +4",
                "DUNGEON: When Blessed Hammer is equipped, you may place "
                ++ "the monster in Rank 1 on the bottom of the deck and "
                ++ "refill the hall."],
    cardGlossary = []
    }

villageDetails BorderGuard = initCardDetails {
    cardName = "Border Guard",
    cardSource = DoomgateLegion,
    cardType = BorderGuard,
    cardIcon = CardIconVillage,
    cardCount = 8,
    cardClasses = [ClassVillager,ClassMercenary],
    cardPrice = Just 5,
    cardText = ["VILLAGE/DUNGEON: Draw one card, then discard one card.",
                "DUNGEON: ATTACK +1",
                "You may destroy this card to prevent one Hero from "
                ++ "being destroyed in a Battle."],
    cardGlossary = []
    }

villageDetails Cyclone = initCardDetails {
    cardName = "Cyclone",
    cardSource = DoomgateLegion,
    cardType = Cyclone,
    cardIcon = CardIconVillage,
    cardCount = 8,
    cardClasses = [ClassSpell],
    cardPrice = Just 5,
    cardText = ["MAGIC ATTACK +2",
                "One Wizard or Cleric of your choice gains +MAGIC ATTACK "
                ++ "equal to that Hero's level."],
    cardGlossary = []
    }

villageDetails DivineStaff = initCardDetails {
    cardName = "Divine Staff",
    cardSource = DoomgateLegion,
    cardType = DivineStaff,
    cardIcon = CardIconVillage,
    cardCount = 8,
    cardClasses = [ClassWeapon,ClassMagic],
    cardGold = Just 2,
    cardWeight = Just 2,
    cardPrice = Just 6,
    cardLight = Just 1,
    cardText = ["MAGIC ATTACK +1",
                "MAGIC ATTACK +2 if equipped to a Cleric or Wizard."],
    cardGlossary = []
    }

villageDetails DoomgateSquire = initCardDetails {
    cardName = "Doomgate Squire",
    cardSource = DoomgateLegion,
    cardType = DoomgateSquire,
    cardIcon = CardIconVillage,
    cardCount = 8,
    cardClasses = [ClassVillager,ClassMercenary],
    cardPrice = Just 5,
    cardText = ["VILLAGE/DUNGEON: Draw one card.",
                "VILLAGE: You may purchase one additional card.",
                "REPEAT DUNGEON: Discard 1 XP to give one Hero ATTACK +1."],
    cardGlossary = []
    }

villageDetails FlaskOfOil = initCardDetails {
    cardName = "Flask of Oil",
    cardSource = DoomgateLegion,
    cardType = FlaskOfOil,
    cardIcon = CardIconVillage,
    cardCount = 8,
    cardClasses = [ClassWeapon,ClassItem,ClassLight],
    cardGold = Just 1,
    cardWeight = Just 2,
    cardPrice = Just 4,
    cardLight = Just 1,
    cardText = ["ATTACK +1",
                "DUNGEON: LIGHT +2 and ATTACK +2.  Destroy this card at "
                ++ "end of battle."],
    cardGlossary = []
    }

villageDetails FortuneTeller = initCardDetails {
    cardName = "Fortune Teller",
    cardSource = DoomgateLegion,
    cardType = FortuneTeller,
    cardIcon = CardIconVillage,
    cardCount = 8,
    cardClasses = [ClassVillager,ClassMercenary],
    cardPrice = Just 4,
    cardText = ["VILLAGE/DUNGEON: Draw two cards.  Keep both if at least "
                ++ "one is a Spell.  Otherwise discard one.",
                "DUNGEON: ATTACK +1"],
    cardGlossary = []
    }

villageDetails GlowBerries = initCardDetails {
    cardName = "Glow Berries",
    cardSource = DoomgateLegion,
    cardType = GlowBerries,
    cardIcon = CardIconVillage,
    cardCount = 8,
    cardClasses = [ClassItem,ClassFood,ClassLight],
    cardGold = Just 1,
    cardPrice = Just 3,
    cardLight = Just 1,
    cardVictoryPoints = Just 1,
    cardText = ["One Hero gains Strength +3."],
    cardGlossary = []
    }

villageDetails GreedBlade = initCardDetails {
    cardName = "Greed Blade",
    cardSource = DoomgateLegion,
    cardType = GreedBlade,
    cardIcon = CardIconVillage,
    cardCount = 8,
    cardClasses = [ClassWeapon,ClassEdged],
    cardGold = Just 3,
    cardWeight = Just 6,
    cardPrice = Just 6,
    cardText = ["ATTACK +1 for each 2 Gold revealed"],
    cardGlossary = []
    }

villageDetails PiousChaplain = initCardDetails {
    cardName = "Pious Chaplain",
    cardSource = DoomgateLegion,
    cardType = PiousChaplain,
    cardIcon = CardIconVillage,
    cardCount = 8,
    cardClasses = [ClassVillager,ClassMercenary],
    cardPrice = Just 4,
    cardText = ["VILLAGE/DUNGEON: Destroy any Disease cards in you hand.  "
                ++ "Draw two cards for each Disease destroyed."],
    cardGlossary = []
    }

villageDetails SoulJar = initCardDetails {
    cardName = "Soul Jar",
    cardSource = DoomgateLegion,
    cardType = SoulJar,
    cardIcon = CardIconVillage,
    cardCount = 8,
    cardClasses = [ClassWeapon,ClassMagic],
    cardGold = Just 2,
    cardWeight = Just 3,
    cardPrice = Just 5,
    cardText = ["MAGIC ATTACK +1",
                "When Soul Jar is equipped to a Wizard or Cleric, select "
                ++ "one monster in your hand and gain MAGIC ATTACK equal "
                ++ "to 1/2 the monster's Health."],
    cardGlossary = []
    }

villageDetails SpiritBlast = initCardDetails {
    cardName = "Spirit Blast",
    cardSource = DoomgateLegion,
    cardType = SpiritBlast,
    cardIcon = CardIconVillage,
    cardCount = 8,
    cardClasses = [ClassSpell],
    cardPrice = Just 7,
    cardText = ["One Hero gains +MAGIC ATTACK equal to the number of XP "
                ++ "(Maximum 6) you currently possess."],
    cardGlossary = []
    }

villageDetails SpiritHunter = initCardDetails {
    cardName = "Spirit Hunter",
    cardSource = DoomgateLegion,
    cardType = SpiritHunter,
    cardIcon = CardIconVillage,
    cardCount = 8,
    cardClasses = [ClassVillager,ClassMercenary],
    cardPrice = Just 3,
    cardText = ["VILLAGE/DUNGEON: Draw one card.",
                "DUNGEON: MAGIC ATTACK +1"],
    cardGlossary = []
    }

villageDetails BluefireStaff = initCardDetails {
    cardName = "Bluefire Staff",
    cardSource = Dragonspire,
    cardType = BluefireStaff,
    cardIcon = CardIconVillage,
    cardCount = 8,
    cardClasses = [ClassWeapon,ClassBlunt,ClassMagic],
    cardGold = Just 2,
    cardWeight = Just 0,
    cardPrice = Just 3,
    cardText = ["MAGIC ATTACK +1",
                "DUNGEON: If Bluefire Staff is equipped to a Cleric "
                ++ "or Wizard, you may reuse a Spell card you have "
                ++ "already used this turn.  If you do, destroy that "
                ++ "Spell at the end of the turn."],
    cardGlossary = []
    }

villageDetails BurntOffering = initCardDetails {
    cardName = "Burnt Offering",
    cardSource = Dragonspire,
    cardType = BurntOffering,
    cardIcon = CardIconVillage,
    cardCount = 8,
    cardClasses = [ClassSpell],
    cardPrice = Just 7,
    cardText = ["VILLAGE/DUNGEON: Destroy 1 card.  At the end of the "
                ++ "turn, draw 1 additional card when you refill "
                ++ "your hand."],
    cardGlossary = []
    }

villageDetails ChieftainsDrum = initCardDetails {
    cardName = "Chieftain's Drum",
    cardSource = Dragonspire,
    cardType = ChieftainsDrum,
    cardIcon = CardIconVillage,
    cardCount = 8,
    cardClasses = [ClassItem,ClassMagic],
    cardGold = Just 2,
    cardPrice = Just 3,
    cardText = ["VILLAGE/DUNGEON: Name Hero, Village, or Monster and "
                ++ "discard the top card of your deck.  If you named "
                ++ "the card's type correctly, draw 2 cards."],
    cardGlossary = []
    }

villageDetails FrostBolt = initCardDetails {
    cardName = "Frost Bolt",
    cardSource = Dragonspire,
    cardType = FrostBolt,
    cardIcon = CardIconVillage,
    cardCount = 8,
    cardClasses = [ClassSpell],
    cardPrice = Just 7,
    cardText = ["MAGIC ATTACK +3"],
    cardGlossary = []
    }

villageDetails FrostGiantAxe = initCardDetails {
    cardName = "Frost Giant Axe",
    cardSource = Dragonspire,
    cardType = FrostGiantAxe,
    cardIcon = CardIconVillage,
    cardCount = 8,
    cardClasses = [ClassWeapon,ClassEdged],
    cardGold = Just 3,
    cardWeight = Just 6,
    cardPrice = Just 8,
    cardText = ["ATTACK +4",
                "DUNGEON: If you have no Heroes with Strength 6 or more, "
                ++ "draw a card.  If it is a Hero, that Hero gains "
                ++ "STRENGTH +3.  (This ability can be used while this "
                ++ "card is not equipped.)"],
    cardGlossary = []
    }

villageDetails GuardianBlade = initCardDetails {
    cardName = "Guardian Blade",
    cardSource = Dragonspire,
    cardType = GuardianBlade,
    cardIcon = CardIconVillage,
    cardCount = 8,
    cardClasses = [ClassWeapon,ClassEdged],
    cardGold = Just 2,
    cardWeight = Just 5,
    cardPrice = Just 5,
    cardText = ["ATTACK +3",
                "If the equipped Hero would be destroyed, discard the "
                ++ "Hero instead."],
    cardGlossary = []
    }

villageDetails Guide = initCardDetails {
    cardName = "Guide",
    cardSource = Dragonspire,
    cardType = Guide,
    cardIcon = CardIconVillage,
    cardCount = 8,
    cardClasses = [ClassVillager,ClassMercenary],
    cardGold = Just 2,
    cardPrice = Just 4,
    cardLight = Just 2,
    cardText = ["VILLAGE: You may buy an additional card."],
    cardGlossary = []
    }

villageDetails Polymorph = initCardDetails {
    cardName = "Polymorph",
    cardSource = Dragonspire,
    cardType = Polymorph,
    cardIcon = CardIconVillage,
    cardCount = 8,
    cardClasses = [ClassSpell],
    cardPrice = Just 3,
    cardText = ["DUNGEON: Look at the top card of the Dungeon Deck.  If "
                ++ "it is a Monster, you may replace any Monster in the "
                ++ "Hall with it.  Place the replaced Monster on top of "
                ++ "the Dungeon Deck.  Draw a card."],
    cardGlossary = []
    }

villageDetails Quartermaster = initCardDetails {
    cardName = "Quartermaster",
    cardSource = Dragonspire,
    cardType = Quartermaster,
    cardIcon = CardIconVillage,
    cardCount = 8,
    cardClasses = [ClassVillager,ClassMercenary],
    cardGold = Just 1,
    cardPrice = Just 4,
    cardText = ["DUNGEON: Draw 3 cards.  Discard any that match cards "
                ++ "already in your hand.",
                "VILLAGE: Discard any number of Heroes.  Draw 1 card "
                ++ "for each Hero discarded."],
    cardGlossary = []
    }

villageDetails RecurveBow = initCardDetails {
    cardName = "Recurve Bow",
    cardSource = Dragonspire,
    cardType = RecurveBow,
    cardIcon = CardIconVillage,
    cardCount = 8,
    cardClasses = [ClassWeapon,ClassBow],
    cardGold = Just 3,
    cardWeight = Just 5,
    cardPrice = Just 6,
    cardText = ["ATTACK +2 against Rank 2 or higher.",
                "Additional ATTACK +1 against Rank 2 or higher for each "
                ++ "Level of this Hero."],
    cardGlossary = []
    }

villageDetails Scout = initCardDetails {
    cardName = "Scout",
    cardSource = Dragonspire,
    cardType = Scout,
    cardIcon = CardIconVillage,
    cardCount = 8,
    cardClasses = [ClassVillager,ClassMercenary],
    cardPrice = Just 3,
    cardText = ["VILLAGE: Draw 3 cards.  Place 3 cards on top of your "
                ++ "deck in any order.",
                "DUNGEON: Look at the top 3 cards of the Dungeon Deck "
                ++ "and return them in any order."],
    cardGlossary = []
    }

villageDetails Silverstorm = initCardDetails {
    cardName = "Silverstorm",
    cardSource = Dragonspire,
    cardType = Silverstorm,
    cardIcon = CardIconVillage,
    cardCount = 8,
    cardClasses = [ClassSpell],
    cardPrice = Just 5,
    cardText = ["MAGIC ATTACK +1 for each 2 gold you reveal (round down)."],
    cardGlossary = []
    }

villageDetails Skullbreaker = initCardDetails {
    cardName = "Skullbreaker",
    cardSource = Dragonspire,
    cardType = Skullbreaker,
    cardIcon = CardIconVillage,
    cardCount = 8,
    cardClasses = [ClassWeapon,ClassBlunt],
    cardGold = Just 2,
    cardWeight = Just 3,
    cardPrice = Just 4,
    cardText = ["ATTACK +2",
                "DUNGEON: If you defeat a Monster this turn, put this "
                ++ "card on the top of your deck."],
    cardGlossary = []
    }

villageDetails SoulGem = initCardDetails {
    cardName = "Soul Gem",
    cardSource = Dragonspire,
    cardType = SoulGem,
    cardIcon = CardIconVillage,
    cardCount = 8,
    cardClasses = [ClassItem,ClassLight],
    cardGold = Just 2,
    cardPrice = Just 5,
    cardText = ["DUNGEON: LIGHT +1 for each Level of your highest "
                ++ "Level Hero."],
    cardGlossary = []
    }

villageDetails SpoiledFood = initCardDetails {
    cardName = "Spoiled Food",
    cardSource = Dragonspire,
    cardType = SpoiledFood,
    cardIcon = CardIconVillage,
    cardCount = 8,
    cardClasses = [ClassItem,ClassFood],
    cardGold = Just 1,
    cardPrice = Just 2,
    cardText = ["DUNGEON: Each Hero gains ATTACK +1 and STRENGTH +3.  "
                ++ "Destroy one attacking Hero at end of battle."],
    cardGlossary = []
    }

villageDetails ThunderRing = initCardDetails {
    cardName = "Thunder Ring",
    cardSource = Dragonspire,
    cardType = ThunderRing,
    cardIcon = CardIconVillage,
    cardCount = 8,
    cardClasses = [ClassItem,ClassLight,ClassMagic],
    cardGold = Just 1,
    cardPrice = Just 3,
    cardLight = Just 1,
    cardText = ["DUNGEON: Draw 1 card.  Reveal and discard the top card "
                ++ "of any opponent's deck.  If the discarded card is a "
                ++ "Hero, gain MAGIC ATTACK +2."],
    cardGlossary = []
    }

villageDetails TorynGauntlet = initCardDetails {
    cardName = "Toryn Gauntlet",
    cardSource = Dragonspire,
    cardType = TorynGauntlet,
    cardIcon = CardIconVillage,
    cardCount = 8,
    cardClasses = [ClassWeapon,ClassMagic],
    cardGold = Just 3,
    cardWeight = Just 2,
    cardPrice = Just 5,
    cardText = ["MAGIC ATTACK +2",
                "STRENGTH +2",
                "The equipped Hero may equip an additional Weapon."],
    cardGlossary = []
    }

villageDetails Trader = initCardDetails {
    cardName = "Trader",
    cardSource = Dragonspire,
    cardType = Trader,
    cardIcon = CardIconVillage,
    cardCount = 8,
    cardClasses = [ClassVillager,ClassMercenary],
    cardPrice = Just 4,
    cardText = ["VILLAGE: In addition to your buy, you may destroy "
                ++ "1 card to take any non-Hero card worth up to 2 "
                ++ "cost more from the village.",
                "DUNGEON: Draw 1 card.  If it is a Monster, draw "
                ++ "another card."],
    cardGlossary = []
    }




diseaseDetails :: DiseaseCard -> CardDetails DiseaseCard
diseaseDetails Disease = initCardDetails {
    cardName = "Disease",
    cardSource = ThunderstoneBase,
    cardType = Disease,
    cardIcon = CardIconBasic,
    cardCount = 10,
    cardClasses = [ClassDisease,ClassSpecial],
    cardText = ["* ATTACK -1"]
    }

diseaseDetails BalefulPlague = initCardDetails {
    cardName = "Baleful Plague",
    cardSource = DoomgateLegion,
    cardType = BalefulPlague,
    cardIcon = CardIconBasic,
    cardCount = 3,
    cardClasses = [ClassDisease,ClassSpecial],
    cardText = ["* All Heroes suffer Strength -1.  If any Hero's "
                ++ "Strength falls below 0 at any point during this "
                ++ "turn, it is destroyed at the end of the turn."]
    }

diseaseDetails Fatigue = initCardDetails {
    cardName = "Fatigue",
    cardSource = DoomgateLegion,
    cardType = Fatigue,
    cardIcon = CardIconBasic,
    cardCount = 3,
    cardClasses = [ClassDisease,ClassSpecial],
    cardText = ["* One Hero suffers Strength -2.  If the Hero's "
                ++ "Strength falls below 0 at any point during this "
                ++ "turn, it is destroyed at the end of the turn."]
    }

diseaseDetails Leprosy = initCardDetails {
    cardName = "Leprosy",
    cardSource = DoomgateLegion,
    cardType = Leprosy,
    cardIcon = CardIconBasic,
    cardCount = 3,
    cardClasses = [ClassDisease,ClassSpecial],
    cardText = ["VILLAGE: This card must be played when Visiting the "
                ++ "Village.  Increase the cost of each card purchased "
                ++ "by 3 gold."]
    }

diseaseDetails Malaise = initCardDetails {
    cardName = "Malaise",
    cardSource = DoomgateLegion,
    cardType = Malaise,
    cardIcon = CardIconBasic,
    cardCount = 3,
    cardClasses = [ClassDisease,ClassSpecial],
    cardText = ["* ATTACK -2"]
    }

diseaseDetails ThundersCurse = initCardDetails {
    cardName = "Thunder's Curse",
    cardSource = DoomgateLegion,
    cardType = ThundersCurse,
    cardIcon = CardIconBasic,
    cardCount = 3,
    cardVictoryPoints = Just (-2),
    cardClasses = [ClassDisease,ClassSpecial],
    cardText = ["* ATTACK -1"]
    }




dungeonFeatureDetails :: DungeonFeatureCard -> CardDetails DungeonFeatureType

dungeonFeatureDetails TheCage = initCardDetails {
    cardName = "The Cage",
    cardSource = WrathOfTheElements,
    cardType = DungeonFeatureTrapDire,
    cardIcon = CardIconDungeon,
    cardCount = 2,
    cardClasses = [ClassTrap,ClassDire],
    cardText = ["Place The Cage aside with one Hero from your hand and "
                ++ "refill the Hall.  If any player defeats any Monster, "
                ++ "destroy The Cage and place the Hero in his discard "
                ++ "pile."]
    }

dungeonFeatureDetails DeliriumPoison = initCardDetails {
    cardName = "Delirium Poison",
    cardSource = WrathOfTheElements,
    cardType = DungeonFeatureTrapDire,
    cardIcon = CardIconDungeon,
    cardCount = 2,
    cardClasses = [ClassTrap,ClassDire],
    cardText = ["All other players reveal their hand and destroy all "
                ++ "revealed Spells.  "
                ++ "Destroy this Trap and refill the Hall."]
    }

dungeonFeatureDetails PoisonGasTrap = initCardDetails {
    cardName = "Poison Gas Trap",
    cardSource = WrathOfTheElements,
    cardType = DungeonFeatureTrapDire,
    cardIcon = CardIconDungeon,
    cardCount = 2,
    cardClasses = [ClassTrap,ClassDire],
    cardText = ["Each player gains one Disease unless they reveal a "
                ++ "Cleric or Thief from their hands.  "
                ++ "Destroy this Trap and refill the Hall."]
    }

dungeonFeatureDetails PitTrap = initCardDetails {
    cardName = "Pit Trap",
    cardSource = WrathOfTheElements,
    cardType = DungeonFeatureTrapDeath,
    cardIcon = CardIconDungeon,
    cardCount = 3,
    cardClasses = [ClassTrap,ClassDeath],
    cardText = ["Each player reveals one Hero, or reveals a hand "
                ++ "containing none.  If a player reveals a Hero that "
                ++ "is not a thief, that player destroys that Hero.  "
                ++ "Destroy this Trap and refill the Hall."]
    }

dungeonFeatureDetails RollingBoulder = initCardDetails {
    cardName = "Rolling Boulder",
    cardSource = WrathOfTheElements,
    cardType = DungeonFeatureTrapDeath,
    cardIcon = CardIconDungeon,
    cardCount = 3,
    cardClasses = [ClassTrap,ClassDeath],
    cardText = ["Each player, starting with the player on your left, "
                ++ "flips a card from the top of their deck onto his "
                ++ "discard pile.  Continue until three Heroes have been "
                ++ "revealed.  Destroy the revealed Heroes.  "
                ++ "Destroy this Trap and refill the Hall."]
    }

dungeonFeatureDetails GreedAmulet = initCardDetails {
    cardName = "Greed Amulet",
    cardSource = DoomgateLegion,
    cardType = DungeonFeatureAmuletTreasures,
    cardIcon = CardIconNone,
    cardCount = 2,
    cardClasses = [ClassAmuletTreasure],
    cardText = ["TREASURE",
                "VILLAGE: Destroy Greed Amulet to gain 4 Gold."]
    }

dungeonFeatureDetails NaturesAmulet = initCardDetails {
    cardName = "Nature's Amulet",
    cardSource = DoomgateLegion,
    cardType = DungeonFeatureAmuletTreasures,
    cardIcon = CardIconNone,
    cardCount = 2,
    cardClasses = [ClassAmuletTreasure],
    cardText = ["TREASURE",
                "DUNGEON: Destroy Nature's Amulet to gain MAGIC ATTACK +3."]
    }

dungeonFeatureDetails StrengthAmulet = initCardDetails {
    cardName = "Strength Amulet",
    cardSource = DoomgateLegion,
    cardType = DungeonFeatureAmuletTreasures,
    cardIcon = CardIconNone,
    cardCount = 2,
    cardClasses = [ClassAmuletTreasure],
    cardText = ["TREASURE",
                "DUNGEON: Destroy Strength Amulet to gain ATTACK +3."]
    }

dungeonFeatureDetails UlbricksArmor = initCardDetails {
    cardName = "Ulbrick's Armor",
    cardSource = DoomgateLegion,
    cardType = DungeonFeatureUlbricksTreasures,
    cardIcon = CardIconNone,
    cardCount = 2,
    cardClasses = [ClassUlbricksTreasure],
    cardText = ["TREASURE",
                "Destroy Ulbrick's Armor and all revealed non-Hero cards.  "
                ++ "Draw two cards for each card destroyed."]
    }

dungeonFeatureDetails UlbricksGauntlets = initCardDetails {
    cardName = "Ulbrick's Gauntlets",
    cardSource = DoomgateLegion,
    cardType = DungeonFeatureUlbricksTreasures,
    cardIcon = CardIconNone,
    cardCount = 1,
    cardClasses = [ClassUlbricksTreasure],
    cardText = ["TREASURE",
                "DUNGEON: If you defeat a monster worth less than 4 VP, "
                ++ "you may Destroy Ulbrick's Gauntlets to take another "
                ++ "turn."]
    }

dungeonFeatureDetails UlbricksHelmet = initCardDetails {
    cardName = "Ulbrick's Helmet",
    cardSource = DoomgateLegion,
    cardType = DungeonFeatureUlbricksTreasures,
    cardIcon = CardIconNone,
    cardCount = 3,
    cardClasses = [ClassUlbricksTreasure],
    cardText = ["TREASURE",
                "Destroy Ulbrick's Helmet and draw three cards."]
    }

dungeonFeatureDetails DragonsClaws = initCardDetails {
    cardName = "Dragon's Claws",
    cardSource = Dragonspire,
    cardType = DungeonFeatureTrapDraconic,
    cardIcon = CardIconDungeon,
    cardCount = 2,
    cardClasses = [ClassTrap,ClassDraconic],
    cardText = ["Each player reveals a Thief or reveals their hand and "
                ++ "destroys all Villager cards within it."]
    }

dungeonFeatureDetails DragonsJaw = initCardDetails {
    cardName = "Dragon's Jaw",
    cardSource = Dragonspire,
    cardType = DungeonFeatureTrapDraconic,
    cardIcon = CardIconDungeon,
    cardCount = 2,
    cardClasses = [ClassTrap,ClassDraconic],
    cardText = ["Each player, starting with the player to the active "
                ++ "player's left, reveals a card from his deck.  "
                ++ "If it is a Weapon, destroy it.  Continue until 3 "
                ++ "Weapons have been destroyed."]
    }

dungeonFeatureDetails DragonsTeeth = initCardDetails {
    cardName = "Dragon's Teeth",
    cardSource = Dragonspire,
    cardType = DungeonFeatureTrapDraconic,
    cardIcon = CardIconDungeon,
    cardCount = 2,
    cardClasses = [ClassTrap,ClassDraconic],
    cardText = ["The active player must destroy his Hero with the "
                ++ "lowest Strength.  Each other player reveals and "
                ++ "discards 3 cards from the top of his deck.  Destroy "
                ++ "all Heroes except those with highest Strength values."]
    }

dungeonFeatureDetails DragonsWords = initCardDetails {
    cardName = "Dragon's Words",
    cardSource = Dragonspire,
    cardType = DungeonFeatureTrapDraconic,
    cardIcon = CardIconDungeon,
    cardCount = 2,
    cardClasses = [ClassTrap,ClassDraconic],
    cardText = ["If this Trap enters the Hall after a Monster has "
                ++ "been defeated, destroy the Monster card unless "
                ++ "the active player reveals a Thief.  Each other "
                ++ "player reveals the top card of his deck and "
                ++ "destroys any Monsters revealed."]
    }

dungeonFeatureDetails EmeraldDragon = initCardDetails {
    cardName = "Emerald Dragon",
    cardSource = Dragonspire,
    cardType = DungeonFeatureFigurineTreasure,
    cardIcon = CardIconDungeon,
    cardCount = 2,
    cardClasses = [ClassFigurine,ClassTreasure],
    cardText = ["TREASURE",
                "When you defeat a Monster, you may place a card from "
                ++ "your hand face down under this card and set them "
                ++ "aside.  The next time a player defeats a Monster "
                ++ "worth 6 VP or more, you must destroy this card and "
                ++ "place the face down card in that player's discard "
                ++ "pile."]
    }

dungeonFeatureDetails IvoryDragon = initCardDetails {
    cardName = "Ivory Dragon",
    cardSource = Dragonspire,
    cardType = DungeonFeatureFigurineTreasure,
    cardIcon = CardIconDungeon,
    cardCount = 2,
    cardClasses = [ClassFigurine,ClassTreasure],
    cardText = ["TREASURE",
                "VILLAGE: Destroy this card and any number of cards "
                ++ "from your hand.  Gain 1 XP for each card destroyed "
                ++ "in this manner."]
    }

dungeonFeatureDetails RubyDragon = initCardDetails {
    cardName = "Ruby Dragon",
    cardSource = Dragonspire,
    cardType = DungeonFeatureFigurineTreasure,
    cardIcon = CardIconDungeon,
    cardCount = 2,
    cardClasses = [ClassFigurine,ClassTreasure],
    cardText = ["TREASURE",
                "Destroy this card at any time to prevent 1 "
                ++ "Battle Effect from affecting you."]
    }

dungeonFeatureDetails SapphireDragon = initCardDetails {
    cardName = "Sapphire Dragon",
    cardSource = Dragonspire,
    cardType = DungeonFeatureFigurineTreasure,
    cardIcon = CardIconDungeon,
    cardCount = 2,
    cardClasses = [ClassFigurine,ClassTreasure],
    cardText = ["TREASURE",
                "When you defeat a Monster, you may set that Monster "
                ++ "aside with this card.  Return both cards to your "
                ++ "deck at the end of the game."]
    }

dungeonFeatureDetails BladeTrap = initCardDetails {
    cardName = "Blade Trap",
    cardSource = WrathOfTheElements,
    cardType = DungeonFeatureTrapDeath,
    cardIcon = CardIconDungeon,
    cardCount = 1,
    cardClasses = [ClassTrap,ClassDeath],
    cardText = ["Destroy the Hero from your hand with the lowest "
                ++ "Strength.  All other players reveal the top two cards "
                ++ "from their decks.  Destroy all revealed Heroes except "
                ++ "the revealed Hero or Heroes with the highest Strength."]
    }





guardianDetails :: GuardianCard -> CardDetails DungeonFeatureType
guardianDetails DarkChampion = initCardDetails {
    cardName = "Dark Champion",
    cardSource = WrathOfTheElements,
    cardType = DungeonFeatureGuardian,
    cardIcon = CardIconDungeon,
    cardCount = 1,
    cardClasses = [ClassDoomknight,ClassGuardian],
    cardHealth = Just 12,
    cardGold = Just 1,
    cardXP = Just 3,
    cardVictoryPoints = Just 4,
    cardText = ["Cannot be removed from the Dungeon Hall unless defeated.",
                "BREACH: Move the Dark Champion to Rank 0.  Each turn the "
                ++ "Dark Champion remains in Rank 0, the active player "
                ++ "must destroy one card from his hand at the end of "
                ++ "his turn."]
    }

guardianDetails UnholyGuardian = initCardDetails {
    cardName = "Unholy Guardian",
    cardSource = DoomgateLegion,
    cardType = DungeonFeatureGuardian,
    cardIcon = CardIconDungeon,
    cardCount = 1,
    cardClasses = [ClassGuardian,ClassDoomknight],
    cardHealth = Just 14,
    cardGold = Just 1,
    cardXP = Just 3,
    cardVictoryPoints = Just 5,
    cardText = ["BREACH: Move to Rank 0.  If the Thunderstone starts a "
                ++ "turn in the hall, place it on the bottom of the "
                ++ "Dungeon Deck and refill the hall."]
    }

guardianDetails GuardianOfNight = initCardDetails {
    cardName = "Guardian of Night",
    cardSource = Dragonspire,
    cardType = DungeonFeatureGuardian,
    cardIcon = CardIconDungeon,
    cardCount = 1,
    cardClasses = [ClassDoomknight,ClassGuardian],
    cardHealth = Just 12,
    cardGold = Just 2,
    cardXP = Just 3,
    cardVictoryPoints = Just 5,
    cardText = ["GLOBAL: Double the basic Light penalties for each Rank.",
                "BREACH: Move to Rank 0."]
    }

guardianDetails GuardianOfTorment = initCardDetails {
    cardName = "Guardian of Torment",
    cardSource = Dragonspire,
    cardType = DungeonFeatureGuardian,
    cardIcon = CardIconDungeon,
    cardCount = 1,
    cardClasses = [ClassGuardian],
    cardHealth = Just 14,
    cardGold = Just 1,
    cardXP = Just 3,
    cardVictoryPoints = Just 6,
    cardText = ["GLOBAL: Battle Effects for Monsters are repeated "
                ++ "an additional time per battle while the Guardian is "
                ++ "in Rank 0.",
                "BREACH: Move to Rank 0."]
    }

guardianDetails DeathSentinel = initCardDetails {
    cardName = "Death Sentinel",
    cardSource = WrathOfTheElements,
    cardType = DungeonFeatureGuardian,
    cardIcon = CardIconDungeon,
    cardCount = 1,
    cardClasses = [ClassDoomknight,ClassGuardian],
    cardHealth = Just 10,
    cardGold = Just 2,
    cardXP = Just 1,
    cardVictoryPoints = Just 3,
    cardText = ["Cannot be removed from the Dungeon Hall unless defeated.",
                "BREACH: Move the Death Sentinel to Rank 0.  While "
                ++ "the Death Sentinel remains in Rank 0, all other "
                ++ "Monsters gain +1 Health."]
    }

cardsOfType :: (Bounded card, Enum card, Eq cardType) => 
               (card -> CardDetails cardType) -> cardType
            -> [card]
cardsOfType details ofType = concatMap countOff cards
  where
    cards =
        filter ((== ofType) . cardType . details) [minBound .. maxBound]
    countOff card = replicate (cardCount $ details card) card

levelsUpTo :: HeroCard -> HeroCard -> Bool
oldHero `levelsUpTo` newHero
  | newHero == Militia = False
  | oldHero == Militia = newLevel == 1
  | otherwise = oldType == newType && oldLevel + 1 == newLevel
  where
    oldType = cardType $ heroDetails oldHero
    CardIconHero oldLevel = cardIcon $ heroDetails oldHero
    newType = cardType $ heroDetails newHero
    CardIconHero newLevel = cardIcon $ heroDetails newHero
