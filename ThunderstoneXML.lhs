> import Data.Char(isUpper)

> import ThunderstoneCards
> import ThunderstoneCardDetails

> xmlQuote :: String -> String
> xmlQuote = concatMap xmlQuoteChar
>   where
>    xmlQuoteChar '<' = "&lt;"
>    xmlQuoteChar '>' = "&gt;"
>    xmlQuoteChar '&' = "&amp;"
>    xmlQuoteChar '"' = "&dquot;"
>    xmlQuoteChar c = [c]

> xmlItem :: Show a => String -> a -> String
> xmlItem item value = xmlString item (show value)

> xmlOption :: Show a => String -> Maybe a -> String
> xmlOption item value = maybe "" (xmlItem item) value

> xmlString :: String -> String -> String
> xmlString item value =
>     "    <" ++ item ++ ">" ++ value ++ "</" ++ item ++ ">\n"

> xmlList :: String -> [String] -> String
> xmlList item values
>   | null values = ""
>   | otherwise = xmlContainer "    " item ""
>                     (concatMap (("  " ++) . xmlString "item") values)

> xmlContainer :: String -> String -> String -> String -> String
> xmlContainer indent element attrs contents =
>     indent ++ "<" ++ element ++ attrs ++ ">\n" ++ contents ++ indent
>            ++ "</" ++ element ++ ">\n"

<card id="">
  <!-- for all cards -->
  <source>...</source>
  <kind>...</kind>
  <count>...</count>
  <glossary>...</glossary>

  <!-- on all cards -->
  <name>...</name>
  <type>...</type>
  <icon>...</icon>
  <class>...</class>...
  <text>...</text>...

  <!-- on some cards -->
  <health>...</health>
  <gold>...</gold>
  <weight>...</weight>
  <price>...</price>
  <light>...</light>
  <levelup>...</levelup>
  <xp>...</xp>
  <vp>...</vp>
</card>

> class ThunderstoneXML item where
>     thunderstoneXML :: item -> String

> thunderstoneCardXML :: (Show cardType, Show card) =>
>                        String -> (card -> CardDetails cardType) -> card
>                     -> String
> thunderstoneCardXML kind cardDetails card =
>     xmlContainer "  " kind (" id=\"" ++ show card ++ "\"") $
>        xmlString "name" (cardName details)
>     ++ xmlItem "source" (cardSource details)
>     ++ xmlItem "group" (cardType details)
>     ++ xmlItem "icon" (cardIcon details)
>     ++ xmlItem "count" (cardCount details)
>     ++ xmlList "class" (map showCardClass (cardClasses details))
>     ++ xmlOption "gold" (cardGold details)
>     ++ xmlOption "light" (cardLight details)
>     ++ xmlOption "vp" (cardVictoryPoints details)
>     ++ xmlOption "strength" (cardStrength details)
>     ++ xmlOption "price" (cardPrice details)
>     ++ xmlOption "xp" (cardXP details)
>     ++ xmlOption "health" (cardHealth details)
>     ++ xmlOption "weight" (cardWeight details)
>     ++ xmlOption "levelup" (cardLevelUp details)
>     ++ xmlList "effects" (map textItem (cardText details))
>     ++ xmlList "glossary" (cardGlossary details)
>     ++ xmlString "layout" "low"
>   where
>     details = cardDetails card
>     textItem text = "\n        <text>" ++ xmlQuote text ++ "</text>\n      "

> showCardClass :: CardClass -> String
> showCardClass ClassUlbricksTreasure = "Ulbrick's Treasure"
> showCardClass cardClass =
>     unwords $ drop 1 $ words $ concatMap splitCamelCase $ show cardClass
>   where
>     splitCamelCase char | isUpper char = [' ',char] | otherwise = [char]

> instance ThunderstoneXML ThunderstoneCard where
>     thunderstoneXML card =
>         thunderstoneCardXML "thunderstone" thunderstoneDetails card

> instance ThunderstoneXML GuardianCard where
>     thunderstoneXML card =
>         thunderstoneCardXML "guardian" guardianDetails card

> instance ThunderstoneXML HeroCard where
>     thunderstoneXML card = thunderstoneCardXML "hero" heroDetails card

> instance ThunderstoneXML VillageCard where
>     thunderstoneXML card = thunderstoneCardXML "village" villageDetails card

> instance ThunderstoneXML MonsterCard where
>     thunderstoneXML card = thunderstoneCardXML "monster" monsterDetails card

> instance ThunderstoneXML DiseaseCard where
>     thunderstoneXML card =
>         thunderstoneCardXML "disease" diseaseDetails card

> instance ThunderstoneXML DungeonFeatureCard where
>     thunderstoneXML card =
>         thunderstoneCardXML "dungeonfeature" dungeonFeatureDetails card

> cardsFrom :: [Source] -> String
> cardsFrom source =
>     xmlString "cards"
>         (cardsXML thunderstoneDetails
>          ++ cardsXML heroDetails
>          ++ cardsXML villageDetails
>          ++ cardsXML monsterDetails
>          ++ cardsXML diseaseDetails
>          ++ cardsXML dungeonFeatureDetails
>          ++ cardsXML guardianDetails)
>   where
>     cardsXML cardDetails =
>         concat [thunderstoneXML card
>                 | card <- [minBound..maxBound],
>                   cardSource (cardDetails card) `elem` source]

> main :: IO ()
> main =
>     putStr $ xmlContainer "" "cards" ""
>            $ cardsXML (cards :: [ThunderstoneCard])
>              ++ cardsXML (cards :: [HeroCard])
>              ++ cardsXML (cards :: [VillageCard])
>              ++ cardsXML (cards :: [MonsterCard])
>              ++ cardsXML (cards :: [DiseaseCard])
>              ++ cardsXML (cards :: [DungeonFeatureCard])
>              ++ cardsXML (cards :: [GuardianCard])
>   where
>     cards :: (Bounded card, Enum card, ThunderstoneXML card) => [card]
>     cards = [minBound..maxBound]
>     cardsXML cards = concatMap thunderstoneXML cards
