> import ThunderstoneCards

> xmlQuote :: String -> String
> xmlQuote = concatMap xmlQuoteChar
>   where
>    xmlQuoteChar '<' = "&lt;"
>    xmlQuoteChar '>' = "&gt;"
>    xmlQuoteChar '&' = "&amp;"
>    xmlQuoteChar '"' = "&dquot;"
>    xmlQuoteChar c = [c]

> xmlItem :: Show a => String -> a -> String
> xmlItem item value = xmlItemStr item (show value)

> xmlItemStr :: String -> String -> String
> xmlItemStr item value = "<" ++ item ++ ">" ++ value ++ "</" ++ item ++ ">"

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
  <defense>...</defense>
  <gold>...</gold>
  <weight>...</weight>
  <price>...</price>
  <light>...</light>
  <levelup>...</levelup>
  <levelupTo>...</levelupTo>...
  <xp>...</xp>
  <vp>...</vp>
</card>

> class ThunderstoneXML item where
>     thunderstoneXML :: item -> String

> thunderstoneCardXML ::
>     (Show card, Show cardType, ThunderstoneXML cardStats) =>
>     String -> (card -> CardDetails cardType cardStats) -> card -> String
> thunderstoneCardXML kind cardDetails card =
>     thunderstoneCardXML' kind (cardDetails card) thunderstoneXML card

> thunderstoneCardXML' :: (Show cardType, Show card) =>
>                         String
>                      -> CardDetails cardType cardStats
>                      -> (cardStats -> String)
>                      -> card
>                      -> String
> thunderstoneCardXML' kind cardDetails cardStatsXML card =
>     "<card id=\"" ++ show card ++ "\">"
>     ++ xmlItem "source" (cardSource cardDetails)
>     ++ (if null (cardClarification cardDetails) then "" else
>         xmlItemStr "glossary" (xmlQuote $ cardClarification cardDetails))
>     ++ xmlItemStr "type" kind
>     ++ xmlItem "count" (cardCount cardDetails)
>     ++ xmlItemStr "name" (cardName cardDetails)
>     ++ xmlItem "group" (cardType cardDetails)
>     ++ xmlItemStr "icon" (drop 8 $ show $ cardIcon cardDetails)
>     ++ concat [xmlItemStr "class" (drop 5 $ show cl)
>                | cl <- cardClasses cardDetails]
>     ++ concat [xmlItemStr "text" (xmlQuote text)
>                | text <- cardText cardDetails]
>     ++ (if cardGold cardDetails > 0
>             || cardClasses cardDetails == [ClassMilitia, ClassHero]
>           then xmlItem "gold" (cardGold cardDetails) else "")
>     ++ (if cardLight cardDetails > 0
>           then xmlItem "light" (cardLight cardDetails) else "")
>     ++ (if cardVictoryPoints cardDetails > 0
>           then xmlItem "vp" (cardVictoryPoints cardDetails) else "")
>     ++ cardStatsXML (cardStats cardDetails)
>     ++ "</card>"

> instance ThunderstoneXML ThunderstoneCard where
>     thunderstoneXML card =
>         thunderstoneCardXML' "Thunderstone"
>             (thunderstoneDetails card) (const "") card

> instance ThunderstoneXML GuardianCard where
>     thunderstoneXML card = undefined

> instance ThunderstoneXML HeroCard where
>     thunderstoneXML card = thunderstoneCardXML "Hero" heroDetails card

> instance ThunderstoneXML VillageCard where
>     thunderstoneXML card = thunderstoneCardXML "Village" villageDetails card

> instance ThunderstoneXML MonsterCard where
>     thunderstoneXML card = thunderstoneCardXML "Monster" monsterDetails card

> instance ThunderstoneXML DiseaseCard where
>     thunderstoneXML card =
>         "<card id=\"Disease\">"
>         ++ xmlItem "source" ThunderstoneBase
>         ++ xmlItemStr "glossary" "Disease: Any time a Disease card is revealed, it must be played.  It has no effect in the Village."
>         ++ xmlItemStr "type" "Disease"
>         ++ xmlItem "count" 15
>         ++ xmlItemStr "name" "Disease"
>         ++ xmlItem "group" "Disease"
>         ++ xmlItemStr "icon" (drop 8 $ show $ CardIconBasic)
>         ++ concat [xmlItemStr "class" (drop 5 $ show cl)
>                    | cl <- [ClassDisease]]
>         ++ concat [xmlItemStr "text" (xmlQuote text)
>                    | text <- ["* ATTACK -1"]]
>         ++ "</card>"

> instance ThunderstoneXML HeroStats where
>     thunderstoneXML stats =
>         xmlItem "strength" (heroStrength stats)
>         ++ xmlItem "level" (heroLevel stats)
>         ++ xmlItem "price" (heroPrice stats)
>         ++ (if null (snd $ heroUpgrade stats) then ""
>               else xmlItem "levelup" (fst $ heroUpgrade stats)
>                    ++ concat [xmlItem "levelupTo" item
>                               | item <- snd $ heroUpgrade stats])

> instance ThunderstoneXML VillageStats where
>     thunderstoneXML stats =
>         xmlItem "price" (villagePrice stats)
>         ++ (if villageWeight stats > 0
>               then xmlItem "weight" (villageWeight stats) else "")

> instance ThunderstoneXML MonsterStats where
>     thunderstoneXML stats =
>         xmlItem "health" (monsterHealth stats)
>         ++ xmlItem "xp" (monsterXP stats)

> cardsFrom :: Source -> String
> cardsFrom source =
>     xmlItemStr "cards"
>         (cardsXML heroDetails
>          ++ cardsXML villageDetails
>          ++ cardsXML monsterDetails
>          ++ thunderstoneXML Disease)
>   where
>     cardsXML cardDetails =
>         concat [thunderstoneXML card
>                 | card <- [minBound..maxBound],
>                   cardSource (cardDetails card) == source]

> main :: IO ()
> main = putStrLn $ cardsFrom ThunderstoneBase
