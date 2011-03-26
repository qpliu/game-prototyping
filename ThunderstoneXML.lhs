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
> xmlItem item value = xmlString item (show value)

> xmlOption :: Show a => String -> Maybe a -> String
> xmlOption item value = maybe "" (xmlItem item) value

> xmlString :: String -> String -> String
> xmlString item value = "<" ++ item ++ ">" ++ value ++ "</" ++ item ++ ">"

> xmlList :: String -> [String] -> String
> xmlList item values =
>     concat ["<" ++ item ++ ">" ++ value ++ "</" ++ item ++ ">"
>             | value <- values]

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
>     "<card id=\"" ++ show card ++ "\" type=\"" ++ kind ++ "\">"
>     ++ xmlString "name" (cardName details)
>     ++ xmlItem "source" (cardSource details)
>     ++ xmlItem "group" (cardType details)
>     ++ xmlString "icon" (drop 8 $ show $ cardIcon details)
>     ++ xmlItem "count" (cardCount details)
>     ++ xmlList "class" (map (drop 5 . show) (cardClasses details))
>     ++ xmlOption "gold" (cardGold details)
>     ++ xmlOption "light" (cardLight details)
>     ++ xmlOption "vp" (cardVictoryPoints details)
>     ++ xmlOption "strength" (cardStrength details)
>     ++ xmlOption "price" (cardPrice details)
>     ++ xmlOption "xp" (cardXP details)
>     ++ xmlOption "health" (cardHealth details)
>     ++ xmlOption "weight" (cardWeight details)
>     ++ xmlOption "levelup" (cardLevelUp details)
>     ++ xmlList "text" (map xmlQuote (cardText details))
>     ++ xmlList "glossary" (cardGlossary details)
>     ++ "</card>"
>   where
>     details = cardDetails card

> instance ThunderstoneXML ThunderstoneCard where
>     thunderstoneXML card =
>         thunderstoneCardXML "Thunderstone" thunderstoneDetails card

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
>         "<card id=\"Disease\" type=\"Disease\">"
>         ++ xmlString "name" "Disease"
>         ++ xmlItem "source" ThunderstoneBase
>         ++ xmlString "group" "Disease"
>         ++ xmlString "icon" (drop 8 $ show $ CardIconBasic)
>         ++ xmlItem "count" 15
>         ++ xmlList "class" [drop 5 $ show ClassDisease]
>         ++ xmlList "text" ["* ATTACK -1"]
>         ++ xmlList "glossary"
>                    ["Disease: Any time a Disease card is revealed, it "
>                     ++ "must be played.  It has no effect in the Village."]
>         ++ "</card>"

> cardsFrom :: [Source] -> String
> cardsFrom source =
>     xmlString "cards"
>         (cardsXML heroDetails
>          ++ cardsXML villageDetails
>          ++ cardsXML monsterDetails
>          ++ thunderstoneXML Disease)
>   where
>     cardsXML cardDetails =
>         concat [thunderstoneXML card
>                 | card <- [minBound..maxBound],
>                   cardSource (cardDetails card) `elem` source]

> main :: IO ()
> main = putStrLn $ cardsFrom [ThunderstoneBase]
