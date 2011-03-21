Prototype the Thunderstone base game.  Avoid the complexities of the
expansions to work out how to implement the basic mechanisms.

For those who do not know Haskell, please note that return does not
mean the same thing it means in most other languages.  It does not
cause the function being defined to exit with the given value.  (Most
of the time, return takes a value of type t and returns a value of
type IO t.  In this file, return takes a value of type t and returns
a value of type Thunderstone t.  Both are special cases of the
generic type signature return :: Monad m => a -> m a, which, in
Java syntax would be
    <A, M<A> extends Monad<A>> M<A> returnMonad(A a);
or something like that.)


I'm pretty happy with the game setup code.  I like the functions
firstGame, randomizedGame and specifiedGame.

I'm pretty happy with Card and HeroCard, MonsterCard, VillageCard,
and ThunderstoneCard data structures, which are exported.  I'm not
so happy with the CardDetails and the CardProperties data structures,
which are not exported.

I'm somewhat satisfied with the exported interface, though I am
not so happy with ThunderstonePlayerState or PlayerOption.

I like the model of PlayerActions transforming the ThunderstoneState
and yielding ThunderstoneEvents, as well as having
ThunderstonePlayerState contain all the valid PlayerActions, but
I'm not satisfied with PlayerActions, which still remains mostly
undefined.

The internal PlayerState is messy and mostly undefined.  The same is
true of the internal CardProperties.  The inchoate vision of how
the dungeon will work is to have the cardBattleResult of the
CardProperties of the MonsterCard contain the code that resolves
the battle.  It's hard to see how to avoid lots of special casing.


> module ThunderstoneBase

The interface:

Data types:

ThunderstoneState and PlayerId are opaque.

>     (ThunderstoneState,PlayerId,
>      Card(..),ThunderstonePlayerState(..),
>      PlayerAction(..),PlayerOption(..),
>      ThunderstoneEvent(..),
>      GameSetup,

Initialization:

>      thunderstoneInit,

Game setup:

>      firstGame,randomizedGame,specifiedGame,
>      thunderstoneStartGame,

Game state:

>      thunderstoneIsGameOver,
>      thunderstonePlayerIds,
>      thunderstonePlayerState,
>      thunderstoneScores,

Game mechanics:

>      thunderstoneTakeAction)

> where

> import Control.Monad(filterM,mapM,mapM_,replicateM,unless,when)
> import Data.List(nub,partition,sortBy,(\\))
> import Data.Maybe(catMaybes,listToMaybe)
> import System.Random(StdGen)

> import qualified Shuffle
> import StateTransformer
>     (StateTransformer,getState,runStateTransformer,setState)
> import ThunderstoneCards
>     (HeroType(..),MonsterType(..),
>      ThunderstoneCard(..),HeroCard(..),MonsterCard(..),VillageCard(..),
>      CardClass(..),CardDetails(..),
>      thunderstoneDetails,
>      HeroStats(..),heroDetails,
>      MonsterStats(..),monsterDetails,
>      VillageStats(..),villageDetails,
>      cardsOfType)

==============================================================================
Exported functions:

Initialize the state with the random number generator.

> thunderstoneInit :: StdGen -> ThunderstoneState
> thunderstoneInit stdGen = ThunderstoneState {
>     thunderstoneStdGen = stdGen,
>     thunderstoneCurrentPlayer = undefined,
>     thunderstonePlayers = [],
>     thunderstoneDungeon = undefined,
>     thunderstoneHeroes = undefined,
>     thunderstoneVillage = undefined,
>     thunderstoneGameOver = undefined
>     }

Start a game, specifying the number of players and choosing which
cards are in the game, which can be either a specific set of cards,
or randomly chosen cards.

> thunderstoneStartGame :: ThunderstoneState -> Int -> GameSetup
>                       -> (ThunderstoneState,[PlayerId])
> thunderstoneStartGame state numberOfPlayers gameSetup =
>     runStateTransformer (setup numberOfPlayers gameSetup) state

Various queries about the state of the game.

> data ThunderstonePlayerState = ThunderstonePlayerState {
>     playerStateHand :: [Card],
>     playerStateDeck :: Int,
>     playerStateDiscards :: Int,
>     playerStateXP :: Int,
>     playerStateOptions :: [PlayerAction],
>     playerStateDungeonHall :: [Card],
>     playerStateHeroes :: [(HeroType,[HeroCard])],
>     playerStateVillage :: [(VillageCard,Int)]
>     }

> thunderstoneIsGameOver :: ThunderstoneState -> Bool
> thunderstoneIsGameOver state = thunderstoneGetState state isGameOver

> thunderstonePlayerIds :: ThunderstoneState -> [PlayerId]
> thunderstonePlayerIds state = thunderstoneGetState state getPlayerIds

> thunderstonePlayerState :: ThunderstoneState -> PlayerId
>                         -> ThunderstonePlayerState
> thunderstonePlayerState state playerId =
>     thunderstoneGetState state (getThunderstonePlayerState playerId)

> thunderstoneScores :: ThunderstoneState -> [(PlayerId,Int)]
> thunderstoneScores state = thunderstoneGetState state getScores

A player takes an action.  If the action is legal, the updated state
and a list of events are returned.

> thunderstoneTakeAction :: ThunderstoneState -> PlayerId -> PlayerAction
>                        -> Maybe (ThunderstoneState,[ThunderstoneEvent])
> thunderstoneTakeAction state playerId playerAction =
>     fmap ((,) updatedState) events
>   where
>     (updatedState,events) =
>         runStateTransformer (takeAction playerId playerAction) state

The cards for the first game.

> firstGame :: GameSetup
> firstGame = return (monsters,heroes,village)
>   where
>     monsters = [MonsterEnchanted, MonsterOoze, MonsterUndeadDoom]
>     heroes = [HeroElf, HeroLorigg, HeroRegian, HeroThyrian]
>     village = [BattleFury, Fireball, FlamingSword, LightstoneGem,
>                MagicalAura, ShortSword, Spear, TownGuard]

Use the Randomizer cards to choose three random monsters, four
random heros, and eight village cards. 

For a longer game, try four or more monsters.

> randomizedGame :: GameSetup
> randomizedGame = do
>     monsters <- shuffle [MonsterAbyssal .. MonsterUndeadSpirit]
>     heroes <- shuffle [HeroAmazon .. HeroThyrian]
>     village <- shuffle [ArcaneEnergies .. Warhammer]
>     return (take 3 monsters, take 4 heroes, take 8 village)

Specify the cards in the game.  If not enough are specified, randomly
choose from the remaining cards.  If too many are specified, randomly
choose among the specified cards.

> specifiedGame :: [MonsterType] -> [HeroType] -> [VillageCard] -> GameSetup
> specifiedGame monsters heroes village = do
>     randomMonsters <- random monsters [MonsterAbyssal .. MonsterUndeadSpirit]
>     randomHeroes <- random heroes [HeroAmazon .. HeroThyrian]
>     randomVillage <- random village [ArcaneEnergies .. Warhammer]
>     return (take 3 randomMonsters, take 4 randomHeroes, take 8 randomVillage)
>   where
>     random specified others = do
>         shuffledSpecified <- shuffle (nub specified)
>         shuffledOthers <- shuffle (others \\ shuffledSpecified)
>         return (shuffledSpecified ++ shuffledOthers)

==============================================================================

> data Card =
>     MonsterCard MonsterCard
>   | HeroCard HeroCard
>   | VillageCard VillageCard
>   | DiseaseCard
>   | ThunderstoneCard ThunderstoneCard
>   deriving Eq

> instance Show Card where
>     show (MonsterCard card) = cardName $ monsterDetails card
>     show (HeroCard card) = cardName $ heroDetails card
>     show (VillageCard card) = cardName $ villageDetails card
>     show DiseaseCard = "Disease"
>     show (ThunderstoneCard card) = cardName $ thunderstoneDetails card

Game setup:

> type GameSetup = Thunderstone ([MonsterType],[HeroType],[VillageCard])

> setup :: Int -> GameSetup -> Thunderstone [PlayerId]
> setup numberOfPlayers gameSetup = do
>     (monsters,heroes,village) <- gameSetup
>     dungeon <- buildDungeon monsters
>     state <- getState
>     setState state {
>         thunderstoneCurrentPlayer = 0,
>         thunderstonePlayers = replicate numberOfPlayers newPlayer,
>         thunderstoneDungeon = dungeon,
>         thunderstoneHeroes = map heroStack (HeroMilitia:heroes),
>         thunderstoneVillage =
>             map villageStack ([Dagger, IronRations, Torch] ++ village),
>         thunderstoneGameOver = False
>         }
>     playerIds <- getPlayerIds
>     mapM_ drawStartingHand playerIds
>     currentPlayerId <- getCurrentPlayerId
>     setPlayerState currentPlayerId StartingTurn
>     return playerIds

>   where

>     newPlayer :: Player
>     newPlayer = Player {
>         playerHand = [],
>         playerDeck = [],
>         playerDiscard = [],
>         playerXP = 0,
>         playerState = Waiting
>         }

>     drawStartingHand :: PlayerId -> Thunderstone ()
>     drawStartingHand playerId = do
>         militia <- multiple 6 $ drawHero HeroMilitia
>         dagger <- multiple 2 $ drawResource Dagger
>         ironRations <- multiple 2 $ drawResource IronRations
>         torch <- multiple 2 $ drawResource Torch
>         discard playerId (map HeroCard militia)
>         discard playerId (map VillageCard (dagger ++ ironRations ++ torch))
>         hand <- multiple 6 $ drawCard playerId
>         setHand playerId hand

There are ten cards for each class.  Take all 30 Monster cards that match
the three selected classes and shuffle them together.  This becomes the
Dungeon Deck.  Count off ten Monster cards (without revealing them) and
shuffle them together with the special Thunderstone card.  Place these
eleven cards at the bottom of the Dungeon Deck.

>     buildDungeon :: [MonsterType] -> Thunderstone [Card]
>     buildDungeon monsters = do
>         dungeon <- shuffle $ map MonsterCard cards
>         shuffleToBottom 10 [ThunderstoneCard StoneOfMystery] dungeon
>       where
>         cards = concatMap (cardsOfType monsterDetails) monsters

Place both level 3 Hero cards in the stack.  Next, place all four level
2 Hero cards on top of the stack.  Finally, place all six level 1 Hero
cards on top of those.  This will create a stack of Hero cards with all
level 3 cards on the bottom, the level 2 cards in the middle, and the
level 1 cards on top.

>     heroStack :: HeroType -> (HeroType,[HeroCard])
>     heroStack hero = (hero,sortBy level (cardsOfType heroDetails hero))
>       where
>         level a b = compare (heroLevel $ cardStats $ heroDetails a)
>                             (heroLevel $ cardStats $ heroDetails b)

There are 15 of each basic card and 8 of each village card.

>     villageStack :: VillageCard -> (VillageCard,Int)
>     villageStack card = (card,length (cardsOfType villageDetails card))

Game state:

> type Thunderstone a = StateTransformer ThunderstoneState a

> newtype PlayerId = PlayerId Int

> data ThunderstoneState = ThunderstoneState {
>     thunderstoneStdGen :: StdGen,
>     thunderstoneCurrentPlayer :: Int,
>     thunderstonePlayers :: [Player],
>     thunderstoneDungeon :: [Card],
>     thunderstoneHeroes :: [(HeroType,[HeroCard])],
>     thunderstoneVillage :: [(VillageCard,Int)],
>     thunderstoneGameOver :: Bool
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
>         villageEffects :: [(Card,[VillageEffect])],
>         villageEffectsNumberOfBuys :: Int,
>         villageEffectsGold :: Int
>         }
>   | PurchasingCards {
>         purchasingNumberOfBuys :: Int,
>         purchasingGold :: Int
>         }
>   | LevelingUpHeroes

>   | UsingDungeonEffects
>   | AttackingMonster
>   | TakingSpoils

>   | Resting

>   | Waiting

>   | Discarding {
>         discardingCards :: Int,
>         discarding2CardsOr1Hero :: Int,
>         discardingHero :: Int
>         }
>   | WaitingForDiscards

>   | ChoosingOption PlayerOption
>         [((Int,String),Thunderstone [ThunderstoneEvent])]
>         (Maybe (Thunderstone ()))

> data PlayerOption =
>     WhichCardToDestroy
>   | WhichCardToDiscard
>   | WhichCardToBuy
>   | WhichHeroToLevelUp
>   | WhichEffectToActivate
>   deriving Show

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
>   | UseVillageEffect
>   | FinishUsingVillageEffects
>   | PurchaseCard
>   | FinishPurchasingCards
>   | LevelUpHero

>   | EnterDungeon
>   | EquipHero
>   | AttackMonster

>   | Rest
>   | DestroyCard

>   | EndTurn

>   | ChooseOption (Int,String)
>   | Backout

> data ThunderstoneEvent =
>     PlayerEvent PlayerId PlayerAction
>   | PlayerHand PlayerId [Card]
>   | PlayerPurchase PlayerId Card
>   | PlayerUpgrade PlayerId HeroCard HeroCard
>   | PlayerDiscard PlayerId Card
>   | PlayerDestroyCard PlayerId Card
>   | PlayerUseEffect PlayerId Card String
>   | GameOverEvent [(PlayerId,Int)]

Plus breach effects and other things that players should be notified of.


Game mechanics

> isGameOver :: Thunderstone Bool
> isGameOver = do
>     state <- getState
>     return (thunderstoneGameOver state)

> getThunderstonePlayerState :: PlayerId
>                            -> Thunderstone ThunderstonePlayerState
> getThunderstonePlayerState playerId = do
>     state <- getState
>     player <- getPlayer playerId
>     playerOptions <- getPlayerOptions playerId
>     return ThunderstonePlayerState {
>         playerStateHand = playerHand player,
>         playerStateDeck = length (playerDeck player),
>         playerStateDiscards = length (playerDiscard player),
>         playerStateXP = playerXP player,
>         playerStateOptions = playerOptions,
>         playerStateDungeonHall = take 3 (thunderstoneDungeon state),
>         playerStateHeroes = thunderstoneHeroes state,
>         playerStateVillage = thunderstoneVillage state
>         }

> getScores :: Thunderstone [(PlayerId,Int)]
> getScores = do
>     playerIds <- getPlayerIds
>     scores <- mapM getScore playerIds
>     return (zip playerIds scores)
>   where
>     getScore :: PlayerId -> Thunderstone Int
>     getScore playerId = do
>         player <- getPlayer playerId
>         return $ sum $ map cardScore
>                $ playerHand player ++ playerDeck player
>                                    ++ playerDiscard player

>     cardScore :: Card -> Int
>     cardScore (MonsterCard card) = cardVictoryPoints $ monsterDetails card
>     cardScore (HeroCard card) = cardVictoryPoints $ heroDetails card
>     cardScore (VillageCard card) = cardVictoryPoints $ villageDetails card
>     cardScore DiseaseCard = 0
>     cardScore (ThunderstoneCard card) =
>         cardVictoryPoints $ thunderstoneDetails card

> getPlayerOptions :: PlayerId -> Thunderstone [PlayerAction]
> getPlayerOptions playerId = do
>     gameOver <- isGameOver
>     if gameOver
>       then return []
>       else do
>         playerState <- getPlayerState playerId
>         options <- optionsWhen playerState
>         filterM legalAction options
>   where

>     legalAction playerAction = do
>         savedState <- getState
>         actionResult <- takeAction playerId playerAction
>         setState savedState
>         return (maybe False (const True) actionResult)

>     optionsWhen StartingTurn =
>         return [VisitVillage, EnterDungeon, Rest]

>     optionsWhen UsingVillageEffects {
>             villageEffects = villageHand
>             } = do
>         if villageEffectAvailable
>           then return [UseVillageEffect,FinishUsingVillageEffects,EndTurn]
>           else return [FinishUsingVillageEffects,EndTurn]
>       where
>         villageEffectAvailable =
>             not $ null $ concatMap snd villageHand

>     optionsWhen PurchasingCards {
>             purchasingNumberOfBuys = numberOfBuys,
>             purchasingGold = gold
>             } = do
>         forSale <- getForSaleInVillage
>         if numberOfBuys > 0 && gold >= minimum (map snd forSale)
>           then return [PurchaseCard,FinishPurchasingCards,EndTurn]
>           else return [FinishPurchasingCards,EndTurn]

>     optionsWhen LevelingUpHeroes = do
>         heroUpgrades <- getHeroUpgrades playerId
>         if null heroUpgrades
>           then return [EndTurn]
>           else return [LevelUpHero,EndTurn]

>     optionsWhen UsingDungeonEffects = undefined

>     optionsWhen AttackingMonster = undefined

>     optionsWhen TakingSpoils = undefined

>     optionsWhen Resting = return [DestroyCard,EndTurn]

>     optionsWhen Waiting = return []

>     optionsWhen Discarding {
>                     discardingCards = cards,
>                     discarding2CardsOr1Hero = cardsOrHero,
>                     discardingHero = hero
>                     } = do
>         hand <- getHand playerId
>         if null hand || (all (== 0) [cards, cardsOrHero, hero])
>             || (cards == 0 && all (not . isHero) hand)
>           then return [Backout]
>           else if cards == 0 && cardsOrHero == 0
>             then return (zipWith option [1..] (filter isHero hand))
>             else return (zipWith option [1..] hand)
>       where
>         option index card = ChooseOption (index,show card)

>     optionsWhen WaitingForDiscards = return []

>     optionsWhen (ChoosingOption _ options backout) = do
>         return $ maybe id (const (Backout:)) backout
>                $ map (ChooseOption . fst) options

> takeAction :: PlayerId -> PlayerAction
>            -> Thunderstone (Maybe [ThunderstoneEvent])
> takeAction playerId playerAction = do
>     gameOver <- isGameOver
>     if gameOver
>       then return Nothing
>       else do
>         playerState <- getPlayerState playerId
>         result <- performAction playerState playerAction
>         gameOver <- isGameOver
>         unless gameOver startNextTurnIfTurnFinished
>         return result

>   where

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
>         return (all isWaiting playerStates)
>       where
>         isWaiting Waiting = True
>         isWaiting _ = False

End your turn by discarding all cards face up on your discard pile,
and draw six new cards to form a new hand.

>     endTurn :: [ThunderstoneEvent]
>             -> Thunderstone (Maybe [ThunderstoneEvent])
>     endTurn events = do
>         hand <- getHand playerId
>         discard playerId hand
>         hand <- multiple 6 $ drawCard playerId
>         setHand playerId hand
>         setPlayerState playerId Waiting
>         return (Just events)

>     performAction :: PlayerState -> PlayerAction
>                   -> Thunderstone (Maybe [ThunderstoneEvent])

>     performAction StartingTurn VisitVillage = do
>         hand <- getHand playerId
>         setPlayerState playerId
>             UsingVillageEffects {
>                 villageEffects = map addVillageEffects hand,
>                 villageEffectsNumberOfBuys = 0,
>                 villageEffectsGold = 0
>                 }
>         return (Just [PlayerEvent playerId VisitVillage,
>                       PlayerHand playerId hand])
>       where
>         addVillageEffects card =
>             (card,cardVillageEffects $ cardProperties card)

>     performAction StartingTurn EnterDungeon = do
>         hand <- getHand playerId
>         undefined -- update player state
>         return (Just [PlayerEvent playerId EnterDungeon,
>                       PlayerHand playerId hand])

>     performAction StartingTurn Rest = do
>         setPlayerState playerId Resting
>         return (Just [PlayerEvent playerId Rest])

>     performAction StartingTurn _ = return Nothing

Village:

Village: Using Village Effects:

>     performAction playerState@UsingVillageEffects {
>             villageEffects = villageHand
>             } UseVillageEffect = do
>         setPlayerState playerId
>                        (ChoosingOption WhichEffectToActivate options backout)
>         return (Just [])
>       where
>         enumCards = zip [0..] villageHand
>         enumEffects = zip [1..] (concatMap mergeEffects enumCards)
>         mergeEffects (cardIndex,(card,villageEffects)) =
>             map (mergeEffect cardIndex card) villageEffects
>         mergeEffect cardIndex card villageEffect =
>             (cardIndex,card,villageEffect)
>         options = map makeOption enumEffects
>         backout = Just (setPlayerState playerId playerState)
>         makeOption
>             (index,(cardIndex,card,(effectName,effectAction))) =
>             ((index,show card ++ ": " ++ effectName),
>              performEffectAction cardIndex effectName effectAction)
>         performEffectAction cardIndex effectName effectAction = do
>             setPlayerState playerId playerState
>             result <- effectAction playerId playerState {
>                           villageEffects =
>                               removeEffect villageHand cardIndex effectName
>                           }
>                         cardIndex
>             case result of
>               Nothing -> do
>                 setPlayerState playerId playerState
>                 return []
>               Just events -> return events
>         removeEffect villageHand cardIndex effectName =
>             map (removeCardEffect cardIndex effectName)
>                 (zip [0..] villageHand)
>         removeCardEffect cardIndex effectName (index,(card,cardEffects))
>           | cardIndex == index =
>                 (card,filter (notNamed effectName) cardEffects)
>           | otherwise = (card,cardEffects)
>         notNamed effectName (name,_) = effectName /= name

>     performAction
>             UsingVillageEffects {
>                 villageEffectsNumberOfBuys = numberOfBuys,
>                 villageEffectsGold = gold
>                 }
>             FinishUsingVillageEffects = do
>         hand <- getHand playerId
>         let producedGold = sum (map (cardVillageGold . cardProperties) hand)
>         setPlayerState playerId
>             PurchasingCards {
>                 purchasingNumberOfBuys = numberOfBuys + 1,
>                 purchasingGold = gold + producedGold
>                 }
>         return (Just [])

>     performAction UsingVillageEffects {} EndTurn = do
>         endTurn []
>         return (Just [])

>     performAction UsingVillageEffects {} _ = return Nothing

Village: Purchasing cards:

>     performAction
>             playerState@PurchasingCards {
>                 purchasingNumberOfBuys = numberOfBuys,
>                 purchasingGold = gold
>                 }
>             PurchaseCard = do
>         forSale <- fmap (filter ((<= gold) . snd)) getForSaleInVillage
>         if null forSale || numberOfBuys <= 0
>           then return Nothing
>           else do
>             setPlayerState playerId
>                 (ChoosingOption WhichCardToBuy
>                      (zipWith purchaseOption [1..] forSale)
>                      (Just backout))
>             return (Just [])
>       where
>         purchaseOption index (card,_) = ((index,show card),doPurchase card)
>         doPurchase card = do
>             setPlayerState playerId playerState
>             result <- purchaseCard playerId card
>             return [PlayerPurchase playerId card]
>         backout = do
>             setPlayerState playerId playerState

>     performAction PurchasingCards {} FinishPurchasingCards = do
>         setPlayerState playerId LevelingUpHeroes
>         return (Just [])

>     performAction PurchasingCards {} EndTurn = do
>         endTurn []
>         return (Just [])

>     performAction PurchasingCards {} _ = return Nothing

Village: Leveling Up Heroes:

>     performAction LevelingUpHeroes LevelUpHero = do
>         xp <- getXP playerId
>         -- [(HeroCard,(Int,HeroCard))]
>         upgrades <- fmap (filter (canUpgrade xp)) (getHeroUpgrades playerId)
>         if null upgrades
>           then return Nothing
>           else do
>             setPlayerState playerId
>                 (ChoosingOption WhichHeroToLevelUp
>                      (zipWith upgradeOption [1..] upgrades)
>                      (Just backout))
>             return (Just [])
>       where
>         canUpgrade xp (_,(price,_)) = xp >= price
>         upgradeOption index (oldHero,(price,newHero)) =
>             ((index,showUpgrade oldHero newHero price),
>              doUpgrade oldHero newHero)
>         showUpgrade oldHero newHero price =
>             show (HeroCard oldHero) ++ " -> " ++ show (HeroCard newHero)
>                                     ++ " (" ++ show price ++ "XP)"
>         doUpgrade oldHero newHero = do
>             result <- upgradeHero playerId oldHero newHero
>             setPlayerState playerId LevelingUpHeroes
>             if result
>               then return [PlayerUpgrade playerId oldHero newHero]
>               else return []
>         backout = do
>             setPlayerState playerId LevelingUpHeroes

>     performAction LevelingUpHeroes EndTurn = do
>         endTurn []
>         return (Just [])

>     performAction LevelingUpHeroes _ = return Nothing

Dungeon:

Rest:

>     performAction Resting DestroyCard = do
>         hand <- getHand playerId
>         if null hand
>           then
>             endTurn []
>           else do
>             setPlayerState playerId
>                 (ChoosingOption WhichCardToDestroy
>                      (zipWith chooseCard [1..] hand)
>                      (Just backout))
>             return (Just [])
>       where
>         chooseCard optionNumber card =
>             ((optionNumber,show card),destroyCard card)
>         destroyCard card = do
>             hand <- getHand playerId
>             setHand playerId (remove1 card hand)
>             endTurn []
>             return [PlayerEvent playerId DestroyCard]
>         backout =
>             setPlayerState playerId Resting

>     performAction Resting EndTurn = do
>         endTurn []

>     performAction Resting _ = return Nothing

Discarding as nonactive player:

>     performAction playerState@Discarding {
>                     discardingCards = cards,
>                     discarding2CardsOr1Hero = cardsOrHero,
>                     discardingHero = hero
>                     } (ChooseOption (index,description)) = do
>         hand <- getHand playerId
>         let (discards,holds) = partition ((== index) . fst) (zip [1..] hand)
>         if null discards || description /= show (snd $ head discards)
>           then return Nothing
>           else do
>             let (_,card) = head discards
>             let newHand = map snd holds
>             if null newHand
>               then do
>                 setPlayerState playerId Waiting
>                 setHand playerId newHand
>                 return (Just [PlayerDiscard playerId card])
>               else if not (isHero card) && cards == 0 && cardsOrHero == 0
>                 then return Nothing
>                 else do
>                   if isHero card
>                     then setPlayerState playerId discardHero
>                     else setPlayerState playerId discardNonhero
>                   setHand playerId newHand
>                   return (Just [PlayerDiscard playerId card])
>       where
>         discardHero
>           | cards + cardsOrHero + hero <= 1 = Waiting
>           | hero > 0 = playerState { discardingHero = hero - 1 }
>           | cardsOrHero > 0 =
>                 playerState { discarding2CardsOr1Hero = cardsOrHero - 1 }
>           | otherwise = playerState { discardingCards = cards - 1 }
>         discardNonhero
>           | hero == 0 && cardsOrHero == 0 && cards <= 1 = Waiting
>           | cards > 0 = playerState { discardingCards = cards - 1 }
>           | otherwise = playerState {
>                             discardingCards = 1,
>                             discarding2CardsOr1Hero = cardsOrHero - 1
>                             }

>     performAction Discarding {
>                     discardingCards = cards,
>                     discarding2CardsOr1Hero = cardsOrHero,
>                     discardingHero = hero
>                     } Backout = do
>         hand <- getHand playerId
>         if null hand || (all (== 0) [cards, cardsOrHero, hero])
>             || (cards == 0 && all (not . isHero) hand)
>           then do
>             setPlayerState playerId Waiting
>             return (Just [])
>           else
>             return Nothing

>     performAction Discarding {} _ = return Nothing

Generic choose option:

>     performAction (ChoosingOption _ options _) (ChooseOption option) = do
>         maybe (return Nothing) (fmap Just) $ lookup option options

>     performAction (ChoosingOption _ _ (Just backout)) Backout = do
>         backout
>         return (Just [])

>     performAction (ChoosingOption _ _ _) _ = return Nothing

Low level game mechanics

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

> getForSaleInVillage :: Thunderstone [(Card,Int)]
> getForSaleInVillage = do
>     state <- getState
>     let village = map fst (filter ((> 0) . snd) (thunderstoneVillage state))
>     let heroes = concatMap (take 1 . snd) (thunderstoneHeroes state)
>     return (map addPrice $ map VillageCard village ++ map HeroCard heroes)
>   where
>     addPrice card = (card,cardVillagePrice $ cardProperties card)

> purchaseCard :: PlayerId -> Card -> Thunderstone Bool
> purchaseCard playerId card = do
>     playerState <- getPlayerState playerId
>     doPurchase playerState
>   where
>     cardPrice = cardVillagePrice $ cardProperties card
>     doPurchase playerState@PurchasingCards {
>             purchasingNumberOfBuys = numberOfBuys,
>             purchasingGold = gold
>             }
>       | numberOfBuys <= 0 || gold < cardPrice = return False
>       | otherwise =
>           case card of
>             VillageCard villageCard -> do
>                 result <- drawResource villageCard
>                 maybe (return False) (const completePurchase) result
>             HeroCard heroCard -> do
>                 result <- drawHero (cardType $ heroDetails heroCard)
>                 maybe (return False) (const completePurchase) result
>             _ -> return False
>       where
>         completePurchase = do
>             setPlayerState playerId playerState {
>                 purchasingNumberOfBuys = numberOfBuys - 1,
>                 purchasingGold = gold - cardPrice
>                 }
>             discard playerId [card]
>             return True
>     doPurchase _ = return False

> getHeroUpgrades :: PlayerId -> Thunderstone [(HeroCard,(Int,HeroCard))]
> getHeroUpgrades playerId = do
>     state <- getState
>     player <- getPlayer playerId
>     let heroes = nub $ concatMap heroUpgrades $ concatMap onlyHeroes
>                                               $ playerHand player
>     let xp = playerXP player
>     let availableHeroes = concatMap snd $ thunderstoneHeroes state
>     return (filter (upgradeAvailable xp availableHeroes) heroes)
>   where
>     onlyHeroes :: Card -> [HeroCard]
>     onlyHeroes (HeroCard hero) = [hero]
>     onlyHeroes _ = []

>     heroUpgrades :: HeroCard -> [(HeroCard,(Int,HeroCard))]
>     heroUpgrades card =
>         map (\ newHero -> (card,(price,newHero))) upgradeTo
>       where
>         (price,upgradeTo) = heroUpgrade $ cardStats $ heroDetails card

>     upgradeAvailable xp availableHeroes (_,(price,hero)) =
>         xp >= price && hero `elem` availableHeroes

> upgradeHero :: PlayerId -> HeroCard -> HeroCard -> Thunderstone Bool
> upgradeHero playerId oldHero newHero = do
>     xp <- getXP playerId
>     hand <- getHand playerId
>     let (upgradePrice,upgradeHeroes) =
>             heroUpgrade $ cardStats $ heroDetails oldHero
>     if not (HeroCard oldHero `elem` hand)
>         || xp < upgradePrice
>         || not (newHero `elem` upgradeHeroes)
>       then
>         return False
>       else do
>         heroUpgrade <- drawHeroUpgrade newHero
>         maybe (return False) (const $ do
>                   setXP playerId (xp - upgradePrice)
>                   setHand playerId (remove1 (HeroCard oldHero) hand)
>                   discard playerId [HeroCard newHero]
>                   return True)
>               heroUpgrade

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

> getCurrentPlayerId :: Thunderstone PlayerId
> getCurrentPlayerId = do
>     state <- getState
>     return (PlayerId (thunderstoneCurrentPlayer state))

> getPlayerIds :: Thunderstone [PlayerId]
> getPlayerIds = do
>     state <- getState
>     return (map PlayerId [0..length (thunderstonePlayers state) - 1])

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

> getXP :: PlayerId -> Thunderstone Int
> getXP playerId = fmap playerXP (getPlayer playerId)

> setXP :: PlayerId -> Int -> Thunderstone ()
> setXP playerId xp = do
>     player <- getPlayer playerId
>     setPlayer playerId player { playerXP = xp }

> getPlayerState :: PlayerId -> Thunderstone PlayerState
> getPlayerState playerId = fmap playerState (getPlayer playerId)

> setPlayerState:: PlayerId -> PlayerState -> Thunderstone ()
> setPlayerState playerId state = do
>      player <- getPlayer playerId
>      setPlayer playerId player { playerState = state }

> getVillage :: Thunderstone [(VillageCard,Int)]
> getVillage = do
>     state <- getState
>     return (thunderstoneVillage state)

More briefly: fmap thunderstoneVillage getState

> getResourceCount :: VillageCard -> Thunderstone Int
> getResourceCount card = do
>     resources <- getVillage
>     return (maybe 0 id (lookup card resources))

More briefly: fmap (maybe 0 id . lookup card) getResources

> setResourceCount :: VillageCard -> Int -> Thunderstone ()
> setResourceCount card count = do
>     state <- getState
>     let village = thunderstoneVillage state
>     setState state {
>         thunderstoneVillage = map setCount (thunderstoneVillage state)
>         }
>   where
>     setCount resource@(resourceCard,_)
>       | resourceCard == card = (resourceCard,count)
>       | otherwise = resource

> drawResource :: VillageCard -> Thunderstone (Maybe VillageCard)
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

> drawHeroUpgrade :: HeroCard -> Thunderstone (Maybe HeroCard)
> drawHeroUpgrade heroCard = do
>     state <- getState
>     let stack = lookup heroType (thunderstoneHeroes state)
>     if heroCard `containedIn` stack
>       then do
>         setState state {
>             thunderstoneHeroes = map updateHeroes (thunderstoneHeroes state)
>             }
>         return (Just heroCard)
>       else return Nothing
>   where
>     heroType = cardType $ heroDetails heroCard
>     containedIn card Nothing = False
>     containedIn card (Just stack) = card `elem` stack
>     updateHeroes heroes@(hero,stack)
>       | hero == heroType = (hero,remove1 heroCard stack)
>       | otherwise = heroes

> discardIndex :: PlayerId -> Int -> Thunderstone ()
> discardIndex playerId index = do
>     hand <- getHand playerId
>     discard playerId [hand !! index]
>     destroyIndex playerId index

> destroyIndex :: PlayerId -> Int -> Thunderstone ()
> destroyIndex playerId index = do
>     hand <- getHand playerId
>     setHand playerId (removeIndex index hand)
>     playerState <- getPlayerState playerId
>     setPlayerState playerId (destroyIndexInState playerState)
>   where
>     destroyIndexInState playerState@UsingVillageEffects {
>             villageEffects = hand
>             } =
>         playerState { villageEffects = removeIndex index hand }
>     destroyIndexInState playerState@UsingDungeonEffects {} =
>         undefined
>     destroyIndexInState playerState = playerState

> multiple :: (Functor m, Monad m) => Int -> m (Maybe a) -> m [a]
> multiple count action = fmap catMaybes (replicateM count action)

> removeIndex :: Int -> [a] -> [a]
> removeIndex index list = take index list ++ drop (index+1) list

> remove1 :: Eq a => a -> [a] -> [a]
> remove1 item items = before ++ drop 1 after
>   where
>     (before,after) = span (/= item) items

> thunderstoneGetState :: ThunderstoneState -> Thunderstone a -> a
> thunderstoneGetState state getThunderstoneState =
>     snd $ runStateTransformer getThunderstoneState state

=============================================================================

Card properties

> isHero :: Card -> Bool
> isHero (HeroCard _) = True
> isHero _ = False

> hasClass :: Card -> CardClass -> Bool
> hasClass card cardClass = undefined

> data CardProperties = CardProperties {
>     cardDungeonEffects :: [DungeonEffect],
>     cardBattleEffects :: [BattleEffect],
>     cardSpoilsEffects :: [SpoilsEffect],
>     cardBattleResult :: BattleResult,
>     cardBreachEffect :: BreachEffect,
>     cardVillageGold :: Int,
>     cardVillagePrice :: Int,
>     cardVillageEffects :: [VillageEffect]
>     }

> type DungeonEffect = (String,PlayerId -> Thunderstone ())
> type BattleEffect = (String,PlayerId -> Thunderstone ())
> type SpoilsEffect = (String,PlayerId -> Thunderstone ())

Village Effect:
(String: text of the effect, perform the effect with the PlayerId: of
 the player where the PlayerState: is the players state with the
 effect removed and the Int: is the index of the card in the player's
 hand.  REPEAT effects will not set the player state.)

> type VillageEffect =
>     (String,PlayerId -> PlayerState -> Int
>                      -> Thunderstone (Maybe [ThunderstoneEvent]))
> type BattleResult = (String,PlayerId -> Thunderstone Bool)
> type BreachEffect = (String,Thunderstone ())

> class HasCardProperties card where
>     cardProperties :: card -> CardProperties

> instance HasCardProperties Card where
>     cardProperties (MonsterCard card) = cardProperties card
>     cardProperties (HeroCard card) = cardProperties card
>     cardProperties (VillageCard card) = cardProperties card
>     cardProperties DiseaseCard = CardProperties {
>         cardDungeonEffects = [],
>         cardBattleEffects = [],
>         cardSpoilsEffects = [],
>         cardBattleResult = undefined,
>         cardBreachEffect = undefined,
>         cardVillageGold = 0,
>         cardVillagePrice = undefined,
>         cardVillageEffects = []
>         }
>     cardProperties (ThunderstoneCard card) = cardProperties card

> instance HasCardProperties ThunderstoneCard where
>     cardProperties _ = undefined

> heroCardProperties :: HeroCard -> [DungeonEffect] -> [BattleEffect]
>                                -> [SpoilsEffect]
>                    -> CardProperties
> heroCardProperties heroCard dungeonEffects battleEffects spoilsEffects =
>     CardProperties {
>         cardDungeonEffects = dungeonEffects,
>         cardBattleEffects = battleEffects,
>         cardSpoilsEffects = spoilsEffects,
>         cardBattleResult = undefined,
>         cardBreachEffect = undefined,
>         cardVillageGold = cardGold $ heroDetails heroCard,
>         cardVillagePrice = heroPrice $ cardStats $ heroDetails heroCard,
>         cardVillageEffects = []
>         }

> instance HasCardProperties HeroCard where

>     cardProperties AmazonArcher = heroCardProperties AmazonArcher
>         [] [amazon] []
>       where
>         amazon =
>             ("ATTACK +1.\nAdditional ATTACK +2 at Rank 2 or 3.",
>              undefined)
> -- Amazon: This Hero's Dungeon Effect is an Attack that is in addition
> -- to the Amazon's normal Attack.

>     cardProperties AmazonHuntress = heroCardProperties AmazonHuntress
>         [] [amazon] []
>       where
>         amazon =
>             ("ATTACK +2.\nAdditional ATTACK +3 at Rank 2 or 3.",
>              undefined)
> -- Amazon: This Hero's Dungeon Effect is an Attack that is in addition
> -- to the Amazon's normal Attack.

>     cardProperties AmazonQueen = heroCardProperties AmazonQueen
>         [] [amazon] []
>       where
>         amazon =
>             ("ATTACK +2.\nAdditional ATTACK +4 at Rank 2 or 3.",
>              undefined)
> -- Amazon: This Hero's Dungeon Effect is an Attack that is in addition
> -- to the Amazon's normal Attack.

>     cardProperties ChaliceQuester = heroCardProperties ChaliceQuester
>         [destroyDisease] [chalice] []
>       where
>         destroyDisease =
>             ("REPEAT DUNGEON: Destroy one Disease to draw one card.",
>              undefined)
>         chalice =
>             ("ATTACK +2",
>              undefined)
> -- Chalice Quester and Defender: You man continue to destroy Disease
> -- cards and draw new cards until you choose which Monster to attack.

>     cardProperties ChaliceDefender = heroCardProperties ChaliceDefender
>         [destroyDisease,drawCard,attackPlus1] [chalice] []
>       where
>         destroyDisease =
>             ("REPEAT DUNGEON: Destroy one Disease to draw one card.",
>              undefined)
>         drawCard =
>             ("DUNGEON: Draw one card.",
>              undefined)
>         attackPlus1 =
>             ("DUNGEON: ATTACK +1 for each Item that produces Light.",
>              undefined)
>         chalice =
>             ("ATTACK +3",
>              undefined)
> -- Chalice Quester and Defender: You man continue to destroy Disease
> -- cards and draw new cards until you choose which Monster to attack.

>     cardProperties ChalicePaladin = heroCardProperties ChalicePaladin
>         [drawCard] [chalice] [spoils]
>       where
>         drawCard =
>             ("DUNGEON: Draw one card.",
>              undefined)
>         chalice =
>             ("ATTACK +4",
>              undefined)
>         spoils =
>             ("Spoils (Village).",
>              undefined)
> -- Chalic Paladin: You may purchase any one Village card (including
> -- Basic and Hero cards) from the Village after a victorious battle,
> -- using the gold in your hand.

>     cardProperties DwarfGuardian = heroCardProperties DwarfGuardian
>         [] [dwarf] []
>       where
>         dwarf =
>             ("ATTACK +1\nAdditional ATTACK +3 when equipped with an "
>              ++ "Edged Weapon.",
>              undefined)
> -- Dwarf Guardian: His total Attack Value if an Edged Weapon is equipped
> -- is +4.  This bonus is part of the Dwarf's ability which he retains even
> -- if the Weapon later becomes useless (due to a Monster's Battle Effect,
> -- for instance).

>     cardProperties DwarfJanissary = heroCardProperties DwarfJanissary
>         [] [dwarf] [spoils]
>       where
>         dwarf =
>             ("ATTACK +2\nAdditional ATTACK +4 when equipped with an "
>              ++ "Edged Weapon.",
>              undefined)
>         spoils =
>             ("Spoils (Weapon).",
>              undefined)
> -- Dwarf Janissary: If revealed during a Dungeon action, you may purchase
> -- one Weapon card from the Village after a victorious battle, using the
> -- gold in your hand.  His total Attack Vlaue if an Edged Weapon is
> -- equipped is +6.

>     cardProperties DwarfSentinel = heroCardProperties DwarfSentinel
>         [] [dwarf] []
>       where
>         dwarf =
>             ("ATTACK +3\nAdditional ATTACK +45 when equipped with an "
>              ++ "Edged Weapon.",
>              undefined)
> -- Dwarf Sentinel: His total Attack Value with an Edged Weapon equipped
> -- is +8.

>     cardProperties ElfWizard = heroCardProperties ElfWizard
>         [] [elf] []
>       where
>         elf =
>             ("MAGIC ATTACK +2",
>              undefined)

>     cardProperties ElfSorcerer = heroCardProperties ElfSorcerer
>         [] [elf] [spoils]
>       where
>         elf =
>             ("MAGIC ATTACK +3",
>              undefined)
>         spoils =
>             ("You may return one Monster to the bottom of the deck "
>              ++ "after defeating a monster.  (Refill the hall.)",
>              undefined)
> -- Elf Sorcerer/Archmage: When a Monster is returned to the bottom of
> -- the monster deck, refill the Dungeon Hall.  If this results in a
> -- Breach effect, resolve it immediately.  If the Thunderstone moves
> -- to Rank 1 of the Dungeon Hall, the game ends immediately; you do
> -- not collect the Thunderstone.

>     cardProperties ElfArchmage = heroCardProperties ElfArchmage
>         [dungeon] [elf] []
>       where
>         dungeon =
>             ("DUNGEON: You may return one Monster to the bottom of "
>              ++ "the deck and refill the hall before the beginning "
>              ++ "of a battle.",
>              undefined)
>         elf =
>             ("MAGIC ATTACK +3",
>              undefined)
> -- Elf Sorcerer/Archmage: When a Monster is returned to the bottom of
> -- the monster deck, refill the Dungeon Hall.  If this results in a
> -- Breach effect, resolve it immediately.  If the Thunderstone moves
> -- to Rank 1 of the Dungeon Hall, the game ends immediately; you do
> -- not collect the Thunderstone.

>     cardProperties FeaynArcher = heroCardProperties FeaynArcher
>         [] [feayn] []
>       where
>         feayn =
>             ("Cannot attack Rank 1.\nATTACK +2",
>              undefined)
> -- Feayn: If a Dungeon Actions causes you to attack a Monster in
> -- Rank 1, do not add the Feayn's Attack bonus to your Attack Value.
> -- If Feayn does not attack, his Light bonus is lost.

>     cardProperties FeaynMarksman = heroCardProperties FeaynMarksman
>         [] [feayn] []
>       where
>         feayn =
>             ("Cannot attack Rank 1.\nATTACK +3",
>              undefined)
> -- Feayn: If a Dungeon Actions causes you to attack a Monster in
> -- Rank 1, do not add the Feayn's Attack bonus to your Attack Value.
> -- If Feayn does not attack, his Light bonus is lost.

>     cardProperties FeaynSniper = heroCardProperties FeaynSniper
>         [] [feayn] [spoils]
>       where
>         feayn =
>             ("Cannot attack Rank 1.\nATTACK +4",
>              undefined)
>         spoils =
>             ("Gain +1 XP if you defeat a Monster in Rank 3.",
>              undefined)
> -- Feayn: If a Dungeon Actions causes you to attack a Monster in
> -- Rank 1, do not add the Feayn's Attack bonus to your Attack Value.
> -- If Feayn does not attack, his Light bonus is lost.

>     cardProperties LoriggThief = heroCardProperties LoriggThief
>         [] [lorigg] []
>       where
>         lorigg =
>             ("ATTACK +1",
>              undefined)

>     cardProperties LoriggRogue = heroCardProperties LoriggRogue
>         [dungeon] [lorigg] []
>       where
>         dungeon =
>             ("DUNGEON: All other players discard one card.",
>              undefined)
>         lorigg =
>             ("ATTACK +2",
>              undefined)
> -- Lorigg Outlaw or Rogue: Regardless of whether the battle is victorious
> -- or not, all other players must discard cards when this Hero enters the
> -- Dungeon.

>     cardProperties LoriggOutlaw = heroCardProperties LoriggOutlaw
>         [dungeon] [lorigg] []
>       where
>         dungeon =
>             ("DUNGEON: All other players discard one card.",
>              undefined)
>         lorigg =
>             ("ATTACK +2",
>              undefined)
> -- Lorigg Outlaw or Rogue: Regardless of whether the battle is victorious
> -- or not, all other players must discard cards when this Hero enters the
> -- Dungeon.

>     cardProperties OutlandsWarrior = heroCardProperties OutlandsWarrior
>         [dungeon] [outlands] []
>       where
>         dungeon =
>             ("DUNGEON: Destroy one Food for an additional ATTACK +3.",
>              undefined)
>         outlands =
>             ("ATTACK +3",
>              undefined)

>     cardProperties OutlandsSlayer = heroCardProperties OutlandsSlayer
>         [dungeonMonster,dungeonFood] [outlands] []
>       where
>         dungeonMonster =
>             ("DUNGEON: Gain +1 ATTACK for each Monster card revealed "
>              ++ "from your hand.",
>              undefined)
>         dungeonFood =
>             ("REPEAT DUNGEON: Destroy one Food for an additional ATTACK +3.",
>              undefined)
>         outlands =
>             ("ATTACK +5",
>              undefined)
> -- Outlands Slayer or Khan: The Hero gains an Attack bonus for each
> -- Monster card revealed in your hand before the battle.

>     cardProperties OutlandsKhan = heroCardProperties OutlandsKhan
>         [dungeon] [outlands] []
>       where
>         dungeon =
>             ("DUNGEON: ATTACK +2 for each Monster card revealed "
>              ++ "from your hand.",
>              undefined)
>         outlands =
>             ("ATTACK +7",
>              undefined)
> -- Outlands Slayer or Khan: The Hero gains an Attack bonus for each
> -- Monster card revealed in your hand before the battle.

>     cardProperties RedbladeKiller = heroCardProperties RedbladeKiller
>         [] [redblade] []
>       where
>         redblade =
>             ("ATTACK +2",
>              undefined)

>     cardProperties RedbladePoisoner = heroCardProperties RedbladePoisoner
>         [dungeon] [redblade] []
>       where
>         dungeon =
>             ("DUNGEON: All other players discard one card.",
>              undefined)
>         redblade =
>             ("ATTACK +3",
>              undefined)
> -- Redblade Assassin or Poisoner: Regardless of whether the battle is
> -- victorious or not, all other players must discard cards when this Hero
> -- enters the Dungeon.

>     cardProperties RedbladeAssassin = heroCardProperties RedbladeAssassin
>         [dungeon] [redblade] []
>       where
>         dungeon =
>             ("DUNGEON: All other players discard one Hero or two cards.",
>              undefined)
>         redblade =
>             ("ATTACK +4",
>              undefined)
> -- Redblade Assassin or Poisoner: Regardless of whether the battle is
> -- victorious or not, all other players must discard cards when this Hero
> -- enters the Dungeon.

>     cardProperties RegianCleric = heroCardProperties RegianCleric
>         [dungeon] [regian] []
>       where
>         dungeon =
>             ("REPEAT DUNGEON: Destroy one Disease to draw one card.",
>              undefined)
>         regian =
>             ("MAGIC ATTACK +1",
>              undefined)
> -- Regian: You may continue to destroy Disease cards and draw new cards
> -- until the battle begins.

>     cardProperties RegianPriest = heroCardProperties RegianPriest
>         [dungeonCard,dungeonDisease] [regian] []
>       where
>         dungeonCard =
>             ("DUNGEON: Draw one card.",
>              undefined)
>         dungeonDisease =
>             ("REPEAT DUNGEON: Destroy one Disease to draw one card.",
>              undefined)
>         regian =
>             ("MAGIC ATTACK +2",
>              undefined)
> -- Regian: You may continue to destroy Disease cards and draw new cards
> -- until the battle begins.

>     cardProperties RegianBishop = heroCardProperties RegianBishop
>         [dungeonCard,dungeonDisease] [regian] []
>       where
>         dungeonCard =
>             ("DUNGEON: Draw two cards.",
>              undefined)
>         dungeonDisease =
>             ("REPEAT DUNGEON: Destroy one Disease to draw one card.",
>              undefined)
>         regian =
>             ("MAGIC ATTACK +3",
>              undefined)
> -- Regian: You may continue to destroy Disease cards and draw new cards
> -- until the battle begins.

>     cardProperties SelurinMagician = heroCardProperties SelurinMagician
>         [] [selurin] []
>       where
>         selurin =
>             ("MAGIC ATTACK +2\nAll Items and Magic Attack Spells gain "
>              ++ "MAGIC ATTACK +1.",
>              undefined)
> -- Selurin: Each Spell with a Magic Attack bonus gains a Magic Attack bonus
> -- of +1.  Each Item (with the Item keyword), regardless of whether it has
> -- an Attack bonus or not, gains a Magic Attack bonus of +1.

>     cardProperties SelurinWarlock = heroCardProperties SelurinWarlock
>         [] [selurin] []
>       where
>         selurin =
>             ("MAGIC ATTACK +2\nTotal MAGIC ATTACK x2* (apply last)",
>              undefined)
> -- Selurin Theurge or Warlock: The x2 multiplier of the Selurin Wizard
> -- affects only Magic Attack bonuses, and is applied after all Magic Attack
> -- bonuses have been calculated.  Multiple Wizards multiply together
> -- (two become x4, three become x8, etc.).

>     cardProperties SelurinTheurge = heroCardProperties SelurinTheurge
>         [dungeon] [selurin] []
>       where
>         dungeon =
>             ("DUNGEON: Each player discards one Hero or shows they "
>              ++ "have none.  You may borrow one of those discarded "
>              ++ "Heroes for the battle, returning it at the end.",
>              undefined)
>         selurin =
>             ("MAGIC ATTACK +2\nTotal MAGIC ATTACK x2* (apply last)",
>              undefined)
> -- Selurin Theurge or Warlock: The x2 multiplier of the Selurin Wizard
> -- affects only Magic Attack bonuses, and is applied after all Magic Attack
> -- bonuses have been calculated.  Multiple Wizards multiply together
> -- (two become x4, three become x8, etc.).
> -- Selurin Theurge: If the borrowed Hero is destroyed by a Battle Effect,
> -- it is not returned to the original owner.  Instead, destroy the card.

>     cardProperties ThyrianSquire = heroCardProperties ThyrianSquire
>         [dungeon] [thyrian] []
>       where
>         dungeon =
>             ("DUNGEON: Destroy one Food for additional ATTACK +2.",
>              undefined)
>         thyrian =
>             ("ATTACK +2",
>              undefined)
> -- Thyrian: Food destroyed by this Dungeon Effect cannot also be used to
> -- gain a Strength bonus or for any other effect.

>     cardProperties ThyrianKnight = heroCardProperties ThyrianKnight
>         [dungeon] [thyrian] []
>       where
>         dungeon =
>             ("DUNGEON: Destroy one Food for additional ATTACK +2.",
>              undefined)
>         thyrian =
>             ("ATTACK +4\nAll Militia gain ATTACK +1.",
>              undefined)
> -- Thyrian: Food destroyed by this Dungeon Effect cannot also be used to
> -- gain a Strength bonus or for any other effect.

>     cardProperties ThyrianLord = heroCardProperties ThyrianLord
>         [dungeon] [thyrian] []
>       where
>         dungeon =
>             ("DUNGEON: Destroy one Food to place one Monster from "
>              ++ "the hall worth 1 or 2 VP into your discard pile.  "
>              ++ "Refill the hall.",
>              undefined)
>         thyrian =
>             ("ATTACK +4\nAll Heroes other than Fighters gain ATTACK +2.",
>              undefined)
> -- Thyrian: Food destroyed by this Dungeon Effect cannot also be used to
> -- gain a Strength bonus or for any other effect.
> -- Thyrian Lord: You may only select a Monster iwth 1 or 2 VP, and not
> -- 0 VP.  When a Monster is placed in your discard pile, refill the
> -- Dungeon Hall.  If this results in a Breach effect, resolve it
> -- immediately.  If the Thunderstone moves to Rank 1 of the Dungeon
> -- Hall, the game ends immediately; you do not collect the Thunderstone.
> -- You do not earn any Experience Points for the Effect.

>     cardProperties _ = undefined

> monsterCardProperties :: MonsterCard -> BattleResult -> BreachEffect
>                       -> CardProperties
> monsterCardProperties monsterCard battleResult breachEffect =
>     CardProperties {
>         cardDungeonEffects = [],
>         cardBattleEffects = [],
>         cardSpoilsEffects = [],
>         cardBattleResult = battleResult,
>         cardBreachEffect = breachEffect,
>         cardVillageGold = 0,
>         cardVillagePrice = undefined,
>         cardVillageEffects = []
>         }

> instance HasCardProperties MonsterCard where

>     cardProperties ArchdukeOfPain = monsterCardProperties ArchdukeOfPain
>         archduke breach
>       where
>         archduke =
>             ("Magic Attack Required\nBATTLE: Destroy all Clerics and "
>              ++ "Wizards.",
>              undefined)
>         breach =
>             ("BREACH: Destroy the top two cards from each Hero deck "
>              ++ "in the Village.",
>              undefined)
> -- Archduke of Pain: You must have a Magic Attack of at least +1 in order
> -- to defeat the Archduke of Pain.  You may still choose to attack the
> -- Archduke, even without Magic Attack present.  If there are no Cleric
> -- and/or Wizard cards in the battle, there is no effect.  When the
> -- Archduke reaches Rank 1 of he Dungeon Hall, destroy the top two cards
> -- from each Hero stack in the Village, including Militia.

>     cardProperties _ = undefined

> villageCardProperties :: VillageCard -> [DungeonEffect] -> [BattleEffect]
>                                      -> [VillageEffect]
>                       -> CardProperties
> villageCardProperties villageCard dungeonEffects battleEffects
>                       villageEffects =
>     CardProperties {
>         cardDungeonEffects = dungeonEffects,
>         cardBattleEffects = battleEffects,
>         cardSpoilsEffects = [],
>         cardBattleResult = undefined,
>         cardBreachEffect = undefined,
>         cardVillageGold = cardGold $ villageDetails villageCard,
>         cardVillagePrice =
>             villagePrice $ cardStats $ villageDetails villageCard,
>         cardVillageEffects = villageEffects
>         }

> instance HasCardProperties VillageCard where

>     cardProperties ArcaneEnergies = villageCardProperties ArcaneEnergies
>         [allAttacksBecomeMagic] [] []
>       where
>         allAttacksBecomeMagic =
>             ("DUNGEON: All ATTACKS from Heroes with Weapons equipped "
>              ++ "become MAGIC ATTACKS.  Draw one card.",
>              undefined)
> -- Arcane Energies: You must draw a card when you use this dungeon
> -- ability.

>     cardProperties Banish = villageCardProperties Banish
>         [banish] [] []
>       where
>         banish =
>             ("DUNGEON: Return one Monster to the bottom of the deck and "
>              ++ "refill the hall, or rearrange the hall.  Destroy one "
>              ++ "card from your hand.  Draw one card.",
>              undefined)
> -- Banish: You must declare you are entering the Dungeon to play
> -- Banish, but do not choose which Monster to attack until after the
> -- Hall is refilled.  If Banish results in a Breach (or Trap) Effect,
> -- resolve it immediately.  You may rearrange the hall so as to place
> -- the Thunderstone in Rank 1 of the Dungeon Hall, ending the game
> -- immediately without collecting the Thunderstone.  Multiple Banish
> -- cards can be used before choosing which Monster to attack, but each
> -- must be completely resolved before the next can be played.  You
> -- must draw a card when using this Dungeon Ability.

>     cardProperties Barkeep = villageCardProperties Barkeep
>         [] [] [purchaseAdditional,destroyForGold]
>       where
>         purchaseAdditional =
>             ("VILLAGE: You may purchase one additional card this turn.",
>              doPurchaseAdditional)
>         destroyForGold =
>             ("VILLAGE: Destroy this card to gain 2 Gold.",
>              doDestroyForGold)
> -- Barkeep: Each additional Barkeep allows you to purchase one 
> -- additional card.  You do not gain the gold value of the Barkeep when
> -- it is destroyed.
>         doPurchaseAdditional playerId playerState _ = do
>             setPlayerState playerId playerState {
>                 villageEffectsNumberOfBuys =
>                     villageEffectsNumberOfBuys playerState + 1
>                 }
>             let (text,_) = purchaseAdditional
>             return (Just [PlayerUseEffect playerId
>                                           (VillageCard Barkeep) text])
>         doDestroyForGold playerId playerState cardIndex = do
>             destroyIndex playerId cardIndex
>             setPlayerState playerId playerState {
>                 villageEffectsGold = villageEffectsGold playerState + 2
>                 }
>             let (text,_) = destroyForGold
>             return (Just [PlayerUseEffect playerId
>                                           (VillageCard Barkeep) text])

>     cardProperties BattleFury = villageCardProperties BattleFury
>         [battleFury] [] []
>       where
>         battleFury =
>             ("DUNGEON: All Heroes gain ATTACK +1.",
>              undefined)
> -- Battle Fury: Militia are Heroes, and gain the Attack bonus from this
> -- spell.

>     cardProperties Feast = villageCardProperties Feast
>         [feast] [] []
>       where
>         feast =
>             ("DUNGEON: All Heroes gain Strength +1 and ATTACK +1.",
>              undefined)
> -- Feast: Militia cards are Heroes, so they gain the Attack and Strength
> -- bonuses from this card.

>     cardProperties Fireball = villageCardProperties Fireball
>         [] [fireball] []
>       where
>         fireball =
>             ("MAGIC ATTACK +3",
>              undefined)
> -- Fireball: You do not need Heroes present to use this Spell.

>     cardProperties FlamingSword = villageCardProperties FlamingSword
>         [] [flamingSword] []
>       where
>         flamingSword =
>             ("MAGIC ATTACK +3",
>              undefined)
> -- Flaming Sword: You only gain the Light bonus if the Flaming Sword
> -- is equipped to a Hero.

>     cardProperties Goodberries = villageCardProperties Goodberries
>         [goodberries] [] []
>       where
>         goodberries =
>             ("DUNGEON: One Hero gains Strength +3 and ATTACK becomes "
>              ++ "MAGIC ATTACK for that Hero.",
>              undefined)
> -- Goodberries: After the final Attack bonus of the Hero is calculated,
> -- its entire bonus becomes Magic Attack.  Militia are Heroes, and may
> -- benefit from this card.

>     cardProperties Hatchet = villageCardProperties Hatchet
>         [] [hatchet] []
>       where
>         hatchet =
>             ("ATTACK +3",
>              undefined)
> -- Hatchet: This is an Edged Weapon.

>     cardProperties Lantern = villageCardProperties Lantern
>         [] [lantern] []
>       where
>         lantern =
>             ("Light +2",
>              undefined)
> -- Lantern: This Item always provides Light +2, even without a Hero present.

>     cardProperties LightstoneGem = villageCardProperties LightstoneGem
>         [] [lightstoneGem] []
>       where
>         lightstoneGem =
>             ("Light +3",
>              undefined)
> -- Lightstone Gem: This Item always provides Light +3, even without a Hero
> -- present.

>     cardProperties MagicalAura = villageCardProperties MagicalAura
>         [magicalAura] [] []
>       where
>         magicalAura =
>             ("DUNGEON: All Weapons become Weight 0.  Draw one card.",
>              undefined)
> -- Magical Aura: When played with a Polearm, the Hero must still have a
> -- Strength of 8 or more to gain the +6 bonus.  You must draw a card when
> -- using this Dungeon Effect.

>     cardProperties Pawnbroker = villageCardProperties Pawnbroker
>         [] [] [destroyCardFor3,destroyThisFor2Gold]
>       where
>         destroyCardFor3 =
>             ("VILLAGE: Destroy any card with a gold value to gain its "
>              ++ "gold value plus 3 Gold.",
>              doDestroyFor3)
>         destroyThisFor2Gold =
>             ("VILLAGE: Destroy this card to gain 2 Gold.",
>              doDestroyFor2)
> -- Pawnbroker: You can destroy both the Pawnbroker and another card to
> -- produce X+5 gold in a single turn.  When you destroy a card with
> -- Pawnbroker, do not add its inherent gold value to your total gold
> -- that turn.
>         doDestroyFor2 playerId playerState cardIndex = do
>             setPlayerState playerId playerState {
>                 villageEffectsGold = villageEffectsGold playerState + 2
>                 }
>             destroyIndex playerId cardIndex
>             let (text,_) = destroyThisFor2Gold
>             return (Just [PlayerUseEffect playerId
>                                           (VillageCard Pawnbroker) text])
>         doDestroyFor3 playerId playerState _ = do
>             backoutState <- getPlayerState playerId
>             hand <- getHand playerId
>             setPlayerState playerId
>                 (ChoosingOption WhichCardToDestroy
>                      (map chooseCard $ filter hasGoldValue $ zip [0..] hand)
>                      (Just (setPlayerState playerId backoutState)))
>             return (Just [])
>           where
>             goldValue card = cardVillageGold (cardProperties card)
>             hasGoldValue (_,card) = goldValue card > 0
>             chooseCard (index,card) =
>                 ((index,show card),do
>                      setPlayerState playerId playerState {
>                          villageEffectsGold =
>                                  villageEffectsGold playerState
>                                      + goldValue card + 3
>                          }
>                      let (text,_) = destroyCardFor3
>                      return [PlayerUseEffect playerId
>                                        (VillageCard Pawnbroker) text,
>                              PlayerDestroyCard playerId card])

>     cardProperties Polearm = villageCardProperties Polearm
>         [] [polearm] []
>       where
>         polearm =
>             ("ATTACK +2, or ATTACK +6 when attached to a Hero with 8 "
>              ++ "or more Strength.",
>              undefined)
> -- Polearm: A Hero with a Strength of 2 can equip the Polearm for an
> -- Attack bonus of +2.  A Hero with a Strength of 8 or higher gains +6
> -- instead.

>     cardProperties ShortSword = villageCardProperties ShortSword
>         [] [shortSword] []
>       where
>         shortSword =
>             ("ATTACK +4",
>              undefined)
> -- Short Sword: This is an Edged Weapon.

>     cardProperties Spear = villageCardProperties Spear
>         [throwSpear] [spear] []
>       where
>         throwSpear =
>             ("DUNGEON: If you destroy (throw) the Spear, the Attack "
>              ++ "bonus increases by an additional +3, for a total of "
>              ++ "+5.  However, the Spear is still considered equipped "
>              ++ "for the entire battle, even if you use the effect.",
>              undefined)
>         spear =
>             ("ATTACK +2",
>              undefined)

>     cardProperties TownGuard = villageCardProperties TownGuard
>         [] [] [draw2,draw3]
>       where
>         draw2 =
>             ("VILLAGE: Draw two cards.",doDraw2)
>         draw3 =
>             ("VILLAGE: Destroy this card to draw three additional cards.",
>              doDraw3)
> -- Town Guard: Destroying this card allows you to draw a total of five
> -- extra cards.
>         doDraw2 playerId playerState _ = do
>             cards <- multiple 2 $ drawCard playerId
>             hand <- getHand playerId
>             setHand playerId (cards ++ hand)
>             setPlayerState playerId playerState {
>                 villageEffects =
>                     map addEffects cards ++ villageEffects playerState
>                 }
>             let (text,_) = draw2
>             return (Just [PlayerUseEffect playerId (VillageCard TownGuard)
>                                           text])
>         doDraw3 playerId playerState cardIndex = do
>             setPlayerState playerId playerState
>             destroyIndex playerId cardIndex
>             cards <- multiple 3 $ drawCard playerId
>             hand <- getHand playerId
>             setHand playerId (cards ++ hand)
>             setPlayerState playerId playerState {
>                 villageEffects =
>                     map addEffects cards ++ villageEffects playerState
>                 }
>             let (text,_) = draw3
>             return (Just [PlayerUseEffect playerId (VillageCard TownGuard)
>                                           text])
>         addEffects card = (card,cardVillageEffects $ cardProperties card)

>     cardProperties Trainer = villageCardProperties Trainer
>         [] [] [destroyMilitiaFor2XP,destroyThisFor2Gold]
>       where
>         destroyMilitiaFor2XP =
>             ("VILLAGE: Destroy one Militia to gain 2 XP.",
>              doDestroyForXP)
>         destroyThisFor2Gold =
>             ("VILLAGE: Destroy this card to gain 2 Gold.",
>              doDestroyForGold)
> -- Trainer: Each Trainer in your hand may only destroy one Militia each
> -- turn.
>         doDestroyForXP playerId playerState _ = do
>             setPlayerState playerId playerState
>             hand <- getHand playerId
>             case filter ((== (HeroCard Militia)) . snd) (zip [0..] hand) of
>               (militiaIndex,_):_ -> do
>                 xp <- getXP playerId
>                 setXP playerId (xp + 2)
>                 let (text,_) = destroyMilitiaFor2XP
>                 return (Just [PlayerUseEffect playerId (VillageCard Trainer)
>                                               text])
>               _ -> -- no Militia: using up this Village Effect is okay
>                 return (Just [])
>         doDestroyForGold playerId playerState cardIndex = do
>             setPlayerState playerId playerState {
>                 villageEffectsGold = villageEffectsGold playerState + 2
>                 }
>             destroyIndex playerId cardIndex
>             let (text,_) = destroyThisFor2Gold
>             return (Just [PlayerUseEffect playerId (VillageCard Trainer)
>                                           text])

>     cardProperties Warhammer = villageCardProperties Warhammer
>         [] [warhammer] []
>       where
>         warhammer =
>             ("ATTACK +3\nClerics gain an additional ATTACK +3 "
>              ++ "against Doomknights and Undead.",
>              undefined)
> -- Warhammer: A Cleric attacking a Doomknight or Undead gains a total
> -- Attack bonus of +6.

>     cardProperties _ = undefined
