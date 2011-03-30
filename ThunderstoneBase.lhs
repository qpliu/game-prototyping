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

> module ThunderstoneBase

The interface:

Data types:

ThunderstoneState and PlayerId are opaque.

>     (ThunderstoneState,PlayerId,
>      Card(..),ThunderstonePlayerState(..),ThunderstonePublicState(..),
>      PlayerAction(..),PlayerOption(..),PlayerStateInfo(..),
>      ThunderstoneEvent(..),
>      GameSetup,

Initialization:

>      thunderstoneInit,

Game setup:

>      firstGame,randomizedGame,specifiedGame,
>      thunderstoneStartGame,

Game state:

>      thunderstonePublicState,
>      thunderstonePlayerIds,
>      thunderstonePlayerState,

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
>      CardClass(..))
> import ThunderstoneCardDetails(CardDetails(..),
>      thunderstoneDetails,
>      heroDetails,
>      monsterDetails,
>      villageDetails,
>      cardsOfType,levelsUpTo)

==============================================================================
Exported functions:

Initialize the state with the random number generator.

> thunderstoneInit :: StdGen -> ThunderstoneState
> thunderstoneInit stdGen = ThunderstoneState {
>     thunderstoneStdGen = stdGen,
>     thunderstoneCurrentPlayer = error "undefined thunderstoneCurrentPlayer",
>     thunderstonePlayers = [],
>     thunderstoneDungeon = error "undefined thunderstoneDungeon",
>     thunderstoneHeroes = error "undefined thunderstoneHeroes",
>     thunderstoneVillage = error "undefined thunderstoneVillage",
>     thunderstoneGameOver = error "undefined thunderstoneGameOver"
>     }

Start a game, specifying the number of players and choosing which
cards are in the game, which can be either a specific set of cards,
or randomly chosen cards.

> thunderstoneStartGame :: ThunderstoneState -> Int -> GameSetup
>                       -> (ThunderstoneState,[PlayerId])
> thunderstoneStartGame state numberOfPlayers gameSetup =
>     runStateTransformer (setup numberOfPlayers gameSetup) state

Various queries about the state of the game.

> data ThunderstonePublicState = ThunderstonePublicState {
>     publicStateGameOver :: Bool,
>     publicStateScores :: [(PlayerId,Int)],
>     publicStateTakingAction :: [PlayerId],
>     publicStateDungeonHall :: [Card],
>     publicStateHeroes :: [(HeroType,[HeroCard])],
>     publicStateVillage :: [(VillageCard,Int)]
>     }

> data ThunderstonePlayerState = ThunderstonePlayerState {
>     playerStateHand :: [Card],
>     playerStateDeck :: Int,
>     playerStateDiscards :: Int,
>     playerStateXP :: Int,
>     playerStateOptions :: [PlayerAction],
>     playerStateDungeonHall :: [Card],
>     playerStateHeroes :: [(HeroType,[HeroCard])],
>     playerStateVillage :: [(VillageCard,Int)],
>     playerStateInfo :: [PlayerStateInfo]
>     }

> thunderstonePublicState :: ThunderstoneState -> ThunderstonePublicState
> thunderstonePublicState state =
>     thunderstoneGetState state getThunderstonePublicState

> thunderstonePlayerIds :: ThunderstoneState -> [PlayerId]
> thunderstonePlayerIds state = thunderstoneGetState state getPlayerIds

> thunderstonePlayerState :: ThunderstoneState -> PlayerId
>                         -> ThunderstonePlayerState
> thunderstonePlayerState state playerId =
>     thunderstoneGetState state (getThunderstonePlayerState playerId)

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
>         level a b = compare (cardIcon $ heroDetails a)
>                             (cardIcon $ heroDetails b)

There are 15 of each basic card and 8 of each village card.

>     villageStack :: VillageCard -> (VillageCard,Int)
>     villageStack card = (card,cardCount $ villageDetails card)

Game state:

> type Thunderstone a = StateTransformer ThunderstoneState a

> newtype PlayerId = PlayerId Int
>     deriving Eq

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
>         villageEffectsUsed :: [Bool],
>         villageEffectsNumberOfBuys :: Int,
>         villageEffectsGold :: Int
>         }
>   | UsingVillageEffectsCard {
>         villageEffectsCardIndex :: Int,
>         villageEffectsEffects :: [(VillageEffect,Bool)],
>         villageEffectsUsed :: [Bool],
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

>   | DiscardingCards {
>         discardingCards :: [Card],
>         discardingCardCount :: Int,
>         discardingCardsDone :: (PlayerId,[Card]) -> Thunderstone ()
>         }
>   | DiscardingHero {
>         discardingCardsDone :: (PlayerId,[Card]) -> Thunderstone ()
>         }
>   | DiscardingTwoCardsOrOneHero {
>         discardingCardsDone :: (PlayerId,[Card]) -> Thunderstone ()
>         }
>   | WaitingForDiscards {
>         waitingForDiscards :: [(PlayerId,[Card])],
>         waitingForDiscardsDone :: [(PlayerId,[Card])] -> Thunderstone ()
>         }

>   | ChoosingOption PlayerOption
>         [((Int,String),Thunderstone [ThunderstoneEvent])]
>         (Maybe (Thunderstone ()))

> data PlayerOption =
>     WhichCardToDestroy
>   | WhichCardToDiscard
>   | WhichCardToBuy
>   | WhichCardToUse
>   | WhichEffectToUse
>   | WhichHeroToLevelUp
>   deriving Show

> data PlayerStateInfo =
>     PlayerStatePurchases Int
>   | PlayerStateGold Int
>   | PlayerStateAttack Int
>   | PlayerStateMagicAttack Int
>   | PlayerStateLight Int
>   | PlayerStateOption PlayerOption
>   | PlayerStateEffectsUsed [Bool]
>   | PlayerStateCard Card
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
>   | FinishUsingCard
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

>   deriving (Eq,Show)

> data ThunderstoneEvent =
>     PlayerEvent PlayerId PlayerAction
>   | PlayerRevealCards PlayerId [Card]
>   | PlayerPurchase PlayerId Card
>   | PlayerUpgrade PlayerId HeroCard HeroCard
>   | PlayerDiscard PlayerId Card
>   | PlayerDestroyCard PlayerId Card
>   | PlayerUseEffect PlayerId Card String
>   | PlayerStartsTurn PlayerId
>   | GameOverEvent [(PlayerId,Int)]

Plus breach effects and other things that players should be notified of.


Game mechanics

> isGameOver :: Thunderstone Bool
> isGameOver = do
>     state <- getState
>     return (thunderstoneGameOver state)

> getThunderstonePublicState :: Thunderstone ThunderstonePublicState
> getThunderstonePublicState = do
>     state <- getState
>     scores <- getScores
>     playerIds <- getPlayerIds
>     takingAction <- filterM takingAction playerIds
>     return ThunderstonePublicState {
>         publicStateGameOver = thunderstoneGameOver state,
>         publicStateScores =
>             if thunderstoneGameOver state then scores else [],
>         publicStateTakingAction = takingAction,
>         publicStateDungeonHall = take 3 (thunderstoneDungeon state),
>         publicStateHeroes = thunderstoneHeroes state,
>         publicStateVillage = thunderstoneVillage state
>         }
>   where
>     takingAction playerId = do
>         playerState <- getPlayerState playerId
>         case playerState of
>           Waiting -> return False
>           WaitingForDiscards {} -> return False
>           _ -> return True

> getThunderstonePlayerState :: PlayerId
>                            -> Thunderstone ThunderstonePlayerState
> getThunderstonePlayerState playerId = do
>     state <- getState
>     player <- getPlayer playerId
>     playerOptions <- getPlayerOptions playerId
>     playerState <- getPlayerState playerId
>     hand <- getHand playerId
>     return ThunderstonePlayerState {
>         playerStateHand = playerHand player,
>         playerStateDeck = length (playerDeck player),
>         playerStateDiscards = length (playerDiscard player),
>         playerStateXP = playerXP player,
>         playerStateOptions = playerOptions,
>         playerStateDungeonHall = take 3 (thunderstoneDungeon state),
>         playerStateHeroes = thunderstoneHeroes state,
>         playerStateVillage = thunderstoneVillage state,
>         playerStateInfo = getStateInfo hand playerState
>         }
>   where
>     getStateInfo hand UsingVillageEffects {
>             villageEffectsUsed = effectsUsed,
>             villageEffectsNumberOfBuys = buys,
>             villageEffectsGold = gold
>             } =
>         [PlayerStatePurchases (buys + 1),
>          PlayerStateGold (gold + sum (map cardVillageGold hand)),
>          PlayerStateEffectsUsed effectsUsed]
>     getStateInfo hand UsingVillageEffectsCard {
>             villageEffectsCardIndex = cardIndex,
>             villageEffectsUsed = effectsUsed,
>             villageEffectsNumberOfBuys = buys,
>             villageEffectsGold = gold
>             } =
>         [PlayerStatePurchases (buys + 1),
>          PlayerStateGold (gold + sum (map cardVillageGold hand)),
>          PlayerStateEffectsUsed effectsUsed,
>          PlayerStateCard (hand !! cardIndex)]
>     getStateInfo _ PurchasingCards {
>             purchasingNumberOfBuys = buys,
>             purchasingGold = gold
>             } =
>         [PlayerStatePurchases buys, PlayerStateGold gold]
>     getStateInfo _ (ChoosingOption playerOption _ _) =
>         [PlayerStateOption playerOption]
>     getStateInfo _ _ = []

> getScores :: Thunderstone [(PlayerId,Int)]
> getScores = do
>     playerIds <- getPlayerIds
>     scores <- mapM getScore playerIds
>     return (zip playerIds scores)
>   where

This is where the card text on Horde, Swarm, Stone of Terror and
Stone of Scorn override the generic scoring.

>     getScore :: PlayerId -> Thunderstone Int
>     getScore playerId = do
>         player <- getPlayer playerId
>         return $ sum $ map cardScore
>                $ playerHand player ++ playerDeck player
>                                    ++ playerDiscard player
>     cardScore :: Card -> Int
>     cardScore (MonsterCard card) = cardVP $ monsterDetails card
>     cardScore (HeroCard card) = cardVP $ heroDetails card
>     cardScore (VillageCard card) = cardVP $ villageDetails card
>     cardScore DiseaseCard = 0
>     cardScore (ThunderstoneCard card) = cardVP $ thunderstoneDetails card
>     cardVP :: CardDetails cardType -> Int
>     cardVP details = maybe 0 id $ cardVictoryPoints details

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
>             villageEffectsUsed = cardUsed
>             } = do
>         hand <- getHand playerId
>         return ([FinishUsingVillageEffects,EndTurn]
>                 ++ [ChooseOption (index,show card)
>                     | (index,(card,used)) <- zip [0..] (zip hand cardUsed),
>                       not used, not (null $ cardVillageEffects card)])

>     optionsWhen UsingVillageEffectsCard {
>             villageEffectsEffects = effects
>             } = do
>         return ([FinishUsingCard,FinishUsingVillageEffects,EndTurn]
>                 ++ [ChooseOption (index,effectText)
>                     | (index,((effectText,_),used)) <- zip [0..] effects,
>                       not used])

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

>     optionsWhen UsingDungeonEffects = error "undefined optionsWhen UsingDungeonEffects"

>     optionsWhen AttackingMonster = error "undefined optionsWhen AttackingMonster"

>     optionsWhen TakingSpoils = error "undefined optionsWhen TakingSpoils"

>     optionsWhen Resting = return [DestroyCard,EndTurn]

>     optionsWhen Waiting = return []

>     optionsWhen DiscardingCards {
>                     discardingCards = cards,
>                     discardingCardCount = count
>                     } = do
>         hand <- getHand playerId
>         if null hand || length cards >= count
>           then return [Backout]
>           else return $ map ChooseOption $ zip [0..] (map show hand)

>     optionsWhen DiscardingHero {} = do
>         heroes <- fmap (filter isHero) (getHand playerId)
>         if null heroes
>           then return [Backout]
>           else return $ map ChooseOption $ zip [0..] (map show heroes)

>     optionsWhen DiscardingTwoCardsOrOneHero {} = do
>         hand <- getHand playerId
>         if null hand
>           then return [Backout]
>           else return $ map ChooseOption $ zip [0..] (map show hand)

>     optionsWhen WaitingForDiscards {} = return []

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
>         checkIfDoneWaitingForDiscards
>         gameOver <- isGameOver
>         if gameOver
>           then do
>             scores <- getScores
>             return (fmap (++ [GameOverEvent scores]) result)
>           else do
>             nextTurn <- startNextTurnIfTurnFinished
>             return (fmap (++ nextTurn) result)

>   where

>     startNextTurnIfTurnFinished :: Thunderstone [ThunderstoneEvent]
>     startNextTurnIfTurnFinished = do
>         turnFinished <- isTurnFinished
>         if not turnFinished
>           then
>             return []
>           else do
>             state <- getState
>             setState state {
>                 thunderstoneCurrentPlayer =
>                     (thunderstoneCurrentPlayer state + 1)
>                         `mod` length (thunderstonePlayers state)
>                 }
>             currentPlayerId <- getCurrentPlayerId
>             setPlayerState currentPlayerId StartingTurn
>             return [PlayerStartsTurn currentPlayerId]

>     isTurnFinished :: Thunderstone Bool
>     isTurnFinished = do
>         playerIds <- getPlayerIds
>         playerStates <- mapM getPlayerState playerIds
>         return (all isWaiting playerStates)
>       where
>         isWaiting Waiting = True
>         isWaiting _ = False

>     checkIfDoneWaitingForDiscards :: Thunderstone ()
>     checkIfDoneWaitingForDiscards = do
>         playerIds <- getPlayerIds
>         playerStates <- mapM getPlayerState playerIds
>         if any isDiscarding playerStates
>           then return ()
>           else do
>             playerId <- getCurrentPlayerId
>             playerState <- getPlayerState playerId
>             case playerState of
>               WaitingForDiscards {
>                       waitingForDiscards = discards,
>                       waitingForDiscardsDone = doneWaiting
>                       } ->
>                 doneWaiting discards
>               _ -> return ()
>       where
>         isDiscarding DiscardingCards {} = True
>         isDiscarding DiscardingHero {} = True
>         isDiscarding DiscardingTwoCardsOrOneHero {} = True
>         isDiscarding _ = False

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
>                 villageEffectsUsed = replicate (length hand) False,
>                 villageEffectsNumberOfBuys = 0,
>                 villageEffectsGold = 0
>                 }
>         return (Just [PlayerEvent playerId VisitVillage,
>                       PlayerRevealCards playerId hand])

>     performAction StartingTurn EnterDungeon = do
>         hand <- getHand playerId
>         setPlayerState playerId Waiting -- temporary code while real code is still undefined
>         return (Just [PlayerEvent playerId EnterDungeon,
>                       PlayerRevealCards playerId hand])

>     performAction StartingTurn Rest = do
>         setPlayerState playerId Resting
>         return (Just [PlayerEvent playerId Rest])

>     performAction StartingTurn _ = return Nothing

Village:

Choose a card from which to use effect:

>     performAction
>             UsingVillageEffects {
>                 villageEffectsUsed = used,
>                 villageEffectsNumberOfBuys = buys,
>                 villageEffectsGold = gold
>                 }
>             (ChooseOption (index,description)) = do
>         hand <- getHand playerId
>         if index < 0 || index >= length used || index >= length hand
>                      || (used !! index)
>                      || description /= show (hand !! index)
>           then return Nothing
>           else do
>             setPlayerState playerId UsingVillageEffectsCard {
>                 villageEffectsCardIndex = index,
>                 villageEffectsEffects =
>                     zip (cardVillageEffects (hand !! index)) (repeat False),
>                 villageEffectsUsed = used,
>                 villageEffectsNumberOfBuys = buys,
>                 villageEffectsGold = gold
>                 }
>             return (Just [])

>     performAction
>             UsingVillageEffects {
>                 villageEffectsNumberOfBuys = numberOfBuys,
>                 villageEffectsGold = gold
>                 }
>             FinishUsingVillageEffects = do
>         hand <- getHand playerId
>         setPlayerState playerId
>             PurchasingCards {
>                 purchasingNumberOfBuys = numberOfBuys + 1,
>                 purchasingGold = gold + sum (map cardVillageGold hand)
>                 }
>         return (Just [])

>     performAction UsingVillageEffects {} EndTurn = do
>         endTurn []
>         return (Just [])

>     performAction UsingVillageEffects {} _ = return Nothing

Choose an effect to activate from the current card:

>     performAction
>             UsingVillageEffectsCard {
>                 villageEffectsCardIndex = cardIndex,
>                 villageEffectsEffects = effects,
>                 villageEffectsUsed = used
>                 }
>             (ChooseOption (index,description))
>       | index < 0 || index >= length effects
>                   || effectUsed
>                   || description /= effectText =
>             return Nothing
>       | otherwise =
>             effect playerId cardIndex markEffectUsed markCardUsed
>       where
>         ((effectText,effect),effectUsed) = effects !! index
>         markEffectUsed = do
>             playerState <- getPlayerState playerId
>             setPlayerState playerId playerState {
>                 villageEffectsEffects =
>                     setIndex (villageEffectsCardIndex playerState)
>                         (villageEffectsEffects playerState)
>                         (fst (villageEffectsEffects playerState !!cardIndex),
>                          True)
>                 }
>         markCardUsed = do
>             playerState <- getPlayerState playerId
>             setPlayerState playerId playerState {
>                 villageEffectsUsed =
>                     setIndex (villageEffectsCardIndex playerState)
>                              (villageEffectsUsed playerState) True
>                 }

>     performAction
>             UsingVillageEffectsCard {
>                 villageEffectsUsed = used,
>                 villageEffectsNumberOfBuys = numberOfBuys,
>                 villageEffectsGold = gold
>                 }
>             FinishUsingCard = do
>         setPlayerState playerId UsingVillageEffects {
>             villageEffectsUsed = used,
>             villageEffectsNumberOfBuys = numberOfBuys,
>             villageEffectsGold = gold
>             }
>         return (Just [])

>     performAction
>             UsingVillageEffectsCard {
>                 villageEffectsUsed = used,
>                 villageEffectsNumberOfBuys = numberOfBuys,
>                 villageEffectsGold = gold
>                 }
>             FinishUsingVillageEffects = do
>         hand <- getHand playerId
>         setPlayerState playerId
>             PurchasingCards {
>                 purchasingNumberOfBuys = numberOfBuys + 1,
>                 purchasingGold = gold + sum (map cardVillageGold hand)
>                 }
>         return (Just [])

>     performAction UsingVillageEffectsCard {} EndTurn = do
>         endTurn []
>         return (Just [])

>     performAction UsingVillageEffectsCard {} _ = return Nothing

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

>     performAction playerState@DiscardingCards {
>                     discardingCards = cards,
>                     discardingCardCount = count,
>                     discardingCardsDone = discardingDone
>                     } (ChooseOption (index,description)) = do
>         hand <- getHand playerId
>         let (discards,holds) = partition chosen (zip [0..] hand)
>             chosen (i,card) = i == index && show card == description
>         if null discards
>           then return Nothing
>           else do
>             let (_,card) = head discards
>             let newHand = map snd holds
>             if null newHand || length cards + 1 >= count
>               then do
>                 setPlayerState playerId Waiting
>                 setHand playerId newHand
>                 discardingDone (playerId,card:cards)
>                 return (Just [PlayerDiscard playerId card])
>               else do
>                 setPlayerState playerId playerState {
>                     discardingCards = card:cards
>                     }
>                 setHand playerId newHand
>                 return (Just [PlayerDiscard playerId card])

>     performAction DiscardingCards {} _ = return Nothing

>     performAction playerState@DiscardingHero {
>                     discardingCardsDone = discardingDone
>                     } (ChooseOption (index,description)) = do
>         hand <- getHand playerId
>         let (discards,holds) = partition chosen (zip [0..] hand)
>             chosen (i,card) = i == index && show card == description
>         if null discards || not (isHero $ snd $ head discards)
>           then return Nothing
>           else do
>             let (_,card) = head discards
>             setPlayerState playerId Waiting
>             setHand playerId (map snd holds)
>             discardingDone (playerId,[card])
>             return (Just [PlayerDiscard playerId card])

>     performAction playerState@DiscardingTwoCardsOrOneHero {
>                     discardingCardsDone = discardingDone
>                     } (ChooseOption (index,description)) = do
>         hand <- getHand playerId
>         let (discards,holds) = partition chosen (zip [0..] hand)
>             chosen (i,card) = i == index && show card == description
>         if null discards
>           then return Nothing
>           else do
>             let (_,card) = head discards
>             setHand playerId (map snd holds)
>             if isHero card
>               then do
>                 setPlayerState playerId Waiting
>                 discardingDone (playerId,[card])
>                 return (Just [PlayerDiscard playerId card])
>               else do
>                 setPlayerState playerId DiscardingCards {
>                     discardingCards = [card],
>                     discardingCardCount = 2,
>                     discardingCardsDone = discardingDone
>                     }
>                 return (Just [PlayerDiscard playerId card])

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
>     addPrice card = (card,cardVillagePrice card)

> purchaseCard :: PlayerId -> Card -> Thunderstone Bool
> purchaseCard playerId card = do
>     playerState <- getPlayerState playerId
>     doPurchase playerState
>   where
>     cardPrice = cardVillagePrice card
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
>     hand <- getHand playerId
>     xp <- getXP playerId
>     let availableHeroes = nub $ concatMap snd $ thunderstoneHeroes state
>     return $ concatMap (upgrades availableHeroes)
>            $ concatMap (filterUpgradeable xp) (nub hand)
>   where
>     filterUpgradeable :: Int -> Card -> [HeroCard]
>     filterUpgradeable xp (HeroCard hero)
>       | maybe False (xp >=) (cardLevelUp $ heroDetails hero) = [hero]
>     filterUpgradeable _ _ = []
>
>     upgrades :: [HeroCard] -> HeroCard -> [(HeroCard,(Int,HeroCard))]
>     upgrades available hero =
>         [(hero,(cost,newHero)) | newHero <- available,
>                                  hero `levelsUpTo` newHero]
>       where
>         Just cost = cardLevelUp $ heroDetails hero

> upgradeHero :: PlayerId -> HeroCard -> HeroCard -> Thunderstone Bool
> upgradeHero playerId oldHero newHero = do
>     xp <- getXP playerId
>     hand <- getHand playerId
>     if not (HeroCard oldHero `elem` hand)
>         || maybe True (xp <) (cardLevelUp $ heroDetails oldHero)
>         || not (oldHero `levelsUpTo` newHero)
>       then
>         return False
>       else do
>         heroUpgrade <- drawHeroUpgrade newHero
>         maybe (return False) (const $ do
>                   setXP playerId (xp - xpCost)
>                   setHand playerId (remove1 (HeroCard oldHero) hand)
>                   discard playerId [HeroCard newHero]
>                   return True)
>               heroUpgrade
>   where
>     Just xpCost = cardLevelUp (heroDetails oldHero)

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
>             villageEffectsUsed = used
>             } =
>         playerState { villageEffectsUsed = removeIndex index used }
>     destroyIndexInState playerState@UsingVillageEffectsCard {
>             villageEffectsCardIndex = cardIndex,
>             villageEffectsUsed = used,
>             villageEffectsNumberOfBuys = numberOfBuys,
>             villageEffectsGold = gold
>             }
>       | index < cardIndex =
>         playerState {
>             villageEffectsCardIndex = cardIndex - 1,
>             villageEffectsUsed = removeIndex index used
>             }
>       | index == cardIndex =
>         UsingVillageEffects {
>             villageEffectsUsed = removeIndex index used,
>             villageEffectsNumberOfBuys = numberOfBuys,
>             villageEffectsGold = gold
>             }
>       | otherwise =
>         playerState {
>             villageEffectsUsed = removeIndex index used
>             }
>     destroyIndexInState playerState@UsingDungeonEffects {} =
>         error "destroyIndex UsingDungeonEffects" -- undefined
>     destroyIndexInState playerState = playerState

> multiple :: (Functor m, Monad m) => Int -> m (Maybe a) -> m [a]
> multiple count action = fmap catMaybes (replicateM count action)

> removeIndex :: Int -> [a] -> [a]
> removeIndex index list = take index list ++ drop (index+1) list

> remove1 :: Eq a => a -> [a] -> [a]
> remove1 item items = before ++ drop 1 after
>   where
>     (before,after) = span (/= item) items

> setIndex :: Int -> [a] -> a -> [a]
> setIndex index list item = take index list ++ item : drop (index+1) list

> thunderstoneGetState :: ThunderstoneState -> Thunderstone a -> a
> thunderstoneGetState state getThunderstoneState =
>     snd $ runStateTransformer getThunderstoneState state

=============================================================================

> cardGoldValue :: Card -> Maybe Int
> cardGoldValue (MonsterCard card) = cardGold $ monsterDetails card
> cardGoldValue (HeroCard card) = cardGold $ heroDetails card
> cardGoldValue (VillageCard card) = cardGold $ villageDetails card
> cardGoldValue DiseaseCard = Nothing
> cardGoldValue (ThunderstoneCard card) = cardGold $ thunderstoneDetails card

> cardVillageGold :: Card -> Int
> cardVillageGold card = maybe 0 id (cardGoldValue card)

> cardVillagePrice :: Card -> Int
> cardVillagePrice (HeroCard card) = maybe 0 id (cardPrice $ heroDetails card)
> cardVillagePrice (VillageCard card) =
>     maybe 0 id (cardPrice $ villageDetails card)
> cardVillagePrice card = error ("cardVillagePrice: " ++ show card)

> cardVillageEffects :: Card -> [VillageEffect]
> cardVillageEffects (MonsterCard _) = []
> cardVillageEffects (HeroCard _) = []
> cardVillageEffects card@(VillageCard villageCard) =
>     concatMap (getVillageEffect card) (cardText $ villageDetails villageCard)

> type DungeonEffect = (String,PlayerId -> Thunderstone ())
> type BattleEffect = (String,PlayerId -> Thunderstone ())
> type SpoilsEffect = (String,PlayerId -> Thunderstone ())

Village Effect:
(String: text of the effect, perform the effect with the PlayerId: of
 the player where the PlayerState: is the players state with the
 effect removed and the Int: is the index of the card in the player's
 hand.  REPEAT effects will not set the player state.)

> type VillageEffect =
>     (String,PlayerId -> Int                   -- cardIndex
>                      -> Thunderstone ()       -- markEffectUsed
>                      -> Thunderstone ()       -- markCardUsed
>                      -> Thunderstone (Maybe [ThunderstoneEvent]))
> type BattleResult = (String,PlayerId -> Thunderstone Bool)
> type BreachEffect = Maybe (String,Thunderstone ())

> getVillageEffect :: Card -> String -> [VillageEffect]
> getVillageEffect card text

>   | text == "VILLAGE: You may purchase one additional card this turn." =
>         [(text, \ playerId cardIndex markEffectUsed markCardUsed -> do
>             markEffectUsed
>             markCardUsed
>             playerState <- getPlayerState playerId
>             setPlayerState playerId playerState {
>                 villageEffectsNumberOfBuys =
>                     villageEffectsNumberOfBuys playerState + 1
>                 }
>             return (Just [PlayerUseEffect playerId card text]))]

>   | text == "VILLAGE: Destroy one Militia to gain 2 XP." =
>         [(text, \ playerId cardIndex markEffectUsed markCardUsed -> do
>             hand <- getHand playerId
>             case filter ((== (HeroCard Militia)) . snd) (zip [0..] hand) of
>               (militiaIndex,_):_ -> do
>                 markEffectUsed
>                 markCardUsed
>                 destroyIndex playerId militiaIndex
>                 xp <- getXP playerId
>                 setXP playerId (xp + 2)
>                 return (Just [PlayerUseEffect playerId card text])
>               _ -> -- no Militia
>                 return Nothing)]

>   | text == "VILLAGE: Destroy this card to gain 2 Gold." =
>         [(text, \ playerId cardIndex markEffectUsed markCardUsed -> do
>             markEffectUsed
>             markCardUsed
>             destroyIndex playerId cardIndex
>             playerState <- getPlayerState playerId
>             setPlayerState playerId playerState {
>                 villageEffectsGold = villageEffectsGold playerState + 2
>                 }
>             return (Just [PlayerUseEffect playerId card text]))]

>   | text == "VILLAGE: Destroy any card with a gold value to gain its gold value plus 3 Gold." =
>         [(text, \ playerId cardIndex markEffectUsed markCardUsed -> do
>             playerState <- getPlayerState playerId
>             hand <- getHand playerId
>             let backout = setPlayerState playerId playerState
>             let used = villageEffectsUsed playerState
>             let eligible ((index,_),_) =
>                     cardGoldValue (hand !! index) /= Nothing
>                         && not (used !! index)
>             let chooseCard index chosenCard =
>                     ((index,show chosenCard),do
>                         setPlayerState playerId playerState {
>                              villageEffectsGold =
>                                      villageEffectsGold playerState
>                                          + cardVillageGold chosenCard + 3
>                              }
>                         markEffectUsed
>                         markCardUsed
>                         destroyIndex playerId index
>                         return [PlayerUseEffect playerId card text,
>                                 PlayerDestroyCard playerId chosenCard])
>             setPlayerState playerId
>                 (ChoosingOption WhichCardToDestroy
>                          (filter eligible $ zipWith chooseCard [0..] hand)
>                          (Just (setPlayerState playerId playerState)))
>             return (Just []))]

>   | text == "VILLAGE: Draw two cards." =
>         [(text, \ playerId cardIndex markEffectUsed markCardUsed -> do
>             markEffectUsed
>             markCardUsed
>             cards <- multiple 2 $ drawCard playerId
>             hand <- getHand playerId
>             setHand playerId (hand ++ cards)
>             playerState <- getPlayerState playerId
>             setPlayerState playerId playerState {
>                 villageEffectsUsed =
>                     villageEffectsUsed playerState
>                         ++ replicate (length cards) False
>                 }
>             return (Just [PlayerUseEffect playerId card text]))]

>   | text == "VILLAGE: Destroy this card to draw three additional cards." =
>         [(text, \ playerId cardIndex markEffectUsed markCardUsed -> do
>             markEffectUsed
>             markCardUsed
>             destroyIndex playerId cardIndex
>             cards <- multiple 3 $ drawCard playerId
>             hand <- getHand playerId
>             setHand playerId (hand ++ cards)
>             playerState <- getPlayerState playerId
>             setPlayerState playerId playerState {
>                 villageEffectsUsed =
>                     villageEffectsUsed playerState
>                         ++ replicate (length cards) False
>                 }
>             return (Just [PlayerUseEffect playerId card text]))]

>   | otherwise = []

> isHero :: Card -> Bool
> isHero (HeroCard _) = True
> isHero _ = False

> hasClass :: Card -> CardClass -> Bool
> hasClass (HeroCard card) cardClass =
>     cardClass `elem` (cardClasses $ heroDetails card)
> hasClass (VillageCard card) cardClass =
>     cardClass `elem` (cardClasses $ villageDetails card)
> hasClass (MonsterCard card) cardClass =
>     cardClass `elem` (cardClasses $ monsterDetails card)
> hasClass (ThunderstoneCard card) cardClass =
>     cardClass `elem` (cardClasses $ thunderstoneDetails card)
> hasClass DiseaseCard cardClass =
>     cardClass `elem` [ClassDisease,ClassSpecial]
