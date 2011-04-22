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
> import Data.List(intersperse,nub,partition,sort,sortBy,zip4,(\\))
> import Data.Maybe(catMaybes,isNothing,listToMaybe)
> import System.Random(StdGen)

> import qualified Shuffle
> import StateTransformer
>     (StateTransformer,getState,runStateTransformer,setState)
> import ThunderstoneCards
>     (HeroType(..),MonsterType(..),
>      ThunderstoneCard(..),HeroCard(..),MonsterCard(..),VillageCard(..),
>      CardClass(..))
> import qualified ThunderstoneCards
> import ThunderstoneCardDetails(CardDetails(..),
>      thunderstoneDetails,
>      heroDetails,
>      monsterDetails,
>      villageDetails,
>      diseaseDetails,
>      cardsOfType,levelsUpTo)

==============================================================================
Exported functions:

Initialize the state with the random number generator.

> thunderstoneInit :: StdGen -> ThunderstoneState
> thunderstoneInit stdGen = ThunderstoneState {
>     thunderstoneStdGen = stdGen,
>     thunderstoneCurrentPlayer = error "thunderstoneCurrentPlayer",
>     thunderstonePlayers = [],
>     thunderstoneDungeon = error "thunderstoneDungeon",
>     thunderstoneHeroes = error "thunderstoneHeroes",
>     thunderstoneVillage = error "thunderstoneVillage",
>     thunderstoneBreached = error "thunderstoneBreached",
>     thunderstoneBreachState = error "thunderstoneBreachState",
>     thunderstoneGameOver = error "thunderstoneGameOver"
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
>   deriving (Eq,Ord)

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
>         thunderstoneBreached = [],
>         thunderstoneBreachState = [],
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
>         dagger <- multiple 2 $ drawVillage Dagger
>         ironRations <- multiple 2 $ drawVillage IronRations
>         torch <- multiple 2 $ drawVillage Torch
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
>     deriving (Eq,Show)

> data ThunderstoneState = ThunderstoneState {
>     thunderstoneStdGen :: StdGen,
>     thunderstoneCurrentPlayer :: Int,
>     thunderstonePlayers :: [Player],
>     thunderstoneDungeon :: [Card],
>     thunderstoneHeroes :: [(HeroType,[HeroCard])],
>     thunderstoneVillage :: [(VillageCard,Int)],
>     thunderstoneBreached :: [Card],
>     thunderstoneBreachState :: [PlayerId],
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

villageEffectsUsed is used to mark each card as having been used.  Used
cards are not eligible to be destroyed to activate effects.

>   | UsingVillageEffects {
>         villageEffectsUsed :: [Maybe [Bool]],
>         villageEffectsNumberOfBuys :: Int,
>         villageEffectsGold :: Int
>         }
>   | UsingVillageEffectsCard {
>         villageEffectsCardIndex :: Int,
>         villageEffectsUsed :: [Maybe [Bool]],
>         villageEffectsNumberOfBuys :: Int,
>         villageEffectsGold :: Int
>         }
>   | PurchasingCards {
>         purchasingNumberOfBuys :: Int,
>         purchasingGold :: Int
>         }
>   | LevelingUpHeroes

dungeonEffectsUsed is used to mark each card as having its Dungeon Effects
been used.  Used cards are not eligible to be destroyed to activate effects.

>   | UsingDungeonEffects {
>         dungeonEffectsUsed :: [Maybe [Bool]],
>         dungeonEffectsStats :: [DungeonPartyStats]
>         }
>   | UsingDungeonEffectsCard {
>         dungeonEffectsCardIndex :: Int,
>         dungeonEffectsUsed :: [Maybe [Bool]],
>         dungeonEffectsStats :: [DungeonPartyStats]
>         }

>   | Resting

>   | Waiting

>   | DiscardingCards {
>         discardingCardsSavedState :: PlayerState,
>         discardingCards :: [Card],
>         discardingCardCount :: Int,
>         discardingCardsDone :: (PlayerId,[Card])
>                             -> Thunderstone [ThunderstoneEvent]
>         }
>   | DiscardingHero {
>         discardingCardsSavedState :: PlayerState,
>         discardingCardsDone :: (PlayerId,[Card])
>                             -> Thunderstone [ThunderstoneEvent]
>         }
>   | DiscardingTwoCardsOrOneHero {
>         discardingCardsSavedState :: PlayerState,
>         discardingCardsDone :: (PlayerId,[Card])
>                             -> Thunderstone [ThunderstoneEvent]
>         }
>   | WaitingForDiscards {
>         waitingForDiscards :: [(PlayerId,[Card])],
>         waitingForDiscardsDone :: [(PlayerId,[Card])]
>                                -> Thunderstone [ThunderstoneEvent]
>         }

>   | ChoosingOption PlayerOption
>         [((Int,String,Maybe PlayerId),Thunderstone [ThunderstoneEvent])]
>         (Maybe (Thunderstone ()))

> data PlayerOption =
>     WhichCardToDestroy
>   | WhichCardToDiscard
>   | WhichCardToBuy
>   | WhichCardToUse
>   | WhichEffectToUse
>   | WhichHeroToLevelUp
>   | WhichHeroToBorrow
>   | WhichHeroToEquip
>   | WhichMonster
>   | WhichWeaponToEquip
>   deriving Show

> data PlayerStateInfo =
>     PlayerStatePurchases Int
>   | PlayerStateGold Int
>   | PlayerStateAttack [Int]
>   | PlayerStateMagicAttack [Int]
>   | PlayerStateLight [Int]
>   | PlayerStateOption PlayerOption
>   | PlayerStateEffectsUsed [Bool]
>   | PlayerStateEquipped [(Int,Int)]
>   | PlayerStateStrength [Int]
>   | PlayerStateWeight [Int]
>   | PlayerStateNotAttacking [Bool]
>   | PlayerStateCard Card
>   deriving Show

> data DungeonPartyStats = DungeonPartyStats {
>         dungeonPartyEquippedWith :: Maybe Int,
>         dungeonPartyEquippedBy :: Maybe Int,
>         dungeonPartyStrength :: Int,
>         dungeonPartyAttack :: Int,
>         dungeonPartyMagicAttack :: Int,
>         dungeonPartyLight :: Int,
>         dungeonPartyWeight :: Int,
>         dungeonPartyNotAttacking :: Bool,
>         dungeonPartyDestroyed :: Bool,
>         dungeonPartyBorrowedFrom :: Maybe PlayerId
>     }

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

>   | ChooseOption (Int,String,Maybe PlayerId)
>   | Backout

>   deriving (Eq,Show)

> data ThunderstoneEvent =
>     ThunderstoneEventPlayerAction PlayerId PlayerAction
>   | ThunderstoneEventRevealCards PlayerId [Card]
>   | ThunderstoneEventDrawCards PlayerId [Card]
>   | ThunderstoneEventReshuffle PlayerId
>   | ThunderstoneEventPurchase PlayerId Card
>   | ThunderstoneEventUpgrade PlayerId HeroCard HeroCard
>   | ThunderstoneEventDiscard PlayerId Card
>   | ThunderstoneEventDestroyCard PlayerId Card
>   | ThunderstoneEventUseEffect PlayerId Card String
>   | ThunderstoneEventBattleEffect PlayerId Card String
>   | ThunderstoneEventSpoilsEffect PlayerId Card String
>   | ThunderstoneEventBorrowCard PlayerId Card PlayerId
>   | ThunderstoneEventPlayerStartsTurn PlayerId
>   | ThunderstoneEventEquip PlayerId Card Card
>   | ThunderstoneEventGainDungeonCard PlayerId Int Card
>   | ThunderstoneEventReturnMonster Int Card
>   | ThunderstoneEventDungeonHallChanged
>   | ThunderstoneEventBreach Card String
>   | ThunderstoneEventAttack PlayerId Int Card
>   | ThunderstoneEventLoseBattle PlayerId Int Card
>   | ThunderstoneEventWinBattle PlayerId Int Card
>   | ThunderstoneEventGameOver [(PlayerId,Int)]


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
>          PlayerStateEffectsUsed (map (not . isNothing) effectsUsed)]
>     getStateInfo hand UsingVillageEffectsCard {
>             villageEffectsCardIndex = cardIndex,
>             villageEffectsUsed = effectsUsed,
>             villageEffectsNumberOfBuys = buys,
>             villageEffectsGold = gold
>             } =
>         [PlayerStatePurchases (buys + 1),
>          PlayerStateGold (gold + sum (map cardVillageGold hand)),
>          PlayerStateEffectsUsed (map (not . isNothing) effectsUsed),
>          PlayerStateCard (hand !! cardIndex)]
>     getStateInfo _ PurchasingCards {
>             purchasingNumberOfBuys = buys,
>             purchasingGold = gold
>             } =
>         [PlayerStatePurchases buys, PlayerStateGold gold]
>     getStateInfo _ UsingDungeonEffects {
>             dungeonEffectsUsed = effectsUsed,
>             dungeonEffectsStats = stats
>             } =
>         [PlayerStateEffectsUsed (map (not . isNothing) effectsUsed)]
>         ++ dungeonEffectsStatsInfo stats
>     getStateInfo hand UsingDungeonEffectsCard {
>             dungeonEffectsCardIndex = cardIndex,
>             dungeonEffectsUsed = effectsUsed,
>             dungeonEffectsStats = stats
>             } =
>         [PlayerStateEffectsUsed (map (not . isNothing) effectsUsed),
>          PlayerStateCard (hand !! cardIndex)]
>         ++ dungeonEffectsStatsInfo stats
>     getStateInfo _ (ChoosingOption playerOption _ _) =
>         [PlayerStateOption playerOption]
>     getStateInfo _ _ = []
>     dungeonEffectsStatsInfo stats =
>         [PlayerStateEquipped (concat $ zipWith equipped [0..] stats),
>          PlayerStateStrength (map dungeonPartyStrength stats),
>          PlayerStateWeight (map dungeonPartyWeight stats),
>          PlayerStateAttack (map dungeonPartyAttack stats),
>          PlayerStateMagicAttack (map dungeonPartyMagicAttack stats),
>          PlayerStateLight (map dungeonPartyLight stats),
>          PlayerStateNotAttacking (map dungeonPartyNotAttacking stats)]
>       where
>         equipped index stat =
>             maybe [] (\ i -> [(index,i)]) (dungeonPartyEquippedWith stat)

> getScores :: Thunderstone [(PlayerId,Int)]
> getScores = do
>     playerIds <- getPlayerIds
>     scores <- mapM getScore playerIds
>     return (zip playerIds scores)
>   where

This is where the card text on Horde, Swarm, Stone of Terror and
Stone of Scorn would override the generic scoring.

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
>             villageEffectsUsed = used
>             } = do
>         hand <- getHand playerId
>         return ([FinishUsingVillageEffects,EndTurn]
>                 ++ [ChooseOption (index,show card,Nothing)
>                     | (index,(card,cardUsed)) <- zip [0..] (zip hand used),
>                       maybe (not $ null $ cardVillageEffects card)
>                             (not . and)
>                             cardUsed])

>     optionsWhen UsingVillageEffectsCard {
>             villageEffectsCardIndex = cardIndex,
>             villageEffectsUsed = used
>             } = do
>         hand <- getHand playerId
>         return ([FinishUsingCard,FinishUsingVillageEffects,EndTurn]
>                 ++ [ChooseOption (index,effectText,Nothing)
>                     | (index,(effectText,_)) <-
>                           zip [0..] (cardVillageEffects (hand !! cardIndex)),
>                       maybe True (not . (!! index)) (used !! cardIndex)])

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

>     optionsWhen UsingDungeonEffects {
>             dungeonEffectsUsed = used,
>             dungeonEffectsStats = stats
>             } = do
>         hand <- getHand playerId
>         return ([AttackMonster]
>                 ++ (if null (availableEquippings (zip hand stats))
>                       then [] else [EquipHero])
>                 ++ [ChooseOption (index,show card,Nothing)
>                     | (index,(card,cardUsed)) <- zip [0..] (zip hand used),
>                       maybe (not $ null $ cardDungeonEffects card)
>                             (not . and)
>                             cardUsed])

>     optionsWhen UsingDungeonEffectsCard {
>             dungeonEffectsCardIndex = cardIndex,
>             dungeonEffectsUsed = used,
>             dungeonEffectsStats = stats
>             } = do
>         hand <- getHand playerId
>         return ([FinishUsingCard,AttackMonster]
>                 ++ [ChooseOption (index,effectText,Nothing)
>                     | (index,(effectText,_)) <-
>                           zip [0..] (cardDungeonEffects (hand !! cardIndex)),
>                       maybe True (not . (!! index)) (used !! cardIndex)])

>     optionsWhen Resting = return [DestroyCard,EndTurn]

>     optionsWhen Waiting = return []

>     optionsWhen DiscardingCards {
>                     discardingCards = cards,
>                     discardingCardCount = count
>                     } = do
>         hand <- getHand playerId
>         if null hand || length cards >= count
>           then return [Backout]
>           else return $ map ChooseOption
>                       $ zip3 [0..] (map show hand) (repeat Nothing)

>     optionsWhen DiscardingHero {} = do
>         heroes <- fmap (filter isHero) (getHand playerId)
>         if null heroes
>           then return [Backout]
>           else return $ map ChooseOption
>                       $ zip3 [0..] (map show heroes) (repeat Nothing)

>     optionsWhen DiscardingTwoCardsOrOneHero {} = do
>         hand <- getHand playerId
>         if null hand
>           then return [Backout]
>           else return $ map ChooseOption
>                       $ zip3 [0..] (map show hand) (repeat Nothing)

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
>             return (fmap (++ [ThunderstoneEventGameOver scores]) result)
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
>             return [ThunderstoneEventPlayerStartsTurn currentPlayerId]

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
>                       } -> do
>                 doneWaiting discards
>                 checkIfDoneWaitingForDiscards
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
>         deckSize <- fmap (length . playerDeck) (getPlayer playerId)
>         hand <- multiple 6 $ drawCard playerId
>         setHand playerId hand
>         setPlayerState playerId Waiting
>         return (Just (events ++ if deckSize < 6
>                                   then [ThunderstoneEventReshuffle playerId]
>                                   else []))

>     performAction :: PlayerState -> PlayerAction
>                   -> Thunderstone (Maybe [ThunderstoneEvent])

>     performAction StartingTurn VisitVillage = do
>         hand <- getHand playerId
>         setPlayerState playerId
>             UsingVillageEffects {
>                 villageEffectsUsed = replicate (length hand) Nothing,
>                 villageEffectsNumberOfBuys = 0,
>                 villageEffectsGold = 0
>                 }
>         return (Just [ThunderstoneEventPlayerAction playerId VisitVillage,
>                       ThunderstoneEventRevealCards playerId hand])

>     performAction StartingTurn EnterDungeon = do
>         hand <- getHand playerId
>         setPlayerState playerId UsingDungeonEffects {
>             dungeonEffectsUsed = replicate (length hand) Nothing,
>             dungeonEffectsStats = map initDungeonPartyStats hand
>             }
>         return (Just [ThunderstoneEventPlayerAction playerId EnterDungeon,
>                       ThunderstoneEventRevealCards playerId hand])

>     performAction StartingTurn Rest = do
>         setPlayerState playerId Resting
>         return (Just [ThunderstoneEventPlayerAction playerId Rest])

>     performAction StartingTurn _ = return Nothing

Village:

Choose a card from which to use effect:

>     performAction
>             UsingVillageEffects {
>                 villageEffectsUsed = used,
>                 villageEffectsNumberOfBuys = buys,
>                 villageEffectsGold = gold
>                 }
>             (ChooseOption (index,description,_)) = do
>         hand <- getHand playerId
>         if index < 0 || index >= length hand
>                      || description /= show (hand !! index)
>           then return Nothing
>           else do
>             setPlayerState playerId UsingVillageEffectsCard {
>                 villageEffectsCardIndex = index,
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

>     performAction UsingVillageEffects {} _ = return Nothing

Choose an effect to activate from the current card:

>     performAction
>             UsingVillageEffectsCard {
>                 villageEffectsCardIndex = cardIndex,
>                 villageEffectsUsed = used
>                 }
>             (ChooseOption (index,description,_)) = do
>         hand <- getHand playerId
>         if index < 0 || index >= length (effects hand) || effectUsed
>           then
>             return Nothing
>           else
>             effect hand playerId cardIndex markCardUsed markEffectUsed
>       where
>         effects hand = cardVillageEffects (hand !! cardIndex)
>         effectText hand = fst (effects hand !! index)
>         effect hand = snd (effects hand !! index)
>         effectUsed = maybe False (!! index) (used !! cardIndex)
>         markCardUsed = do
>             playerState <- getPlayerState playerId
>             hand <- getHand playerId
>             let cardIndex = villageEffectsCardIndex playerState
>             let effects = cardVillageEffects (hand !! cardIndex)
>             maybe (setPlayerState playerId playerState {
>                        villageEffectsUsed =
>                            setIndex cardIndex
>                                     (villageEffectsUsed playerState)
>                                     (Just $ replicate (length effects) False)
>                     })
>                   (const (return ()))
>                   (villageEffectsUsed playerState !! cardIndex)
>         markEffectUsed = do
>             playerState <- getPlayerState playerId
>             hand <- getHand playerId
>             let cardIndex = villageEffectsCardIndex playerState
>             let effects = cardVillageEffects (hand !! cardIndex)
>             setPlayerState playerId playerState {
>                 villageEffectsUsed =
>                     setIndex cardIndex
>                              (villageEffectsUsed playerState)
>                              (markUsed (length effects)
>                                   (villageEffectsUsed playerState
>                                    !! cardIndex))
>                 }
>           where
>             markUsed numberOfEffects Nothing =
>                 Just $ setIndex index (replicate numberOfEffects False) True
>             markUsed _ (Just flags) = Just $ setIndex index flags True

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
>         purchaseOption index (card,_) =
>             ((index,show card,Nothing),doPurchase card)
>         doPurchase card = do
>             setPlayerState playerId playerState
>             result <- purchaseCard playerId card
>             return [ThunderstoneEventPurchase playerId card]
>         backout = do
>             setPlayerState playerId playerState

>     performAction PurchasingCards {} FinishPurchasingCards = do
>         setPlayerState playerId LevelingUpHeroes
>         return (Just [])

>     performAction PurchasingCards {} EndTurn = do
>         endTurn []

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
>             ((index,showUpgrade oldHero newHero price,Nothing),
>              doUpgrade oldHero newHero)
>         showUpgrade oldHero newHero price =
>             show (HeroCard oldHero) ++ " -> " ++ show (HeroCard newHero)
>                                     ++ " (" ++ show price ++ "XP)"
>         doUpgrade oldHero newHero = do
>             result <- upgradeHero playerId oldHero newHero
>             setPlayerState playerId LevelingUpHeroes
>             if result
>               then return [ThunderstoneEventUpgrade playerId oldHero newHero]
>               else return []
>         backout = do
>             setPlayerState playerId LevelingUpHeroes

>     performAction LevelingUpHeroes EndTurn = do
>         endTurn []

>     performAction LevelingUpHeroes _ = return Nothing

Dungeon:

>     performAction
>             playerState@UsingDungeonEffects {
>                 dungeonEffectsStats = stats
>                 }
>             EquipHero = do
>         hand <- getHand playerId
>         if null (equippings hand)
>           then
>             return Nothing
>           else do
>             setPlayerState playerId
>                 (ChoosingOption WhichHeroToEquip
>                      (map (chooseHero hand)
>                           (nub $ map fst $ equippings hand))
>                      (Just backout))
>             return (Just [])
>       where
>         backout = setPlayerState playerId playerState
>         equippings hand = availableEquippings (zip hand stats)
>         chooseHero hand heroIndex =
>             ((heroIndex,show (hand !! heroIndex),Nothing),
>              equipHero hand heroIndex)
>         equipHero hand heroIndex = do
>             setPlayerState playerId
>                 (ChoosingOption WhichWeaponToEquip
>                     (map (chooseWeapon hand heroIndex)
>                          (map snd $ filter ((== heroIndex) . fst)
>                                            (equippings hand)))
>                     (Just backout))
>             return []
>         chooseWeapon hand heroIndex weaponIndex = do
>             ((weaponIndex,show (hand !! weaponIndex),Nothing),
>              equipWeapon hand heroIndex weaponIndex)
>         equipWeapon hand heroIndex weaponIndex = do
>             setPlayerState playerId playerState {
>                 dungeonEffectsStats = doEquip stats heroIndex weaponIndex
>                 }
>             return [ThunderstoneEventEquip playerId
>                                            (hand !! heroIndex)
>                                            (hand !! weaponIndex)]
>         doEquip stats heroIndex weaponIndex =
>             updateIndex heroIndex
>                         (\ stat -> stat {
>                                 dungeonPartyEquippedWith = Just weaponIndex
>                                 })
>                 $ updateIndex weaponIndex
>                         (\ stat -> stat {
>                               dungeonPartyEquippedBy = Just heroIndex,
>                               dungeonPartyNotAttacking =
>                                   dungeonPartyNotAttacking
>                                       (stats !! heroIndex)
>                               }) stats

>     performAction
>             UsingDungeonEffects {
>                 dungeonEffectsUsed = used,
>                 dungeonEffectsStats = stats
>                 }
>             (ChooseOption (index,description,_)) = do
>         hand <- getHand playerId
>         if index < 0 || index >= length hand
>                      || description /= show (hand !! index)
>           then return Nothing
>           else do
>             setPlayerState playerId UsingDungeonEffectsCard {
>                 dungeonEffectsCardIndex = index,
>                 dungeonEffectsUsed = used,
>                 dungeonEffectsStats = stats
>                 }
>             return (Just [])

>     performAction
>             UsingDungeonEffects {
>                 dungeonEffectsStats = stats
>                 }
>             AttackMonster = do
>         hand <- getHand playerId
>         attackMonster (zip hand stats)

>     performAction UsingDungeonEffects {} _ = return Nothing

>     performAction
>             UsingDungeonEffectsCard {
>                 dungeonEffectsCardIndex = cardIndex,
>                 dungeonEffectsUsed = used,
>                 dungeonEffectsStats = stats
>                 }
>             (ChooseOption (index,description,_)) = do
>         hand <- getHand playerId
>         if index < 0 || index >= length (effects hand) || effectUsed
>           then
>             return Nothing
>           else
>             effect hand playerId cardIndex markCardUsed markEffectUsed
>       where
>         effects hand = cardDungeonEffects (hand !! cardIndex)
>         effectText hand = fst (effects hand !! index)
>         effect hand = snd (effects hand !! index)
>         effectUsed = maybe False (!! index) (used !! cardIndex)
>         markCardUsed = do
>             playerState <- getPlayerState playerId
>             hand <- getHand playerId
>             let cardIndex = dungeonEffectsCardIndex playerState
>             let effects = cardDungeonEffects (hand !! cardIndex)
>             maybe (setPlayerState playerId playerState {
>                        dungeonEffectsUsed =
>                            setIndex cardIndex
>                                     (dungeonEffectsUsed playerState)
>                                     (Just $ replicate (length effects) False)
>                     })
>                   (const (return ()))
>                   (dungeonEffectsUsed playerState !! cardIndex)
>         markEffectUsed = do
>             playerState <- getPlayerState playerId
>             hand <- getHand playerId
>             let cardIndex = dungeonEffectsCardIndex playerState
>             let effects = cardDungeonEffects (hand !! cardIndex)
>             setPlayerState playerId playerState {
>                 dungeonEffectsUsed =
>                     setIndex cardIndex
>                              (dungeonEffectsUsed playerState)
>                              (markUsed (length effects)
>                                   (dungeonEffectsUsed playerState
>                                    !! cardIndex))
>                 }
>           where
>             markUsed numberOfEffects Nothing =
>                 Just $ setIndex index (replicate numberOfEffects False) True
>             markUsed _ (Just flags) = Just $ setIndex index flags True

>     performAction
>             UsingDungeonEffectsCard {
>                 dungeonEffectsUsed = used,
>                 dungeonEffectsStats = stats
>                 }
>             FinishUsingCard = do
>         setPlayerState playerId UsingDungeonEffects {
>             dungeonEffectsUsed = used,
>             dungeonEffectsStats = stats
>             }
>         return (Just [])

>     performAction
>             UsingDungeonEffectsCard {
>                 dungeonEffectsStats = stats
>                 }
>             AttackMonster = do
>         hand <- getHand playerId
>         attackMonster (zip hand stats)

>     performAction UsingDungeonEffectsCard {} _ = return Nothing

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
>             ((optionNumber,show card,Nothing),destroyCard card)
>         destroyCard card = do
>             hand <- getHand playerId
>             setHand playerId (remove1 card hand)
>             events <- endTurn
>                         [ThunderstoneEventPlayerAction playerId DestroyCard]
>             maybe (return []) return events
>         backout =
>             setPlayerState playerId Resting

>     performAction Resting EndTurn = do
>         endTurn []

>     performAction Resting _ = return Nothing

Discarding as nonactive player:

>     performAction playerState@DiscardingCards {
>                     discardingCardsSavedState = savedState,
>                     discardingCards = cards,
>                     discardingCardCount = count,
>                     discardingCardsDone = discardingDone
>                     } (ChooseOption (index,description,_)) = do
>         hand <- getHand playerId
>         let card = hand !! index
>         if index < 0 || index >= length hand
>                      || description /= show card
>           then return Nothing
>           else do
>             setPlayerState playerId savedState
>             destroyIndex playerId index
>             newHand <- getHand playerId
>             if null newHand || length cards + 1 >= count
>               then do
>                 discardEvents <- discardingDone (playerId,card:cards)
>                 return (Just (ThunderstoneEventDiscard playerId card
>                               : discardEvents))
>               else do
>                 newSavedState <- getPlayerState playerId
>                 setPlayerState playerId playerState {
>                     discardingCardsSavedState = newSavedState,
>                     discardingCards = card:cards
>                     }
>                 return (Just [ThunderstoneEventDiscard playerId card])

>     performAction DiscardingCards {} _ = return Nothing

>     performAction playerState@DiscardingHero {
>                     discardingCardsSavedState = savedState,
>                     discardingCardsDone = discardingDone
>                     } (ChooseOption (index,description,_)) = do
>         hand <- getHand playerId
>         let card = hand !! index
>         if index < 0 || index >= length hand
>                      || description /= show card
>                      || not (isHero card)
>           then return Nothing
>           else do
>             setPlayerState playerId savedState
>             destroyIndex playerId index
>             discardEvents <- discardingDone (playerId,[card])
>             return (Just (ThunderstoneEventDiscard playerId card
>                           : discardEvents))

>     performAction playerState@DiscardingTwoCardsOrOneHero {
>                     discardingCardsSavedState = savedState,
>                     discardingCardsDone = discardingDone
>                     } (ChooseOption (index,description,_)) = do
>         hand <- getHand playerId
>         let card = hand !! index
>         if index < 0 || index >= length hand
>                      || description /= show card
>                      || not (isHero card)
>           then return Nothing
>           else do
>             setPlayerState playerId savedState
>             destroyIndex playerId index
>             if isHero card || length hand == 1
>               then do
>                 discardEvents <- discardingDone (playerId,[card])
>                 return (Just (ThunderstoneEventDiscard playerId card
>                               : discardEvents))
>               else do
>                 newSavedState <- getPlayerState playerId
>                 setPlayerState playerId DiscardingCards {
>                     discardingCardsSavedState = newSavedState,
>                     discardingCards = [card],
>                     discardingCardCount = 2,
>                     discardingCardsDone = discardingDone
>                     }
>                 return (Just [ThunderstoneEventDiscard playerId card])

Generic choose option:

>     performAction (ChoosingOption _ options _) (ChooseOption option) = do
>         maybe (return Nothing) (fmap Just) $ lookup option options

>     performAction (ChoosingOption _ _ (Just backout)) Backout = do
>         backout
>         return (Just [])

>     performAction (ChoosingOption _ _ _) _ = return Nothing

>     attackMonster :: [(Card,DungeonPartyStats)]
>                   -> Thunderstone (Maybe [ThunderstoneEvent])
>     attackMonster stats = do
>         dungeon <- getDungeon
>         playerState <- getPlayerState playerId
>         let monsterOptions = filter (canAttack stats) (zip [1..3] dungeon)
>         if null monsterOptions
>           then
>             setPlayerState playerId Waiting
>           else
>             setPlayerState playerId
>                 (ChoosingOption
>                      WhichMonster
>                      (map (chooseOption playerState) monsterOptions)
>                      (Just (setPlayerState playerId playerState)))
>         return (Just [])
>       where
>         chooseOption playerState (rank,monster) =
>             ((rank,show monster,Nothing),do
>                 setPlayerState playerId playerState
>                 battleEvents <- battleMonster playerId (rank,monster)
>                                               (battleResolved rank monster)
>                 return (ThunderstoneEventAttack playerId rank monster
>                         : battleEvents))
>         battleResolved rank monster win events
>           | not win = do
>                 dungeon <- getDungeon
>                 setDungeon (removeIndex (rank - 1) dungeon ++ [monster])
>                 resolveBattleBreachEffects
>                     False rank
>                     (events
>                      ++ [ThunderstoneEventLoseBattle playerId rank monster,
>                          ThunderstoneEventDungeonHallChanged])
>           | otherwise = do
>                 discard playerId [monster]
>                 xp <- getXP playerId
>                 setXP playerId (maybe xp (xp +) (cardXPValue monster))
>                 dungeon <- getDungeon
>                 setDungeon (removeIndex (rank - 1) dungeon)
>                 let canTakeThunderstone =
>                         rank == 1 && isThunderstone (dungeon !! 1)
>                 hand <- getHand playerId
>                 stats <- fmap dungeonEffectsStats $ getPlayerState playerId
>                 let survivingHand =
>                         map fst $ filter (not . dungeonPartyDestroyed . snd)
>                                 $ zip hand stats
>                 resolvePostBattleEffects
>                     (concatMap (spoilsEffect playerId survivingHand False)
>                                survivingHand
>                         ++ spoilsEffect playerId survivingHand True monster)
>                     (resolveBattleBreachEffects canTakeThunderstone rank)
>                     (events
>                      ++ [ThunderstoneEventWinBattle playerId rank monster,
>                          ThunderstoneEventGainDungeonCard
>                                 playerId rank (dungeon !! (rank - 1)),
>                          ThunderstoneEventDungeonHallChanged])
>         resolvePostBattleEffects effects resolveBreach events
>           | null effects = resolveBreach events
>           | otherwise =
>                 head effects events
>                      (resolvePostBattleEffects (tail effects) resolveBreach)
>         resolveBattleBreachEffects canTakeThunderstone rank events = do
>             breachEvents <- if rank == 1
>               then triggerBreachEffects
>                        (battleBreachResolved canTakeThunderstone)
>               else battleBreachResolved canTakeThunderstone
>             return (events ++ breachEvents)
>         battleBreachResolved canTakeThunderstone = do
>             disbandDungeonParty playerId
>             events <- if not canTakeThunderstone
>               then return []
>               else do
>                 dungeon <- getDungeon
>                 setDungeon (drop 1 dungeon)
>                 discard playerId [dungeon !! 0]
>                 return [ThunderstoneEventGainDungeonCard
>                             playerId 1 (dungeon !! 0)]
>             fmap (maybe (error "battleBreachResolved/endTurn") id)
>                  (endTurn events)

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
>                 result <- drawVillage villageCard
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

> initDungeonPartyStats :: Card -> DungeonPartyStats
> initDungeonPartyStats card = DungeonPartyStats {
>     dungeonPartyEquippedWith = Nothing,
>     dungeonPartyEquippedBy = Nothing,
>     dungeonPartyStrength = cardHeroStrength card,
>     dungeonPartyAttack = cardDungeonAttack card,
>     dungeonPartyMagicAttack = cardDungeonMagicAttack card,
>     dungeonPartyLight = cardDungeonLight card,
>     dungeonPartyWeight = cardWeaponWeight card,
>     dungeonPartyNotAttacking = card `hasClass` ClassWeapon,
>     dungeonPartyDestroyed = False,
>     dungeonPartyBorrowedFrom = Nothing
>     }

> availableEquippings :: [(Card,DungeonPartyStats)] -> [(Int,Int)]
> availableEquippings party =
>     [(index1,index2)
>      | (index1,(card1,stats1)) <- zip [0..] party,
>        (index2,(card2,stats2)) <- zip [0..] party,
>        isNothing (dungeonPartyEquippedWith stats1),
>        isNothing (dungeonPartyEquippedBy stats2),
>        isHero card1, card2 `hasClass` ClassWeapon,
>        dungeonPartyStrength stats1 >= dungeonPartyWeight stats2]

> disbandDungeonParty :: PlayerId -> Thunderstone ()
> disbandDungeonParty playerId = do
>     hand <- getHand playerId
>     stats <- fmap dungeonEffectsStats (getPlayerState playerId)
>     setHand playerId $ map fst $ filter stillInHand $ zip hand stats
>     setPlayerState playerId Waiting
>     mapM_ returnBorrowedCards $ zip hand stats
>   where
>     stillInHand (_,stats) =
>         not (dungeonPartyDestroyed stats)
>             && isNothing (dungeonPartyBorrowedFrom stats)
>     returnBorrowedCards (card,stats) =
>         maybe (return ())
>               (flip discard [card])
>               (dungeonPartyBorrowedFrom stats)

> triggerBreachEffects :: Thunderstone [ThunderstoneEvent]
>                      -> Thunderstone [ThunderstoneEvent]
> triggerBreachEffects breachResolved = do
>     card:_ <- getDungeon
>     state <- getState
>     if card `elem` thunderstoneBreached state
>       then breachResolved
>       else do
>         setState state {
>             thunderstoneBreached = card : thunderstoneBreached state
>             }
>         let breachEffects = cardBreachEffects card breachResolved
>         if null breachEffects
>           then breachResolved
>           else fmap concat $ sequence breachEffects

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

> getDungeon :: Thunderstone [Card]
> getDungeon = fmap thunderstoneDungeon getState

> setDungeon :: [Card] -> Thunderstone ()
> setDungeon dungeon = do
>     state <- getState
>     setState state { thunderstoneDungeon = dungeon }

> getVillage :: Thunderstone [(VillageCard,Int)]
> getVillage = fmap thunderstoneVillage getState

> getVillageCount :: VillageCard -> Thunderstone Int
> getVillageCount card = fmap (maybe 0 id . lookup card) getVillage

> setVillageCount :: VillageCard -> Int -> Thunderstone ()
> setVillageCount card count = do
>     state <- getState
>     let village = thunderstoneVillage state
>     setState state {
>         thunderstoneVillage = map setCount (thunderstoneVillage state)
>         }
>   where
>     setCount village@(villageCard,_)
>       | villageCard == card = (villageCard,count)
>       | otherwise = village

> drawVillage :: VillageCard -> Thunderstone (Maybe VillageCard)
> drawVillage card = do
>     count <- getVillageCount card
>     if count > 0
>       then do
>         setVillageCount card (count - 1)
>         return (Just card)
>       else
>         return Nothing

> topHero :: HeroType -> Thunderstone (Maybe HeroCard)
> topHero heroType = do
>     state <- getState
>     let maybeStack = lookup heroType (thunderstoneHeroes state)
>     return (maybe Nothing listToMaybe maybeStack)

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
>       | index == cardIndex =
>         UsingVillageEffects {
>             villageEffectsUsed = removeIndex index used,
>             villageEffectsNumberOfBuys = numberOfBuys,
>             villageEffectsGold = gold
>             }
>       | otherwise =
>         playerState {
>             villageEffectsCardIndex = reduceCardIndex index cardIndex,
>             villageEffectsUsed = removeIndex index used
>             }
>     destroyIndexInState playerState@UsingDungeonEffects {
>             dungeonEffectsUsed = used,
>             dungeonEffectsStats = stats
>             } =
>         playerState {
>             dungeonEffectsUsed = removeIndex index used,
>             dungeonEffectsStats =
>                 removeIndex index (map (reduceEquipIndex index) stats)
>             }
>     destroyIndexInState playerState@UsingDungeonEffectsCard {
>             dungeonEffectsCardIndex = cardIndex,
>             dungeonEffectsUsed = used,
>             dungeonEffectsStats = stats
>             }
>       | index == cardIndex =
>         UsingDungeonEffects {
>             dungeonEffectsUsed = removeIndex index used,
>             dungeonEffectsStats =
>                 removeIndex index (map (reduceEquipIndex index) stats)
>             }
>       | otherwise =
>         playerState {
>             dungeonEffectsCardIndex = reduceCardIndex index cardIndex,
>             dungeonEffectsUsed = removeIndex index used,
>             dungeonEffectsStats =
>                 removeIndex index (map (reduceEquipIndex index) stats)
>             }
>     destroyIndexInState playerState = playerState
>     reduceCardIndex deletedIndex cardIndex
>       | cardIndex > deletedIndex = cardIndex - 1
>       | otherwise = cardIndex
>     reduceEquipIndex deletedIndex stats =
>         stats {
>             dungeonPartyEquippedWith =
>                 reduceIndex (dungeonPartyEquippedWith stats),
>             dungeonPartyEquippedBy =
>                 reduceIndex (dungeonPartyEquippedBy stats)
>             }
>       where
>         reduceIndex Nothing = Nothing
>         reduceIndex (Just i)
>           | i < deletedIndex = Just i
>           | i == deletedIndex = Nothing
>           | i > deletedIndex = Just (i - 1)

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

> updateIndex :: Int -> (a -> a) -> [a] -> [a]
> updateIndex index update list =
>     take index list ++ update (list !! index) : drop (index+1) list

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
> cardVillageEffects card =
>     concatMap (getVillageEffect card) (cardCardText card)

> cardDungeonEffects :: Card -> [DungeonEffect]
> cardDungeonEffects card =
>     concatMap (getDungeonEffect card) (cardCardText card)

> cardCardText :: Card -> [String]
> cardCardText (MonsterCard card) = cardText $ monsterDetails card
> cardCardText (HeroCard card) = cardText $ heroDetails card
> cardCardText (VillageCard card) = cardText $ villageDetails card
> cardCardText DiseaseCard =
>     cardText $ diseaseDetails ThunderstoneCards.Disease
> cardCardText (ThunderstoneCard card) = cardText $ thunderstoneDetails card

> cardHeroStrength :: Card -> Int
> cardHeroStrength (HeroCard card) =
>     maybe 0 id (cardStrength $ heroDetails card)
> cardHeroStrength _ = 0

> cardMonsterHealth :: Card -> Int
> cardMonsterHealth (MonsterCard card) =
>     maybe 0 id (cardHealth $ monsterDetails card)
> cardMonsterHealth _ = 0

> cardDungeonAttack :: Card -> Int
> cardDungeonAttack card = sum $ map getAttack $ cardCardText card
>   where
>     getAttack "ATTACK +1" = 1
>     getAttack "ATTACK +2" = 2
>     getAttack "ATTACK +2, or ATTACK +6 when attached to a Hero with 8 or more Strength." = 2
>     getAttack "ATTACK +3" = 3
>     getAttack "ATTACK +4" = 4
>     getAttack "ATTACK +5" = 5
>     getAttack "ATTACK +6" = 6
>     getAttack "ATTACK +7" = 7
>     getAttack "* ATTACK +1" = 1
>     getAttack "* ATTACK +2" = 2
>     getAttack "* ATTACK +3" = 3
>     getAttack "* ATTACK -1" = -1
>     getAttack "* ATTACK -2" = -2
>     getAttack "* ATTACK -3" = -3
>     getAttack _ = 0

> cardDungeonMagicAttack :: Card -> Int
> cardDungeonMagicAttack card = sum $ map getAttack $ cardCardText card
>   where
>     getAttack "MAGIC ATTACK +1" = 1
>     getAttack "MAGIC ATTACK +2" = 2
>     getAttack "MAGIC ATTACK +3" = 3
>     getAttack "MAGIC ATTACK +4" = 4
>     getAttack "* MAGIC ATTACK +1" = 1
>     getAttack "* MAGIC ATTACK +2" = 2
>     getAttack _ = 0

> cardDungeonLight :: Card -> Int
> cardDungeonLight (MonsterCard card) =
>     maybe 0 id $ cardLight $ monsterDetails card
> cardDungeonLight (HeroCard card) =
>     maybe 0 id $ cardLight $ heroDetails card
> cardDungeonLight (VillageCard card) =
>     maybe 0 id $ cardLight $ villageDetails card
> cardDungeonLight _ = 0

> cardWeaponWeight :: Card -> Int
> cardWeaponWeight (VillageCard card) =
>     maybe 0 id $ cardWeight $ villageDetails card
> cardWeaponWeight _ = 0

> cardXPValue :: Card -> Maybe Int
> cardXPValue (MonsterCard card) = cardXP $ monsterDetails card
> cardXPValue (HeroCard card) = cardXP $ heroDetails card
> cardXPValue (VillageCard card) = cardXP $ villageDetails card
> cardXPValue DiseaseCard = Nothing
> cardXPValue (ThunderstoneCard card) = cardXP $ thunderstoneDetails card

Dungeon/Village Effect:

markCardUsed or markEffectUsed must be called before modifying the
hand (such as when destroying, discarding, or drawing a card).

Repeat effects call markCardUsed, but do not call markEffectUsed so
the effect can be used again.

Non-repeat effects call markEffectUsed, which also does markCardUsed.

> type DungeonEffect =
>     (String,PlayerId -> Int                   -- cardIndex
>                      -> Thunderstone ()       -- markCardUsed
>                      -> Thunderstone ()       -- markEffectUsed
>                      -> Thunderstone (Maybe [ThunderstoneEvent]))
> type VillageEffect =
>     (String,PlayerId -> Int                   -- cardIndex
>                      -> Thunderstone ()       -- markCardUsed
>                      -> Thunderstone ()       -- markEffectUsed
>                      -> Thunderstone (Maybe [ThunderstoneEvent]))

> type BattleEffect = (String,PlayerId -> Thunderstone ())
> type BattleResult = (String,PlayerId -> Thunderstone Bool)
> type SpoilsEffect = (String,PlayerId -> Thunderstone ())
> type BreachEffect = Maybe (String,Thunderstone ())

> getVillageEffect :: Card -> String -> [VillageEffect]
> getVillageEffect card text

> -- Barkeep
>   | text == "VILLAGE: You may purchase one additional card this turn." =
>         [(text, \ playerId cardIndex markCardUsed markEffectUsed -> do
>             markEffectUsed
>             playerState <- getPlayerState playerId
>             setPlayerState playerId playerState {
>                 villageEffectsNumberOfBuys =
>                     villageEffectsNumberOfBuys playerState + 1
>                 }
>             return (Just [ThunderstoneEventUseEffect playerId card text]))]

> -- Trainer
>   | text == "VILLAGE: Destroy one Militia to gain 2 XP." =
>         [(text, \ playerId cardIndex markCardUsed markEffectUsed -> do
>             hand <- getHand playerId
>             case filter ((== (HeroCard Militia)) . snd) (zip [0..] hand) of
>               (militiaIndex,_):_ -> do
>                 markEffectUsed
>                 destroyIndex playerId militiaIndex
>                 xp <- getXP playerId
>                 setXP playerId (xp + 2)
>                 return (Just [ThunderstoneEventUseEffect playerId card text])
>               _ -> -- no Militia
>                 return Nothing)]

> -- Trainer
>   | text == "VILLAGE: Destroy this card to gain 2 Gold." =
>         [(text, \ playerId cardIndex markCardUsed markEffectUsed -> do
>             markEffectUsed
>             destroyIndex playerId cardIndex
>             playerState <- getPlayerState playerId
>             setPlayerState playerId playerState {
>                 villageEffectsGold = villageEffectsGold playerState + 2
>                 }
>             return (Just [ThunderstoneEventUseEffect playerId card text]))]

> -- Pawnbroker
>   | text == "VILLAGE: Destroy any card with a gold value to gain its "
>                 ++ "gold value plus 3 Gold." =
>         [(text, \ playerId cardIndex markCardUsed markEffectUsed -> do
>             playerState <- getPlayerState playerId
>             hand <- getHand playerId
>             let backout = setPlayerState playerId playerState
>             let used = villageEffectsUsed playerState
>             let eligible ((index,_,_),_) =
>                     cardGoldValue (hand !! index) /= Nothing
>                         && isNothing (used !! index)
>             let chooseCard index chosenCard =
>                     ((index,show chosenCard,Nothing),do
>                         setPlayerState playerId playerState {
>                              villageEffectsGold =
>                                      villageEffectsGold playerState
>                                          + cardVillageGold chosenCard + 3
>                              }
>                         markEffectUsed
>                         destroyIndex playerId index
>                         return [ThunderstoneEventUseEffect
>                                     playerId card text,
>                                 ThunderstoneEventDestroyCard
>                                     playerId chosenCard])
>             setPlayerState playerId
>                 (ChoosingOption WhichCardToDestroy
>                          (filter eligible $ zipWith chooseCard [0..] hand)
>                          (Just (setPlayerState playerId playerState)))
>             return (Just []))]

> -- Town Guard
>   | text == "VILLAGE: Draw two cards." =
>         [(text, \ playerId cardIndex markCardUsed markEffectUsed -> do
>             markEffectUsed
>             deckSize <- fmap (length . playerDeck) (getPlayer playerId)
>             cards <- multiple 2 $ drawCard playerId
>             hand <- getHand playerId
>             setHand playerId (hand ++ cards)
>             playerState <- getPlayerState playerId
>             setPlayerState playerId playerState {
>                 villageEffectsUsed =
>                     villageEffectsUsed playerState
>                         ++ replicate (length cards) Nothing
>                 }
>             return (Just ([ThunderstoneEventUseEffect playerId card text,
>                            ThunderstoneEventDrawCards playerId cards]
>                           ++ (if deckSize < 2
>                                 then [ThunderstoneEventReshuffle playerId]
>                                 else []))))]

> -- Town Guard
>   | text == "VILLAGE: Destroy this card to draw three additional cards." =
>         [(text, \ playerId cardIndex markCardUsed markEffectUsed -> do
>             markEffectUsed
>             destroyIndex playerId cardIndex
>             deckSize <- fmap (length . playerDeck) (getPlayer playerId)
>             cards <- multiple 3 $ drawCard playerId
>             hand <- getHand playerId
>             setHand playerId (hand ++ cards)
>             playerState <- getPlayerState playerId
>             setPlayerState playerId playerState {
>                 villageEffectsUsed =
>                     villageEffectsUsed playerState
>                         ++ replicate (length cards) Nothing
>                 }
>             return (Just ([ThunderstoneEventUseEffect playerId card text,
>                            ThunderstoneEventDrawCards playerId cards]
>                           ++ (if deckSize < 3
>                                 then [ThunderstoneEventReshuffle playerId]
>                                 else []))))]

>   | otherwise = []

> getDungeonEffect :: Card -> String -> [DungeonEffect]
> getDungeonEffect card text

> -- Chalice Quester, Chalice Defender, Regian Cleric, Regian Priest,
> -- Regian Bishop
>   | text == "REPEAT DUNGEON: Destroy one Disease to draw one card." =
>         [(text,\ playerId cardIndex markCardUsed markEffectUsed -> do
>             hand <- getHand playerId
>             case filter ((== DiseaseCard) . snd) (zip [0..] hand) of
>               (diseaseIndex,_):_ -> do
>                 markCardUsed
>                 destroyIndex playerId diseaseIndex
>                 deckSize <- fmap (length . playerDeck) (getPlayer playerId)
>                 drawnCard <- dungeonEffectsDrawCards playerId 1
>                 return (Just ([ThunderstoneEventUseEffect playerId card text,
>                                ThunderstoneEventDrawCards playerId drawnCard]
>                               ++ (if deckSize < 1
>                                     then [ThunderstoneEventReshuffle
>                                               playerId]
>                                     else [])))
>               _ -> -- no Disease
>                 return Nothing)]

> -- Chalice Defender
>   | text == "DUNGEON: ATTACK +1 for each Item that produces Light." =
>         [(text,\ playerId cardIndex markCardUsed markEffectUsed -> do
>             markEffectUsed
>             hand <- getHand playerId
>             let isLightItem card =
>                     card `hasClass` ClassItem && card `hasClass` ClassLight
>             let addAttack stats = stats {
>                     dungeonPartyAttack = dungeonPartyAttack stats
>                         + (length $ filter isLightItem hand)
>                     }
>             dungeonEffectsUpdateStats playerId cardIndex addAttack
>             return (Just [ThunderstoneEventUseEffect playerId card text]))]

> -- Chalice Defender, Chalice Paladin, Regian Priest
>   | text == "DUNGEON: Draw one card." =
>         [(text,\ playerId cardIndex markCardUsed markEffectUsed -> do
>             markEffectUsed
>             deckSize <- fmap (length . playerDeck) (getPlayer playerId)
>             drawnCard <- dungeonEffectsDrawCards playerId 1
>             return (Just ([ThunderstoneEventUseEffect playerId card text,
>                            ThunderstoneEventDrawCards playerId drawnCard]
>                           ++ (if deckSize < 1
>                                 then [ThunderstoneEventReshuffle playerId]
>                                 else []))))]

> -- Elf Archmage
>   | text == "DUNGEON: You may return one Monster to the bottom of "
>               ++ "the deck and refill the hall before the beginning of "
>               ++ "a battle." =
>         [(text,\ playerId cardIndex markCardUsed markEffectUsed -> do
>             playerState <- getPlayerState playerId
>             dungeon <- getDungeon
>             let showOption index = "Rank " ++ show (index + 1)
>                                            ++ ": " ++ show (dungeon !! index)
>             let chooseMonster (index,monsterCard) =
>                     ((index,showOption index,Nothing),do
>                          setPlayerState playerId playerState
>                          markEffectUsed
>                          setDungeon (removeIndex index dungeon
>                                      ++ [monsterCard])
>                          breachEvents <- if index == 0
>                             then triggerBreachEffects (return [])
>                             else return []
>                          return ([ThunderstoneEventUseEffect
>                                       playerId card text,
>                                   ThunderstoneEventReturnMonster
>                                       (index + 1) monsterCard,
>                                   ThunderstoneEventDungeonHallChanged]
>                                  ++ breachEvents))
>             -- With expansions, Guardians would also be returnable, but
>             -- Thunderstones, Traps, and Treasures would not.
>             let returnable (_,MonsterCard _) = True
>                 returnable _ = False
>             setPlayerState playerId
>                 (ChoosingOption WhichMonster
>                     (map chooseMonster $ filter returnable
>                                        $ zip [0..2] dungeon)
>                     (Just (setPlayerState playerId playerState)))
>             return (Just []))]

> -- Lorigg Rogue, Lorigg Outlaw, Redblade Poisoner
>   | text == "DUNGEON: All other players discard one card." =
>         [(text,\ playerId cardIndex markCardUsed markEffectUsed -> do
>             markEffectUsed
>             playerState <- getPlayerState playerId
>             setPlayerState playerId WaitingForDiscards {
>                 waitingForDiscards = [],
>                 waitingForDiscardsDone =
>                     const (setPlayerState playerId playerState >> return [])
>                 }
>             playerIds <- getPlayerIds
>             mapM_ (\ otherPlayerId -> do
>                           savedState <- getPlayerState otherPlayerId
>                           setPlayerState otherPlayerId DiscardingCards {
>                               discardingCardsSavedState = savedState,
>                               discardingCards = [],
>                               discardingCardCount = 1,
>                               discardingCardsDone = (\ (playerId,cards) ->
>                                   discard playerId cards >> return [])
>                               })
>                   (filter (/= playerId) playerIds)
>             return (Just [ThunderstoneEventUseEffect playerId card text]))]

> -- Outlands Warrior
>   | text == "DUNGEON: Destroy one Food for an additional ATTACK +3." =
>         [(text,\ playerId cardIndex markCardUsed markEffectUsed -> do
>             let updateStrength stats = stats {
>                     dungeonPartyAttack = dungeonPartyAttack stats + 3
>                     }
>             let choose foodIndex = do
>                     markEffectUsed
>                     dungeonEffectsUpdateStats
>                         playerId cardIndex updateStrength
>                     destroyIndex playerId foodIndex
>                     return [ThunderstoneEventUseEffect playerId card text]
>             let available (card,_,used) =
>                     card `hasClass` ClassFood && isNothing used
>             dungeonEffectsChooseCard
>                 playerId available WhichCardToUse choose True)]

> -- Outlands Slayer
>   | text == "DUNGEON: Gain +1 ATTACK for each Monster card revealed "
>                 ++ "from your hand." =
>         [(text,\ playerId cardIndex markCardUsed markEffectUsed -> do
>             markEffectUsed
>             hand <- getHand playerId
>             let addAttack stats = stats {
>                     dungeonPartyAttack = dungeonPartyAttack stats
>                         + (length $ filter isMonster hand)
>                     }
>             dungeonEffectsUpdateStats playerId cardIndex addAttack
>             return (Just [ThunderstoneEventUseEffect playerId card text]))]

> -- Outlands Slayer
>   | text == "REPEAT DUNGEON: Destroy one Food for an additional ATTACK +3." =
>         [(text,\ playerId cardIndex markCardUsed markEffectUsed -> do
>             let updateStrength stats = stats {
>                     dungeonPartyAttack = dungeonPartyAttack stats + 3
>                     }
>             let choose foodIndex = do
>                     markCardUsed
>                     dungeonEffectsUpdateStats
>                         playerId cardIndex updateStrength
>                     destroyIndex playerId foodIndex
>                     return [ThunderstoneEventUseEffect playerId card text]
>             let available (card,_,used) =
>                     card `hasClass` ClassFood && isNothing used
>             dungeonEffectsChooseCard
>                 playerId available WhichCardToUse choose True)]

> -- Outlands Khan
>   | text == "DUNGEON: ATTACK +2 for each Monster card revealed from "
>                 ++ "your hand." =
>         [(text,\ playerId cardIndex markCardUsed markEffectUsed -> do
>             markEffectUsed
>             hand <- getHand playerId
>             let addAttack stats = stats {
>                     dungeonPartyAttack = dungeonPartyAttack stats
>                         + 2*(length $ filter isMonster hand)
>                     }
>             dungeonEffectsUpdateStats playerId cardIndex addAttack
>             return (Just [ThunderstoneEventUseEffect playerId card text]))]

> -- Redblade Assassin
>   | text == "DUNGEON: All other players discard one Hero or two cards." =
>         [(text,\ playerId cardIndex markCardUsed markEffectUsed -> do
>             markEffectUsed
>             playerState <- getPlayerState playerId
>             setPlayerState playerId WaitingForDiscards {
>                 waitingForDiscards = [],
>                 waitingForDiscardsDone =
>                     const (setPlayerState playerId playerState >> return [])
>                 }
>             playerIds <- getPlayerIds
>             mapM_ (\ otherPlayerId -> do
>                           savedState <- getPlayerState otherPlayerId
>                           setPlayerState otherPlayerId
>                               DiscardingTwoCardsOrOneHero {
>                                   discardingCardsSavedState = savedState,
>                                   discardingCardsDone = (\(playerId,cards) ->
>                                       discard playerId cards >> return [])
>                                   })
>                   (filter (/= playerId) playerIds)
>             return (Just [ThunderstoneEventUseEffect playerId card text]))]

> -- Regian Bishop
>   | text == "DUNGEON: Draw two cards." =
>         [(text,\ playerId cardIndex markCardUsed markEffectUsed -> do
>             markEffectUsed
>             deckSize <- fmap (length . playerDeck) (getPlayer playerId)
>             drawnCards <- dungeonEffectsDrawCards playerId 2
>             return (Just ([ThunderstoneEventUseEffect playerId card text,
>                            ThunderstoneEventDrawCards playerId drawnCards]
>                           ++ (if deckSize < 2
>                                 then [ThunderstoneEventReshuffle playerId]
>                                 else []))))]

> -- Selurin Theurge
>   | text == "DUNGEON: Each player discards one Hero or shows they "
>                 ++ "have none.  You may borrow one of those discarded "
>                 ++ "Heroes for the battle, returning it at the end." =
>         [(text,\ playerId cardIndex markCardUsed markEffectUsed -> do
>             playerState <- getPlayerState playerId
>             hand <- getHand playerId
>             -- player chooses hero to discard
>             -- if player has no hero, continue
>             -- when selection is made:
>             --     markEffectUsed
>             --     set state to WaitingForDiscards associating discarded hero
>             --         with playerId in waitingForDiscards alist
>             --     set other player state to DiscardingHero, where
>             --         discardingCardsDone removes the hero from other
>             --         player's hand and associates the hero with
>             --         the other player's playerId in waitingForDiscards
>             --         alist
>             --
>             --     return ThunderstoneEventUseEffect, plus
>             --         ThunderstoneEventRevealCards for other players that
>             --         have no heroes
>             -- waitingForDiscardsDone: player chooses hero to borrow
>             -- when selection is made:
>             --     add all unselected heroes to their player's discard
>             --         piles
>             --     return ThunderstoneEventBorrowCard
>             let returnDiscard (otherPlayerId,discards) =
>                     discard otherPlayerId discards
>             let doBorrowCard playerState discards otherCard otherPlayerId =
>                   do
>                     hand <- getHand playerId
>                     setHand playerId (hand ++ [otherCard])
>                     setPlayerState playerId playerState {
>                         dungeonEffectsUsed =
>                             dungeonEffectsUsed playerState ++ [Nothing],
>                         dungeonEffectsStats =
>                             dungeonEffectsStats playerState
>                                 ++ [(initDungeonPartyStats otherCard) {
>                                         dungeonPartyBorrowedFrom =
>                                             Just otherPlayerId
>                                         }]
>                         }
>                     mapM_ returnDiscard
>                           (filter ((/= otherPlayerId) . fst) discards)
>                     return [ThunderstoneEventBorrowCard
>                                 playerId otherCard otherPlayerId]
>             let chooseDiscardToBorrow playerState discards = do
>                     setPlayerState playerId
>                         (ChoosingOption WhichHeroToBorrow
>                             ([((0,"Not borrowing any heroes.",Nothing),do
>                                setPlayerState playerId playerState
>                                mapM_ returnDiscard discards
>                                return [])]
>                              ++ [((index,show otherCard,Just otherPlayerId),
>                                   doBorrowCard playerState discards
>                                                otherCard otherPlayerId)
>                                  | (index,(otherPlayerId,otherCard:_)) <-
>                                        zip [1..] discards])
>                             Nothing)
>                     return []
>             let makeOtherPlayerDiscard otherPlayerId = do
>                     otherHand <- getHand otherPlayerId
>                     if any isHero otherHand
>                       then do
>                         savedState <- getPlayerState otherPlayerId
>                         setPlayerState otherPlayerId DiscardingHero {
>                             discardingCardsSavedState = savedState,
>                             discardingCardsDone = \ otherDiscards -> do
>                                 activePlayerState <- getPlayerState playerId
>                                 setPlayerState playerId activePlayerState {
>                                     waitingForDiscards =
>                                         otherDiscards
>                                             : waitingForDiscards
>                                                             activePlayerState
>                                     }
>                                 return []
>                             }
>                         return Nothing
>                       else
>                         return (Just (ThunderstoneEventRevealCards
>                                           otherPlayerId otherHand))
>             let makeOthersDiscard playerDiscard = do
>                     playerState <- getPlayerState playerId
>                     otherPlayers <- fmap (filter (/= playerId)) getPlayerIds
>                     setPlayerState playerId WaitingForDiscards {
>                         waitingForDiscards = playerDiscard,
>                         waitingForDiscardsDone =
>                             chooseDiscardToBorrow playerState
>                         }
>                     revealedHands <- mapM makeOtherPlayerDiscard otherPlayers
>                     return (Just (catMaybes revealedHands))
>             let makeDiscardHeroOption (heroIndex,heroCard) =
>                     ((heroIndex,show heroCard,Nothing),do
>                         setPlayerState playerId playerState
>                         markEffectUsed
>                         destroyIndex playerId heroIndex
>                         makeOthersDiscard [(playerId,[heroCard])]
>                         return
>                             [ThunderstoneEventUseEffect playerId card text,
>                              ThunderstoneEventDiscard playerId heroCard])
>             if not (any isHero hand)
>               then do
>                 markEffectUsed
>                 makeOthersDiscard []
>                 return (Just [ThunderstoneEventUseEffect playerId card text])
>               else do
>                 setPlayerState playerId
>                     (ChoosingOption WhichCardToDiscard
>                         (map makeDiscardHeroOption $ filter (isHero . snd)
>                                                    $ zip [0..] hand)
>                         (Just (setPlayerState playerId playerState)))
>                 return (Just []))]

> -- Thyrian Squire, Thyrian Knight
>   | text == "DUNGEON: Destroy one Food for additional ATTACK +2." =
>         [(text,\ playerId cardIndex markCardUsed markEffectUsed -> do
>             let updateStrength stats = stats {
>                     dungeonPartyAttack = dungeonPartyAttack stats + 2
>                     }
>             let choose foodIndex = do
>                     markEffectUsed
>                     dungeonEffectsUpdateStats
>                         playerId cardIndex updateStrength
>                     destroyIndex playerId foodIndex
>                     return [ThunderstoneEventUseEffect playerId card text]
>             let available (card,_,used) =
>                     card `hasClass` ClassFood && isNothing used
>             dungeonEffectsChooseCard
>                 playerId available WhichCardToUse choose True)]

> -- Thyrian Lord
>   | text == "DUNGEON: Destroy one Food to place one Monster from "
>                 ++ "the hall worth 1 or 2 VP into your discard pile.  "
>                 ++ "Refill the hall." =
>         [(text,\ playerId cardIndex markCardUsed markEffectUsed -> do
>             -- if no monster worth 1 or 2 VP, invalid
>             -- if no unused Food, invalid
>             -- choose Food to destroy
>             -- when selection is made:
>             --     markEffectUsed
>             --     choose Monster from hall
>             --     return ThunderstoneEventUseEffect,
>             --            ThunderstoneEventDestroyCard
>             --     when selection is made:
>             --         resolve breach effects
>             hand <- getHand playerId
>             playerState <- getPlayerState playerId
>             dungeon <- getDungeon
>             let monsterAvailable (_,MonsterCard monsterCard) =
>                     (cardVictoryPoints $ monsterDetails monsterCard)
>                         `elem` [Just 1,Just 2]
>                 monsterVP _ = False
>             let availableMonsters =
>                     filter monsterAvailable $ zip [1..] $ take 3 dungeon
>             let chooseMonster playerState foodIndex foodCard
>                               (rank,monsterCard) =
>                     ((rank,show card,Nothing),do
>                         setPlayerState playerId playerState
>                         setDungeon (removeIndex (rank - 1) dungeon)
>                         discard playerId [monsterCard]
>                         breachEvents <- if rank /= 1
>                           then return []
>                           else triggerBreachEffects (return [])
>                         return ([ThunderstoneEventDungeonHallChanged,
>                                  ThunderstoneEventGainDungeonCard
>                                     playerId rank monsterCard]
>                                 ++ breachEvents))
>             let foodAvailable (_,card,used,stats) =
>                     card `hasClass` ClassFood
>                         && isNothing used
>                         && not (dungeonPartyDestroyed stats)
>             let availableFood =
>                     filter foodAvailable
>                         $ zip4 [0..] hand
>                                (dungeonEffectsUsed playerState)
>                                (dungeonEffectsStats playerState)
>             let chooseFood playerState (foodIndex,foodCard,_,_) =
>                     ((foodIndex,show foodCard,Nothing),do
>                         setPlayerState playerId playerState
>                         markEffectUsed
>                         destroyIndex playerId foodIndex
>                         playerState <- getPlayerState playerId
>                         setPlayerState playerId
>                             (ChoosingOption WhichCardToDiscard
>                                  (map (chooseMonster playerState
>                                                      foodIndex foodCard)
>                                       availableMonsters)
>                                  Nothing)
>                         return [ThunderstoneEventUseEffect playerId
>                                                            card text,
>                                 ThunderstoneEventDestroyCard playerId
>                                                              foodCard])
>             if null availableMonsters || null availableFood
>               then
>                 return Nothing
>               else do
>                 setPlayerState playerId
>                     (ChoosingOption WhichCardToDestroy
>                         (map (chooseFood playerState) availableFood)
>                         (Just (setPlayerState playerId playerState)))
>                 return (Just []))]

> -- Iron Rations
>   | text == "DUNGEON: One Hero gains Strength +2." =
>         [(text,\ playerId cardIndex markCardUsed markEffectUsed -> do
>             let updateStrength stats = stats {
>                     dungeonPartyStrength = dungeonPartyStrength stats + 2
>                     }
>             let choose heroIndex = do
>                     markEffectUsed
>                     dungeonEffectsUpdateStats
>                         playerId heroIndex updateStrength
>                     return [ThunderstoneEventUseEffect playerId card text]
>             dungeonEffectsChooseCard
>                 playerId (\ (card,_,_) -> isHero card)
>                 WhichCardToUse choose True)]

> -- Arcane Energies
>   | text == "DUNGEON: All ATTACKS from Heroes with Weapons "
>                 ++ "equipped become MAGIC ATTACKS.  Draw one card." =
>         [(text,\ playerId cardIndex markCardUsed markEffectUsed -> do
>             markEffectUsed
>             let updateAttacks stats = stats {
>                     dungeonPartyAttack = 0,
>                     dungeonPartyMagicAttack = dungeonPartyAttack stats
>                                             + dungeonPartyMagicAttack stats
>                     }
>             hand <- getHand playerId
>             partyStats <- fmap dungeonEffectsStats $ getPlayerState playerId
>             sequence_
>                 [dungeonEffectsUpdateStats playerId index updateAttacks
>                  | (index,card,stats) <- zip3 [0..] hand partyStats,
>                    (isHero card
>                     && not (isNothing $ dungeonPartyEquippedWith stats))
>                    || (card `hasClass` ClassWeapon
>                        && not (isNothing $ dungeonPartyEquippedBy stats))]
>             deckSize <- fmap (length . playerDeck) (getPlayer playerId)
>             drawnCard <- dungeonEffectsDrawCards playerId 1
>             return (Just ([ThunderstoneEventUseEffect playerId card text,
>                            ThunderstoneEventDrawCards playerId drawnCard]
>                           ++ (if deckSize < 1
>                                 then [ThunderstoneEventReshuffle playerId]
>                                 else []))))]

> -- Banish
>   | text == "DUNGEON: Return one Monster to the bottom of the "
>                 ++ "deck and refill the hall, or rearrange the hall.  "
>                 ++ "Destroy one card from your hand.  Draw one card." =
>         [(text,\ playerId cardIndex markCardUsed markEffectUsed -> do
>             -- choose return monster or rearrange hall
>             -- when selection is made:
>             --     markEffectUsed
>             --     resolve breach effects
>             --     return ThunderstoneEventUseEffect
>             --     when breach effects resolved:
>             --         choose card to destroy
>             --         return ThunderstoneEventDestroyCard
>             --         when selection is made:
>             --             draw card
>             -- ISSUE: If there are no unused cards, then a used card
>             -- will have to be destroyed.  Can't really check before
>             -- activating the effect, because a breach effect can
>             -- destroy or discard cards.
>             let cardsAvailableToDestroy hand UsingDungeonEffectsCard {
>                         dungeonEffectsUsed = used,
>                         dungeonEffectsStats = stats
>                         } =
>                     let unused = filter isUnused (zip [0..] hand)
>                         isUnused (index,_) =
>                             maybe True (const False) (used !! index)
>                     in  if null unused
>                           then zip [0..] hand
>                           else unused
>             let chooseCardToDestroy playerState (index,cardToDestroy) =
>                     ((index,show cardToDestroy,Nothing),do
>                         setPlayerState playerId playerState
>                         destroyIndex playerId index
>                         deckSize <- fmap (length . playerDeck)
>                                          (getPlayer playerId)
>                         drawnCard <- dungeonEffectsDrawCards playerId 1
>                         return ([ThunderstoneEventDestroyCard
>                                      playerId cardToDestroy,
>                                  ThunderstoneEventDrawCards
>                                      playerId drawnCard]
>                                 ++ (if deckSize < 1
>                                       then [ThunderstoneEventReshuffle
>                                                 playerId]
>                                       else [])))
>             let selectCardToDestroy = do
>                     hand <- getHand playerId
>                     playerState <- getPlayerState playerId
>                     setPlayerState playerId
>                         (ChoosingOption WhichCardToDestroy
>                              (map (chooseCardToDestroy playerState)
>                                   (cardsAvailableToDestroy hand playerState))
>                              Nothing)
>                     return []
>             playerState <- getPlayerState playerId
>             dungeon <- getDungeon
>             let chooseReturnMonster (index,monsterCard) =
>                     ((index,show monsterCard,Nothing),do
>                         setPlayerState playerId playerState
>                         markEffectUsed
>                         playerState <- getPlayerState playerId
>                         setDungeon (removeIndex index dungeon
>                                     ++ [monsterCard])
>                         breachEvents <- if index == 0
>                             then triggerBreachEffects selectCardToDestroy
>                             else selectCardToDestroy
>                         return ([ThunderstoneEventUseEffect
>                                      playerId card text,
>                                  ThunderstoneEventReturnMonster
>                                      (index + 1) monsterCard,
>                                  ThunderstoneEventDungeonHallChanged]
>                                 ++ breachEvents))
>             let showReordering reordering =
>                     concat $ intersperse ","
>                            $ map (show . (dungeon !!)) reordering
>             let chooseRearrange (index,reordering) =
>                     ((index,showReordering reordering,Nothing),do
>                         setPlayerState playerId playerState
>                         markEffectUsed
>                         playerState <- getPlayerState playerId
>                         setDungeon (map (dungeon !!) reordering
>                                     ++ drop (length reordering) dungeon)
>                         breachEvents <- if head reordering /= 0
>                             then triggerBreachEffects selectCardToDestroy
>                             else selectCardToDestroy
>                         return (ThunderstoneEventDungeonHallChanged
>                                 : breachEvents))
>             -- With expansions, Guardians would also be returnable, but
>             -- Thunderstones, Traps, and Treasures would not.
>             let returnable (_,MonsterCard _) = True
>                 returnable _ = False
>             setPlayerState playerId
>                 (ChoosingOption WhichMonster
>                      (map chooseReturnMonster
>                           (filter returnable $ zip [0..2] dungeon)
>                       ++ map chooseRearrange
>                              (zip [3..] $ filter (/= [0..2])
>                                         $ permutations [0..2]))
>                      (Just (setPlayerState playerId playerState)))
>             return (Just []))]

> -- Battle Fury
>   | text == "DUNGEON: All Heroes gain ATTACK +1." =
>         [(text,\ playerId cardIndex markCardUsed markEffectUsed -> do
>             markEffectUsed
>             let addAttack stats = stats {
>                     dungeonPartyAttack = dungeonPartyAttack stats + 1
>                     }
>             hand <- getHand playerId
>             sequence_ [dungeonEffectsUpdateStats playerId index addAttack
>                        | (index,card) <- zip [0..] hand, isHero card]
>             return (Just [ThunderstoneEventUseEffect playerId card text]))]

> -- Feast
>   | text == "DUNGEON: All Heroes gain Strength +3 and ATTACK +1." =
>         [(text,\ playerId cardIndex markCardUsed markEffectUsed -> do
>             markEffectUsed
>             let addGains stats = stats {
>                     dungeonPartyAttack = dungeonPartyAttack stats + 1,
>                     dungeonPartyStrength = dungeonPartyStrength stats + 3
>                     }
>             hand <- getHand playerId
>             sequence_ [dungeonEffectsUpdateStats playerId index addGains
>                        | (index,card) <- zip [0..] hand, isHero card]
>             return (Just [ThunderstoneEventUseEffect playerId card text]))]

> -- Goodberries
>   | text == "DUNGEON: One Hero gains Strength +3 and ATTACK "
>                 ++ "becomes MAGIC ATTACK for that Hero." =
>         [(text,\ playerId cardIndex markCardUsed markEffectUsed -> do
>             let updateAttack stats = stats {
>                     dungeonPartyAttack = 0,
>                     dungeonPartyMagicAttack = dungeonPartyAttack stats
>                                             + dungeonPartyMagicAttack stats
>                     }
>             let updateStrength stats = stats {
>                     dungeonPartyStrength = dungeonPartyStrength stats + 3
>                     }
>             let updateWeapon weaponIndex =
>                     case weaponIndex of
>                       Just index ->
>                         dungeonEffectsUpdateStats playerId index updateAttack
>                       _ ->
>                         return ()
>             let updateWeaponStats heroIndex = do
>                     playerState <- getPlayerState playerId
>                     updateWeapon (dungeonPartyEquippedWith
>                                       ((dungeonEffectsStats playerState)
>                                        !! heroIndex))
>             let choose heroIndex = do
>                     markEffectUsed
>                     dungeonEffectsUpdateStats
>                         playerId heroIndex updateStrength
>                     dungeonEffectsUpdateStats
>                         playerId heroIndex updateAttack
>                     updateWeaponStats heroIndex
>                     return [ThunderstoneEventUseEffect playerId card text]
>             dungeonEffectsChooseCard
>                 playerId (\ (card,_,_) -> isHero card)
>                 WhichCardToUse choose True)]

> -- Magical Aura
>   | text == "DUNGEON: All Weapons become Weight 0.  Draw one card." =
>         [(text,\ playerId cardIndex markCardUsed markEffectUsed -> do
>             markEffectUsed
>             let removeWeight stats = stats {
>                     dungeonPartyWeight = 0
>                     }
>             hand <- getHand playerId
>             sequence_ [dungeonEffectsUpdateStats playerId index removeWeight
>                        | (index,card) <- zip [0..] hand,
>                          card `hasClass` ClassWeapon]
>             deckSize <- fmap (length . playerDeck) (getPlayer playerId)
>             drawnCards <- dungeonEffectsDrawCards playerId 1
>             return (Just ([ThunderstoneEventUseEffect playerId card text,
>                            ThunderstoneEventDrawCards playerId drawnCards]
>                           ++ (if deckSize < 1
>                                 then [ThunderstoneEventReshuffle playerId]
>                                 else []))))]

> -- Spear
>   | text == "DUNGEON: You may Destroy this Spear for an "
>                 ++ "additional ATTACK +3." =
>         [(text,\ playerId cardIndex markCardUsed markEffectUsed -> do
>             markEffectUsed
>             let addAttack stats = stats {
>                     dungeonPartyAttack = dungeonPartyAttack stats + 3,
>                     dungeonPartyDestroyed = True
>                     }
>             dungeonEffectsUpdateStats playerId cardIndex addAttack
>             return (Just [ThunderstoneEventUseEffect playerId card text]))]

>   | otherwise = []

> isHero :: Card -> Bool
> isHero (HeroCard _) = True
> isHero _ = False

> isMonster :: Card -> Bool
> isMonster (MonsterCard _) = True
> isMonster _ = False

> isThunderstone :: Card -> Bool
> isThunderstone (ThunderstoneCard _) = True
> isThunderstone _ = False

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

> dungeonEffectsDrawCards :: PlayerId -> Int -> Thunderstone [Card]
> dungeonEffectsDrawCards playerId numberOfCards = do
>     cards <- multiple 1 $ drawCard playerId
>     hand <- getHand playerId
>     setHand playerId (hand ++ cards)
>     playerState <- getPlayerState playerId
>     setPlayerState playerId playerState {
>         dungeonEffectsUsed = dungeonEffectsUsed playerState
>                              ++ replicate (length cards) Nothing,
>         dungeonEffectsStats = dungeonEffectsStats playerState
>                               ++ map initDungeonPartyStats cards
>                     }
>     return cards

> dungeonEffectsUpdateStats :: PlayerId -> Int
>                           -> (DungeonPartyStats -> DungeonPartyStats)
>                           -> Thunderstone ()
> dungeonEffectsUpdateStats playerId cardIndex update = do
>     playerState <- getPlayerState playerId
>     setPlayerState playerId playerState {
>         dungeonEffectsStats = updateIndex cardIndex update
>                                           (dungeonEffectsStats playerState)
>         }

> dungeonEffectsChooseCard :: PlayerId
>                          -> ((Card,DungeonPartyStats,Maybe [Bool]) -> Bool)
>                          -> PlayerOption
>                          -> (Int -> Thunderstone [ThunderstoneEvent])
>                          -> Bool
>                          -> Thunderstone (Maybe [ThunderstoneEvent])
> dungeonEffectsChooseCard playerId cardFilter playerOption
>                          choiceMade backout = do
>     playerState <- getPlayerState playerId
>     let stats = dungeonEffectsStats playerState
>     let used = dungeonEffectsUsed playerState
>     hand <- getHand playerId
>     if null (filter cardFilter $ zip3 hand stats used)
>       then return Nothing
>       else do
>         setPlayerState playerId
>             (ChoosingOption playerOption
>                 [((index,show card,Nothing),choose index playerState)
>                  | (index,card,cardStats,cardUsed) <-
>                         zip4 [0..] hand stats used,
>                    cardFilter (card,cardStats,cardUsed)]
>                 (if backout
>                    then Just (setPlayerState playerId playerState)
>                    else Nothing))
>         return (Just [])
>   where
>     choose index playerState = do
>         setPlayerState playerId playerState
>         choiceMade index

> cardBreachEffects :: Card -> Thunderstone [ThunderstoneEvent]
>                   -> [Thunderstone [ThunderstoneEvent]]
> cardBreachEffects card breachResolved =
>     catMaybes $ map (getBreachEffect card breachResolved) (cardCardText card)

> getBreachEffect :: Card -> Thunderstone [ThunderstoneEvent] -> String
>                 -> Maybe (Thunderstone [ThunderstoneEvent])
> getBreachEffect card breachResolved text

>   | isThunderstone card = Just $ do
>         state <- getState
>         setState state { thunderstoneGameOver = True }
>         return [ThunderstoneEventBreach card text]

> -- Archduke of Pain
>   | text == "BREACH: Destroy the top two cards from each Hero deck "
>                 ++ "in the Village." = Just $ do
>         let destroyTopTwo (heroType,heroCards) = (heroType,drop 2 heroCards)
>         state <- getState
>         setState state {
>             thunderstoneHeroes = map destroyTopTwo (thunderstoneHeroes state)
>             }
>         return [ThunderstoneEventBreach card text]

> -- Tyxr the Old
>   | text == "BREACH: Each player must discard two cards." = Just $ do
>         playerIds <- getPlayerIds
>         let doneDiscarding (playerId,cards) = do
>                 discard playerId cards
>                 state <- getState
>                 let newState = state {
>                     thunderstoneBreachState =
>                         remove1 playerId (thunderstoneBreachState state)
>                     }
>                 setState newState
>                 if null (thunderstoneBreachState newState)
>                   then breachResolved
>                   else return []
>         unfinishedPlayerIds <- fmap catMaybes $ mapM
>                 (\ playerId -> do
>                     hand <- getHand playerId
>                     if null hand
>                       then return Nothing
>                       else do
>                         savedState <- getPlayerState playerId
>                         setPlayerState playerId DiscardingCards {
>                             discardingCardsSavedState = savedState,
>                             discardingCards = [],
>                             discardingCardCount = 2,
>                             discardingCardsDone = doneDiscarding
>                             }
>                         return (Just playerId))
>                 playerIds
>         state <- getState
>         setState state { thunderstoneBreachState = unfinishedPlayerIds }
>         return [ThunderstoneEventBreach card text]

>   | otherwise = Nothing

Blink Dog: "Cannot be attacked if a Light Penalty persists."

> canAttack :: [(Card,DungeonPartyStats)] -> (Int,Card) -> Bool
> canAttack stats dungeon@(rank,MonsterCard BlinkDog) =
>     rank + 1 < sum [dungeonPartyLight stat
>                     | (_,stat) <- fst (applyPartyBattleEffects
>                                            stats dungeon),
>                       not (dungeonPartyNotAttacking stat)]
> canAttack _ (rank,card) = isMonster card

Amazon Archer: "Additional ATTACK +2 at Rank 2 or 3."
Amazon Huntress: "Additional ATTACK +3 at Rank 2 or 3."
Amazon Queen: "Additional ATTACK +4 at Rank 2 or 3."
Dwarf Guardian: "Additional ATTACK +3 when equipped with an Edged Weapon."
Dwarf Janissary: "Additional ATTACK +4 when equipped with an Edged Weapon."
Dwarf Sentinel: "Additional ATTACK +5 when equipped with an Edged Weapon."
Feayn Archer, Feayn Marksman, Feayn Sniper: "Cannot attack Rank 1."
Selurin Magician: "All Items and Magic Attack Spells gain MAGIC ATTACK +1."
Selurin Warlock, Selurin Theurge: "Total MAGIC ATTACK x2* (apply last)"
Thyrian Knight: "All Militia gain ATTACK +1."
Thyrian Lord: "All Heroes other than Fighters gain ATTACK +2."

Polearm: "ATTACK +2, or ATTACK +6 when attached to a Hero "
                ++ "with 8 or more Strength."
Warhammer: "Clerics gain an additional ATTACK +3 against "
                ++ "Doomknights and Undead."

> applyPartyBattleEffects :: [(Card,DungeonPartyStats)] -> (Int,Card)
>                    -> ([(Card,DungeonPartyStats)],Int)
> applyPartyBattleEffects stats (rank,monster) =
>     foldl applyBattleEffect (stats,1) (zip [0..] stats)
>   where
>     applyBattleEffect (stats,totalMagicMultiplier) (index,(card,_))
>       | card == HeroCard AmazonArcher =
>             (modifyWhen (\ (statIndex,(card,stat)) ->
>                             statIndex == index && rank > 1)
>                         (addAttack 2),
>              totalMagicMultiplier)
>       | card == HeroCard AmazonHuntress =
>             (modifyWhen (\ (statIndex,(card,stat)) ->
>                             statIndex == index && rank > 1)
>                         (addAttack 3),
>              totalMagicMultiplier)
>       | card == HeroCard AmazonArcher =
>             (modifyWhen (\ (statIndex,(card,stat)) ->
>                             statIndex == index && rank > 1)
>                         (addAttack 4),
>              totalMagicMultiplier)
>       | card == HeroCard DwarfGuardian =
>             (modifyWhen (\ (statIndex,(card,stat)) ->
>                             statIndex == index
>                                 && indexHasClass
>                                         (dungeonPartyEquippedWith stat)
>                                         ClassEdged)
>                         (addAttack 3),
>              totalMagicMultiplier)
>       | card == HeroCard DwarfJanissary =
>             (modifyWhen (\ (statIndex,(card,stat)) ->
>                             statIndex == index
>                                 && indexHasClass
>                                         (dungeonPartyEquippedWith stat)
>                                         ClassEdged)
>                         (addAttack 4),
>              totalMagicMultiplier)
>       | card == HeroCard DwarfSentinel =
>             (modifyWhen (\ (statIndex,(card,stat)) ->
>                             statIndex == index
>                                 && indexHasClass
>                                         (dungeonPartyEquippedWith stat)
>                                         ClassEdged)
>                         (addAttack 5),
>              totalMagicMultiplier)
>       | card == HeroCard FeaynArcher
>                 || card == HeroCard FeaynMarksman
>                 || card == HeroCard FeaynSniper =
>             (modifyWhen (\ (statIndex,(card,stat)) ->
>                             statIndex == index
>                                 || dungeonPartyEquippedBy stat == Just index)
>                         (\ stat -> stat {
>                             dungeonPartyNotAttacking =
>                                 dungeonPartyNotAttacking stat || rank == 1
>                                 }),
>              totalMagicMultiplier)
>       | card == HeroCard SelurinMagician =
>             (modifyWhen (\ (statIndex,(card,stat)) ->
>                             card `hasClass` ClassItem
>                                 || (card `hasClass` ClassSpell
>                                     && dungeonPartyMagicAttack stat > 0))
>                         (\ stat -> stat {
>                             dungeonPartyMagicAttack =
>                                 dungeonPartyMagicAttack stat + 1
>                                 }),
>              totalMagicMultiplier)
>       | card == HeroCard SelurinWarlock || card == HeroCard SelurinWarlock =
>                 (stats,totalMagicMultiplier*2)
>       | card == HeroCard ThyrianKnight =
>             (modifyWhen (\ (statIndex,(card,stat)) ->
>                              card == HeroCard Militia)
>                         (addAttack 1),
>              totalMagicMultiplier)
>       | card == HeroCard ThyrianLord =
>             (modifyWhen (\ (statIndex,(card,stat)) ->
>                              isHero card
>                                  && not (card `hasClass` ClassFighter))
>                         (addAttack 2),
>              totalMagicMultiplier)
>       | card == VillageCard Polearm =
>             (modifyWhen (\ (statIndex,(card,stat)) ->
>                             statIndex == index
>                                 && indexStrength
>                                         (dungeonPartyEquippedBy stat) >= 8)
>                         (addAttack 4),
>              totalMagicMultiplier)
>       | card == VillageCard Warhammer =
>             (modifyWhen (\ (statIndex,(card,stat)) ->
>                             statIndex == index
>                                 && indexHasClass
>                                         (dungeonPartyEquippedBy stat)
>                                         ClassCleric
>                                 && (monster `hasClass` ClassDoomknight
>                                     || monster `hasClass` ClassUndead))
>                         (addAttack 3),
>              totalMagicMultiplier)
>       | otherwise = (stats,totalMagicMultiplier)
>     modifyWhen test modify = map update (zip [0..] stats)
>       where
>         update (index,(card,stat)) =
>             if test (index,(card,stat))
>               then (card,modify stat) else (card,stat)
>     indexHasClass Nothing testClass = False
>     indexHasClass (Just index) testClass =
>         fst (stats !! index) `hasClass` testClass
>     indexStrength Nothing = 0
>     indexStrength (Just index) = dungeonPartyStrength (snd (stats !! index))
>     addAttack attack stat = stat {
>         dungeonPartyAttack = dungeonPartyAttack stat + attack
>         }

The Unchained, Deadbone Troll, Famine, Kingdom, Suffering: "BATTLE: Gain one Disease."
Lord of Death: "BATTLE: Gain two Diseases."

> gainDiseaseBattleEffects :: PlayerId -> Card
>                          -> Thunderstone [ThunderstoneEvent]
> gainDiseaseBattleEffects playerId monster =
>     fmap concat (mapM applyBattleEffect (cardCardText monster))
>   where
>     applyBattleEffect text
>       | text == "BATTLE: Gain one Disease." = do
>             discard playerId [DiseaseCard]
>             return [ThunderstoneEventGainDungeonCard playerId 0 DiseaseCard]
>       | text == "BATTLE: Gain two Diseases." = do
>             discard playerId [DiseaseCard,DiseaseCard]
>             return [ThunderstoneEventGainDungeonCard playerId 0 DiseaseCard,
>                     ThunderstoneEventGainDungeonCard playerId 0 DiseaseCard]
>       | otherwise = return []

Archduke of Pain, Revenant: "Magic Attack Required"
Succubus, Uyril Unending, Ghost: "HALF-ATTACK without MAGIC ATTACK present"
Tormenter: "HALF-ATTACK without a Weapon present"
Darkness: "Unequipped Heroes cannot attack"
Darkness, Lord Mortis, Blink Dog: "Light -1"
Judgement: "BATTLE: All Heroes suffer Strength -2 and ATTACK -1."
Knightmare: "Light -2"
The Prince, Suffering, Ghost: "BATTLE: All Heroes suffer Strength -2."
Ebon Fume: "Immune to Magic Attack"
Sphinx: "Magic Attack Only"
Grayskin Lizard: "BATTLE: Heroes with Weapons equipped gain ATTACK +1."
Noxious Slag: "HALF-ATTACK from MAGIC ATTACK"
Noxious Slag: "Immune to Edged Weapons"
Revenant: "BATTLE: All Heroes suffer Strength -4.  Any Heroes "
                ++ "with Strength 0 or less are Destroyed."

> noninteractiveBattleEffects :: [(Card,DungeonPartyStats)] -> Card
>                             -> ([(Card,DungeonPartyStats)],Int,Int,Int)
> noninteractiveBattleEffects stats monster =
>     foldl applyBattleEffect (stats,1,1,0) (cardCardText monster)
>   where
>     applyBattleEffect (stats,attackDivisor,magicDivisor,lightPenalty) text
>       | text == "Magic Attack Required" =
>             let (attack,magicAttack) = addAttacks stats
>             in  if magicAttack > 0
>                   then (stats,attackDivisor,magicDivisor,lightPenalty)
>                   else (stats,attack+1,magicAttack+1,lightPenalty)
>       | text == "HALF-ATTACK without MAGIC ATTACK present" =
>             let (attack,magicAttack) = addAttacks stats
>             in  if magicAttack > 0
>                   then (stats,attackDivisor,magicDivisor,lightPenalty)
>                   else (stats,attackDivisor*2,magicDivisor,lightPenalty)
>       | text == "HALF-ATTACK without a Weapon present"
>                 && null (filter (\ (card,stat) ->
>                                     not (dungeonPartyNotAttacking stat)
>                                         && card `hasClass` ClassWeapon)
>                                 stats) =
>             (stats,attackDivisor*2,magicDivisor*2,lightPenalty)
>       | text == "Unequipped Heroes cannot attack" =
>             let removeUnequipped (card,stat)
>                   | isHero card
>                             && isNothing (dungeonPartyEquippedWith stat) =
>                         (card,stat { dungeonPartyNotAttacking = True })
>                   | otherwise = (card,stat)
>             in  (map removeUnequipped stats,
>                  attackDivisor,magicDivisor,lightPenalty)
>       | text == "Light -1" =
>             (stats,attackDivisor,magicDivisor,lightPenalty+1)
>       | text == "BATTLE: All Heroes suffer Strength -2 and ATTACK -1." =
>             (disableHeavyWeapons $ map (reduceHeroStrength 2)
>                                  $ map (reduceHeroAttack 1) stats,
>              attackDivisor,magicDivisor,lightPenalty)
>       | text == "Light -2" =
>             (stats,attackDivisor,magicDivisor,lightPenalty+2)
>       | text == "BATTLE: All Heroes suffer Strength -2." =
>             (disableHeavyWeapons $ map (reduceHeroStrength 2) stats,
>              attackDivisor,magicDivisor,lightPenalty)
>       | text == "Immune to Magic Attack" =
>             let (attack,magicAttack) = addAttacks stats
>             in  (stats,attackDivisor,magicAttack+1,lightPenalty)
>       | text == "Magic Attack Only" =
>             let (attack,magicAttack) = addAttacks stats
>             in  (stats,attack+1,magicDivisor,lightPenalty)
>       | text == "BATTLE: Heroes with Weapons equipped gain ATTACK +1." =
>             let gainAttack (card,stat)
>                   | not (isNothing (dungeonPartyEquippedWith stat)) =
>                         (card,stat {
>                                 dungeonPartyAttack =
>                                     dungeonPartyAttack stat + 1
>                                 })
>                   | otherwise = (card,stat)
>             in  (map gainAttack stats,
>                  attackDivisor,magicDivisor,lightPenalty)
>       | text == "HALF-ATTACK from MAGIC ATTACK" =
>             (stats,attackDivisor,magicDivisor*2,lightPenalty)
>       | text == "Immune to Edged Weapons" =
>             let removeEdged (card,stat)
>                   | card `hasClass` ClassEdged =
>                         (card,stat { dungeonPartyNotAttacking = True })
>                   | otherwise = (card,stat)
>             in  (map removeEdged stats,
>                  attackDivisor,magicDivisor,lightPenalty)
>       | text == "BATTLE: All Heroes suffer Strength -4.  Any Heroes "
>                     ++ "with Strength 0 or less are Destroyed." =
>             (map destroyWeakHero $ disableHeavyWeapons
>                                  $ map (reduceHeroStrength 4) stats,
>              attackDivisor,magicDivisor,lightPenalty)
>       | otherwise = (stats,attackDivisor,magicDivisor,lightPenalty)
>     addAttacks stats = foldl addAttack (0,0) stats
>       where
>         addAttack (attack,magicAttack) (card,stat)
>           | dungeonPartyNotAttacking stat = (attack,magicAttack)
>           | otherwise =
>                 (attack + dungeonPartyAttack stat,
>                  magicAttack + dungeonPartyMagicAttack stat)
>     disableHeavyWeapons stats = map disableHeavyWeapon stats
>       where
>         disableHeavyWeapon (card,stat)
>           | fmap ((< dungeonPartyWeight stat) . dungeonPartyStrength
>                                               . snd . (stats !!))
>                  (dungeonPartyEquippedBy stat) /= Just True =
>                 (card,stat)
>           | otherwise =
>                 (card,stat { dungeonPartyNotAttacking = True })
>     destroyWeakHero (card,stat)
>       | isHero card && dungeonPartyStrength stat <= 0 =
>             (card,stat { dungeonPartyDestroyed = True })
>       | otherwise = (card,stat)
>     reduceHeroStrength reduction (card,stat)
>       | isHero card =
>             (card,stat {
>                 dungeonPartyStrength = dungeonPartyStrength stat - reduction
>                 })
>       | otherwise = (card,stat)
>     reduceHeroAttack reduction (card,stat)
>       | isHero card =
>             (card,stat {
>                 dungeonPartyAttack = dungeonPartyAttack stat - reduction
>                 })
>       | otherwise = (card,stat)

Any cards destroyed by a Battle Effect remain in play until the end of
the battle -- Heroes fight until the bitter end!

Tormenter: "BATTLE: Destroy one Cleric."
Knightmare, The Prince: "BATTLE: Destroy one Fighter."
Ebon Fume: "BATTLE: Destroy one Hero with the highest Strength."
Mythlurian: "BATTLE: Destroy one Hero."
Skaladak, Red Jelly: "BATTLE: Destroy one Weapon."
Uyril Unending, Black Slime, Spirit, Wraith: "BATTLE: Destroy one Militia."
Gray Ooze: "BATTLE: Destroy one Hero unless at least one Weapon "
                ++ "is attached to the Party."
Green Blob: "BATTLE: Destroy one Food."
Harbinger: "BATTLE: Destroy one Spell."
Haunt: "BATTLE: One Hero cannot attack."

> interactiveBattleEffects :: PlayerId -> (Int,Card) -> [ThunderstoneEvent]
>                          -> ([ThunderstoneEvent]
>                                      -> Thunderstone [ThunderstoneEvent])
>                          -> Thunderstone [ThunderstoneEvent]
> interactiveBattleEffects playerId (rank,monster) events resolveBattle = do
>     fmap concat (mapM applyEffects (cardCardText monster))
>   where
>     applyEffects text
>       | text == "BATTLE: Destroy one Cleric." =
>             destroyOneCard (`hasClass` ClassCleric) text
>       | text == "BATTLE: Destroy one Fighter." =
>             destroyOneCard (`hasClass` ClassFighter) text
>       | text == "BATTLE: Destroy one Hero with the highest Strength." = do
>             playerState <- getPlayerState playerId
>             hand <- getHand playerId
>             let stats = dungeonEffectsStats playerState
>             let strength = maximum (map dungeonPartyStrength stats)
>             let eligible (_,card,stat) =
>                     isHero card && strength == dungeonPartyStrength stat
>             case filter eligible (zip3 [0..] hand stats) of
>               [] -> resolveBattle events
>               [(index,_,_)] -> do
>                     setPlayerState playerId playerState {
>                         dungeonEffectsStats =
>                             updateIndex
>                                 index
>                                (dungeonEffectsStats playerState)
>                                (\ stat -> stat {
>                                     dungeonPartyDestroyed = True
>                                     })
>                         }
>                     resolveBattle (events ++ [ThunderstoneEventBattleEffect
>                                                   playerId monster text])
>               options -> do
>                     setPlayerState playerId
>                         (ChoosingOption WhichCardToDestroy
>                              [((index,show card,Nothing),do
>                                    setPlayerState playerId playerState {
>                                        dungeonEffectsStats =
>                                            updateIndex
>                                                index
>                                                (dungeonEffectsStats
>                                                     playerState)
>                                                (\ stat -> stat {
>                                                     dungeonPartyDestroyed =
>                                                         True
>                                                     })
>                                        }
>                                    resolveBattle [])
>                               | (index,card,_) <- options]
>                              Nothing)
>                     return (events ++ [ThunderstoneEventBattleEffect
>                                            playerId monster text])
>       | text == "BATTLE: Destroy one Hero." =
>             destroyOneCard isHero text
>       | text == "BATTLE: Destroy one Weapon." =
>             destroyOneCard (`hasClass` ClassWeapon) text
>       | text == "BATTLE: Destroy one Militia." =
>             destroyOneCard (== HeroCard Militia) text
>       | text == "BATTLE: Destroy one Hero unless at least one Weapon "
>                     ++ "is attached to the Party." = do
>             stats <- fmap dungeonEffectsStats (getPlayerState playerId)
>             if any (not . isNothing . dungeonPartyEquippedBy) stats
>               then resolveBattle events
>               else destroyOneCard isHero text
>       | text == "BATTLE: Destroy one Food." =
>             destroyOneCard (`hasClass` ClassFood) text
>       | text == "BATTLE: Destroy one Spell." =
>             destroyOneCard (`hasClass` ClassSpell) text
>       | text == "BATTLE: One Hero cannot attack." = do
>             stats <- fmap dungeonEffectsStats (getPlayerState playerId)
>             hand <- getHand playerId
>             let eligible (card,stat) =
>                     isHero card && not (dungeonPartyNotAttacking stat)
>             if not (any eligible (zip hand stats))
>               then resolveBattle events
>               else do
>                 playerState <- getPlayerState playerId
>                 setPlayerState playerId
>                     (ChoosingOption WhichCardToDestroy
>                          [((index,show card,Nothing),do
>                                setPlayerState playerId playerState {
>                                    dungeonEffectsStats =
>                                        updateIndex
>                                            index
>                                            (dungeonEffectsStats playerState)
>                                            (\ stat -> stat {
>                                                 dungeonPartyNotAttacking =
>                                                     True
>                                                 })
>                                    }
>                                resolveBattle [])
>                           | (index,card,stat) <- zip3 [0..] hand stats,
>                             eligible (card,stat)]
>                      Nothing)
>                 return (events ++ [ThunderstoneEventBattleEffect
>                                        playerId monster text])
>       | otherwise = resolveBattle events
>     destroyOneCard test text = do
>         cards <- fmap (filter (test . snd) . zip [0..]) (getHand playerId)
>         if null cards
>           then resolveBattle events
>           else do
>             playerState <- getPlayerState playerId
>             setPlayerState playerId
>                 (ChoosingOption WhichCardToDestroy
>                      [((index,show card,Nothing),do
>                            setPlayerState playerId playerState {
>                                dungeonEffectsStats =
>                                    updateIndex
>                                        index
>                                        (dungeonEffectsStats playerState)
>                                        (\ stat -> stat {
>                                             dungeonPartyDestroyed = True
>                                             })
>                                }
>                            resolveBattle [])
>                       | (index,card) <- cards]
>                      Nothing)
>             return (events ++ [ThunderstoneEventBattleEffect
>                                    playerId monster text])
>     updateIndex index stats update =
>         map (\ (i,stat) -> if index == i then update stat else stat)
>             (zip [0..] stats)

> battleMonster :: PlayerId -> (Int,Card)
>               -> (Bool -> [ThunderstoneEvent]
>                        -> Thunderstone [ThunderstoneEvent])
>               -> Thunderstone [ThunderstoneEvent]
> battleMonster playerId (rank,monster) battleResolved = do
>     hand <- getHand playerId
>     playerState <- getPlayerState playerId
>     let (partyStats,magicAttackMultiplier) =
>             applyPartyBattleEffects
>                 (zip hand (dungeonEffectsStats playerState))
>                 (rank,monster)
>     setPlayerState playerId playerState {
>         dungeonEffectsStats = map snd partyStats
>         }
>     events <- gainDiseaseBattleEffects playerId monster
>     interactiveBattleEffects playerId (rank,monster) events
>                              (resolveBattle magicAttackMultiplier)
>   where
>     resolveBattle magicAttackMultiplier events = do
>         hand <- getHand playerId
>         playerState <- getPlayerState playerId
>         let (partyStats,attackDivisor,magicDivisor,lightPenalty) =
>                 noninteractiveBattleEffects
>                     (zip hand (dungeonEffectsStats playerState)) monster
>         battleResolved (totalAttack partyStats magicAttackMultiplier
>                             attackDivisor magicDivisor
>                         >= cardMonsterHealth monster
>                                 + lightDefense lightPenalty partyStats)
>                        events
>     lightDefense lightPenalty partyStats =
>         max 0 (2*(rank + lightPenalty - totalLight partyStats))
>     totalAttack stats magicAttackMultiplier attackDivisor magicDivisor =
>         (attack `div` attackDivisor)
>             + (magicAttackMultiplier*(magicAttack `div` magicDivisor))
>       where
>         (attack,magicAttack) = foldl addAttacks (0,0) $ map getAttacks stats
>         addAttacks (attack1,magicAttack1) (attack2,magicAttack2) =
>             (attack1+attack2,magicAttack1+magicAttack2)
>         getAttacks (_,cardStats)
>           | dungeonPartyNotAttacking cardStats = (0,0)
>           | otherwise = (dungeonPartyAttack cardStats,
>                          dungeonPartyMagicAttack cardStats)
>     totalLight stats = sum (map getLight stats)
>       where
>         getLight (_,cardStats)
>           | dungeonPartyNotAttacking cardStats = 0
>           | otherwise = dungeonPartyLight cardStats

Chalice Paladin: "Spoils (Village)."
Dwarf Janissary: "Spoils (Weapon)."

Sphinx: "Spoils (Reveal six cards from your deck and destroy "
                ++ "any of these cards you choose.  Discard the rest.)"
Gray Oooze: "Spoils (Food)"

> spoilsEffect :: PlayerId -> [Card] -> Bool -> Card
>              -> [[ThunderstoneEvent]
>                  -> ([ThunderstoneEvent] -> Thunderstone [ThunderstoneEvent])
>                  -> Thunderstone [ThunderstoneEvent]]
> spoilsEffect playerId hand defeatedMonster card =
>     if not (isMonster card) || defeatedMonster
>       then concatMap effects (cardCardText card)
>       else []
>   where
>     effects text
>       | text == "Spoils (Village)." = [buySpoils text (const True)]
>       | text == "Spoils (Weapon)." =
>             [buySpoils text (`hasClass` ClassWeapon)]
>       | text == "Spoils (Food)." = [buySpoils text (`hasClass` ClassFood)]
>       | text == "Spoils (Reveal six cards from your deck and destroy "
>                     ++ "any of these cards you choose.  Discard the rest.)" =
>             [\ events onEffectResolved -> do
>                 playerState <- getPlayerState playerId
>                 drawnCards <- multiple 6 $ drawCard playerId
>                 setPlayerState playerId
>                     (ChoosingOption WhichCardToDestroy
>                          [((index,
>                             "Destroy: " ++ show toDestroy
>                                 ++ " Discard: " ++ show toDiscard,
>                             Nothing),do
>                             setPlayerState playerId playerState
>                             discard playerId toDiscard
>                             onEffectResolved
>                                 (map (ThunderstoneEventDestroyCard playerId)
>                                      toDestroy
>                                  ++ map (ThunderstoneEventDiscard playerId)
>                                         toDiscard))
>                           | (index,(toDestroy,toDiscard)) <-
>                                 zip [1..] (partitionOptions drawnCards)]
>                          Nothing)
>                 return
>                     (events
>                      ++ [ThunderstoneEventSpoilsEffect playerId card text,
>                          ThunderstoneEventRevealCards playerId drawnCards])]
>       | otherwise = []
>     availableGold = sum (map cardVillageGold hand)
>     getVillageCards eligible = do
>         village <- fmap (map fst . filter ((> 0) . snd)) getVillage
>         heroes <- fmap catMaybes $ mapM topHero [minBound..maxBound]
>         return $ filter eligible
>                $ filter ((> availableGold) . cardVillagePrice)
>                $ map VillageCard village ++ map HeroCard heroes
>     buySpoils text eligible events onEffectResolved = do
>         availableSpoils <- getVillageCards eligible
>         if null availableSpoils
>           then onEffectResolved events
>           else do
>             playerState <- getPlayerState playerId
>             setPlayerState playerId
>                 (ChoosingOption WhichCardToBuy
>                      [((index,show availableCard,Nothing),do
>                           setPlayerState playerId playerState
>                           discard playerId [availableCard]
>                           case availableCard of
>                             VillageCard villageCard -> do
>                               drawVillage villageCard
>                               return ()
>                             HeroCard heroCard -> do
>                               drawHero (cardType $ heroDetails heroCard)
>                               return ()
>                             _ -> return ()
>                           onEffectResolved [ThunderstoneEventSpoilsEffect
>                                                 playerId card text,
>                                             ThunderstoneEventPurchase
>                                                 playerId availableCard])
>                       | (index,availableCard) <- zip [1..] availableSpoils]
>                      (Just (setPlayerState playerId playerState)))
>             return events

> partitionOptions :: (Eq a,Ord a) => [a] -> [([a],[a])]
> partitionOptions items = nub $ map sortParts $ partitions items
>   where
>     sortParts (a,b) = (sort a,sort b)
>     partitions [] = [([],[])]
>     partitions (a:as) = concatMap (add a) (partitions as)
>     add a (as,bs) = [(a:as,bs),(as,a:bs)]

GHC 6.8 does not have Data.List.permutations

> permutations :: [a] -> [[a]]
> permutations xs0        =  xs0 : perms xs0 []
>   where
>     perms []     _  = []
>     perms (t:ts) is = foldr interleave (perms ts (t:is)) (permutations is)
>       where interleave    xs     r = let (_,zs) = interleave' id xs r in zs
>             interleave' _ []     r = (ts, r)
>             interleave' f (y:ys) r = let (us,zs) = interleave' (f . (y:)) ys r
>                                      in  (y:us, f (t:y:us) : zs)
