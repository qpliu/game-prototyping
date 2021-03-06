> module ThunderstoneApp(thunderstoneApp) where

> import Control.Monad(when,unless)
> import Data.List(find,isInfixOf,nub,(\\))
> import Data.Maybe(isNothing)
> import System.Random(StdGen,getStdGen,mkStdGen,next,randomR)

> import GameApp
>     (GameApp(..),gameApp,
>      Game,gameGetName,gameGetState,gameSetState,
>      gameMessageToAll,gameMessageToAllExcept,gameMessageToUser,
>      App,UserId)

> import ThunderstoneBase
>     (ThunderstoneState,PlayerId,
>      Card(..),ThunderstonePublicState(..),ThunderstonePlayerState(..),
>      ThunderstoneEvent(..),PlayerStateInfo(..),PlayerAction(..),
>      thunderstoneInit,
>      thunderstoneStartGame,randomizedGame,
>      thunderstonePublicState,thunderstonePlayerState,
>      thunderstoneTakeAction)
> import ThunderstoneCards
>     (ThunderstoneCard(..),HeroCard(..),MonsterCard(..),VillageCard(..),
>      DiseaseCard(..),
>      CardClass(..))
> import ThunderstoneCardDetails
>     (CardDetails(..),
>      heroDetails,villageDetails,monsterDetails,thunderstoneDetails,
>      diseaseDetails)

> thunderstoneApp :: IO App
> thunderstoneApp = do
>     stdGen <- getStdGen
>     let (seed,stdGen1) = next stdGen
>     let stdGen2 = mkStdGen seed
>     gameApp $ thunderstoneGame stdGen2 $ thunderstoneInit stdGen1

> thunderstoneGame :: StdGen -> ThunderstoneState -> ThunderstoneGame
> thunderstoneGame stdGen state =
>     ThunderstoneGame {
>         tsPlayers = [],
>         tsBots = [],
>         tsState = state,
>         tsGameStarted = False,
>         tsStdGen = stdGen
>         }

> data ThunderstoneGame = ThunderstoneGame {
>     tsPlayers :: [Player],
>     tsBots :: [Bot],
>     tsState :: ThunderstoneState,
>     tsGameStarted :: Bool,
>     tsStdGen :: StdGen
>     }

> data Player = Player {
>     userId :: UserId,
>     playerId :: PlayerId
>     }

> data Bot = Bot {
>     botName :: String,
>     botId :: PlayerId,
>     botState :: BotState
>     }

> instance GameApp ThunderstoneGame where
>     gameAppName _ = "Thunderstone"
>     gameAppAddPlayer uid = do
>         name <- gameGetName uid
>         gameMessageToAllExcept [uid] [name ++ " arrives."]
>         gameMessageToUser uid
>             ["Welcome to Thunderstone",
>              "/play to join the game",
>              "/start to start the game",
>              "/game to see the game state",
>              "/do to list available actions or to take an action"]
>     gameAppRemovePlayer uid = do
>         name <- gameGetName uid
>         gameMessageToAll [name ++ " leaves."]
>         state <- gameGetState
>         when (userInGame state uid) (do
>             if tsGameStarted state
>               then do
>                 gameMessageToAll ["The game is canceled."]
>                 gameSetState state {
>                     tsPlayers = [],
>                     tsBots = [],
>                     tsGameStarted = False
>                     }
>               else
>                 gameSetState state {
>                     tsPlayers = filter ((/= uid) . userId) (tsPlayers state)
>                     })
>     gameAppCommands = [("/play",playCmd),
>                        ("/bot",botCmd),
>                        ("/start",startCmd),
>                        ("/game",gameCmd),
>                        ("/g",gameCmd),
>                        ("/do",doCmd),
>                        ("/d",doCmd),
>                        ("/",doCmd),
>                        ("/inspect",inspectCmd),
>                        ("/i",inspectCmd)]
>                       ++ [("/" ++ show i,doIndexCmd i) | i <- [1..10]]
>     gameAppPollTime _ _ = Nothing
>     gameAppPoll = return ()

> playCmd :: UserId -> [String] -> Game ThunderstoneGame ()
> playCmd uid _ = do
>     state <- gameGetState
>     doPlay state
>   where
>     doPlay state
>       | userInGame state uid =
>             gameMessageToUser uid ["You are already in the game."]
>       | tsGameStarted state =
>             gameMessageToUser uid
>                 ["You cannot join a game already in progress."]
>       | length (tsPlayers state) + length (tsBots state) >= 5 =
>             gameMessageToUser uid
>                 ["The game is full. (Maximum of 5 players and bots)."]
>       | otherwise = do
>             name <- gameGetName uid
>             gameMessageToAll [name ++ " is playing the game."]
>             state <- gameGetState
>             gameSetState state {
>                 tsPlayers = newPlayer : tsPlayers state
>                 }
>     newPlayer = Player {
>         userId = uid,
>         playerId = error "game not started"
>         }

> botCmd :: UserId -> [String] -> Game ThunderstoneGame ()
> botCmd uid args = do
>     state <- gameGetState
>     doBot state
>   where
>     doBot state
>       | tsGameStarted state =
>             gameMessageToUser uid ["The game is already in progress."]
>       | length (tsPlayers state) + length (tsBots state) >= 5 =
>             gameMessageToUser uid
>                 ["The game is full. (Maximum of 5 players and bots)."]
>       | length args /= 1 || isNothing maybeBot =
>             gameMessageToUser uid
>                 ["Specify bot: " ++ unwords (map fst botList)]
>       | otherwise = do
>             state <- gameGetState
>             gameSetState state {
>                 tsBots = tsBots state ++ [Bot {
>                                             botName = getBotName state,
>                                             botId = error "game not started",
>                                             botState = botState
>                                             }]
>                 }
>     maybeBot = lookup (head args) botList
>     Just botState = maybeBot
>     getBotName state = head args ++ show (length (tsPlayers state)
>                                           + length (tsBots state))

> startCmd :: UserId -> [String] -> Game ThunderstoneGame ()
> startCmd uid _ = do
>     state <- gameGetState
>     doStart state
>   where
>     doStart state
>       | not (userInGame state uid) =
>             gameMessageToUser uid
>                 ["You are not in the game.","/play to join the game."]
>       | tsGameStarted state =
>             gameMessageToUser uid ["The game has already been started."]
>       | otherwise = do
>             name <- gameGetName uid
>             gameMessageToAll [name ++ " starts the game."]
>             state <- gameGetState
>             stdGens <- sequence (replicate (length (tsBots state))
>                                  getNewStdGen)
>             state <- gameGetState
>             gameSetState state {
>                 tsPlayers = setPlayerIds,
>                 tsBots = initBots stdGens,
>                 tsState = thunderstoneState,
>                 tsGameStarted = True
>                 }
>             runBots
>           where
>             (thunderstoneState,playerIds) =
>                 thunderstoneStartGame (tsState state)
>                                       numberOfPlayers randomizedGame
>             numberOfPlayers =
>                 length (tsPlayers state) + length (tsBots state)
>             setPlayerIds = zipWith setPlayerId playerIds (tsPlayers state)
>             setPlayerId playerId player = player { playerId = playerId }
>             initBots stdGens =
>                 zipWith3 initBot
>                          stdGens
>                          (drop (length (tsPlayers state)) playerIds)
>                          (tsBots state)
>             initBot stdGen botId bot = bot {
>                 botId = botId,
>                 botState = botInit (botState bot) stdGen
>                                                   botId
>                                                   (playerIds \\ [botId])
>                 }

> gameCmd :: UserId -> [String] -> Game ThunderstoneGame ()
> gameCmd uid _ = do
>     state <- gameGetState
>     doGame state
>   where
>     doGame state
>       | not (tsGameStarted state) = do
>             names <- mapM (gameGetName . userId) (tsPlayers state)
>             if null names
>               then
>                 gameMessageToUser uid
>                     ["The game has not started and there are no players.",
>                      "/play to join the game.",
>                      "/start to start the game after all players join."]
>               else do
>                 gameMessageToUser uid
>                     (("The game has not started.  Players:") :
>                      [' ' : name | name <- names])
>                 let botNames = map botName (tsBots state)
>                 unless (null botNames)
>                     (gameMessageToUser uid
>                          ("Bots:" : [' ' : name | name <- botNames]))
>       | otherwise = do
>             getPlayerName <- getPlayerNames
>             gameMessageToUser uid
>                (showPublicState (thunderstonePublicState (tsState state))
>                                  getPlayerName)
>             maybe (return ())
>                   (gameMessageToUser uid
>                        . showPlayerState
>                        . thunderstonePlayerState (tsState state))
>                   (getPlayerId state uid)
>     showPublicState state getPlayerName
>       | publicStateGameOver state =
>             ["Game over.  Scores:"] ++ 
>             [" " ++ getPlayerName playerId ++ ": " ++ show score
>              | (playerId,score) <- publicStateScores state]
>       | otherwise =
>             ["Dungeon rank " ++ show rank ++ ": " ++ show card
>              | (rank,card) <- zip [1..] (publicStateDungeonHall state)]
>             ++ ["Village:"]
>             ++ [show heroType ++ " (" ++ show (length cards) ++ ")"
>                 ++ (if null cards
>                       then ""
>                       else ": " ++ show (HeroCard (head cards)) ++ " $"
>                                 ++ showJust (cardPrice $ heroDetails
>                                                        $ head cards))
>                 | (heroType,cards) <- publicStateHeroes state]
>             ++ [show (VillageCard card) ++ " (" ++ show count ++ ") $"
>                 ++ showJust (cardPrice $ villageDetails card)
>                 | (card,count) <- publicStateVillage state]
>             ++ ["Players to take actions:"
>                 ++ (unwords $ map getPlayerName
>                             $ publicStateTakingAction state)]
>     showPlayerState state =
>         ["Draw pile: " ++ show (playerStateDeck state)
>          ++ " Discards: " ++ show (playerStateDiscards state)
>          ++ " XP: " ++ show (playerStateXP state),
>          "Hand:"]
>         ++ showHand (playerStateHand state) (playerStateInfo state)
>         ++ concatMap (showInfo (playerStateInfo state))
>                      (playerStateInfo state)
>     showInfo _ (PlayerStatePurchases buys) = ["Purchases: " ++ show buys]
>     showInfo _ (PlayerStateGold gold) = ["Gold: " ++ show gold]
>     showInfo allInfo (PlayerStateAttack attack) =
>         ["Attack: " ++ show (sum $ filterAttacking allInfo attack)]
>     showInfo allInfo (PlayerStateMagicAttack attack) =
>         ["Magic attack: " ++ show (sum $ filterAttacking allInfo attack)]
>     showInfo allInfo (PlayerStateLight light) =
>         ["Light: " ++ show (sum $ filterAttacking allInfo light)]
>     showInfo _ (PlayerStateOption playerOption) =
>         ["Choosing: " ++ show playerOption]
>     showInfo _ (PlayerStateEffectsUsed _) = []
>     showInfo _ (PlayerStateEquipped _) = []
>     showInfo _ (PlayerStateStrength _) = []
>     showInfo _ (PlayerStateWeight _) = []
>     showInfo _ (PlayerStateNotAttacking _) = []
>     showInfo _ (PlayerStateCard card) = ["Active card: " ++ show card]
>     filterAttacking allInfo list = foldl doFilter list allInfo
>       where
>         doFilter list (PlayerStateNotAttacking flags) =
>             [if flag then 0 else item | (item,flag) <- zip list flags]
>         doFilter list _ = list
>     showJust (Just a) = show a
>     getUsedCards (PlayerStateEffectsUsed used) = used
>     getUsedCards _ = []
>     showHand cards stateInfo =
>         zipWith showHandCard [0..] cards
>       where
>         showHandCard index card =
>             " " ++ show card
>                 ++ maybe "" (\ i -> " (equipping " ++ show (cards !! i)
>                                     ++ " [" ++ show i ++ "])")
>                          (findProperty (equipping index))
>                 ++ maybe "" (\ i -> " (equipped by " ++ show (cards !! i)
>                                     ++ " [" ++ show i ++ "])")
>                          (findProperty (equippedBy index))
>                 ++ maybe "" ((" str:" ++) . show)
>                             (findProperty (strength index card))
>                 ++ maybe "" ((" wgt:" ++) . show)
>                             (findProperty (weight index card))
>                 ++ maybe "" ((" att:" ++) . show)
>                             (findProperty (attack index))
>                 ++ maybe "" ((" mag:" ++) . show)
>                             (findProperty (magicAttack index))
>                 ++ maybe "" ((" lgt:" ++) . show)
>                             (findProperty (light index))
>                 ++ (if findProperty (used index) == Just True
>                       then " (used)" else "")
>         findProperty :: (PlayerStateInfo -> Maybe b) -> Maybe b
>         findProperty f = foldl (\ result item -> maybe (f item) Just result)
>                                Nothing stateInfo
>         equipping index (PlayerStateEquipped list) = lookup index list
>         equipping index _ = Nothing
>         equippedBy index (PlayerStateEquipped list) =
>             lookup index $ map (\ (a,b) -> (b,a)) list
>         equippedBy index _ = Nothing
>         strength index (HeroCard _) (PlayerStateStrength list) =
>             Just (list !! index)
>         strength _ _ _ = Nothing
>         weight index (VillageCard vcard) (PlayerStateWeight list)
>           | ClassWeapon `elem` (cardClasses $ villageDetails vcard) =
>                 Just (list !! index)
>           | otherwise = Nothing
>         weight _ _ _ = Nothing
>         attack index (PlayerStateAttack list) = Just (list !! index)
>         attack _ _ = Nothing
>         magicAttack index (PlayerStateMagicAttack list) =
>            Just (list !! index)
>         magicAttack _ _ = Nothing
>         light index (PlayerStateLight list) = Just (list !! index)
>         light _ _ = Nothing
>         used index (PlayerStateEffectsUsed list) = Just (list !! index)
>         used _ _ = Nothing

> doCmd :: UserId -> [String] -> Game ThunderstoneGame ()
> doCmd uid args = do
>     state <- gameGetState
>     doDo state
>   where
>     doDo state
>       | not (userInGame state uid) =
>             gameMessageToUser uid
>                 ["You are not in the game.","/play to join the game."]
>       | not (tsGameStarted state) =
>             gameMessageToUser uid
>                 ["The game has not been started.",
>                  "/start to start the game."]
>       | length args /= 1 || isNothing selectedOption
>                          || isNothing actionResult =
>             if null listOptions
>               then gameMessageToUser uid ["(Nothing to do)"]
>               else gameMessageToUser uid listOptions
>       | otherwise = do
>             broadcastEvents events
>             if publicStateGameOver $ thunderstonePublicState newState
>               then
>                 gameSetState state {
>                     tsPlayers = [],
>                     tsBots = [],
>                     tsGameStarted = False
>                     }
>               else do
>                 gameSetState state { tsState = newState }
>                 runBots
>       where
>         Just playerId = getPlayerId state uid
>         selectedOption = lookup (head args) (zip (map show [1..]) options)
>         Just option = selectedOption
>         listOptions = [show index ++ ". " ++ show option
>                        | (index,option) <- zip [1..] options]
>         options = playerStateOptions 
>                       $ thunderstonePlayerState (tsState state) playerId
>         actionResult = thunderstoneTakeAction (tsState state) playerId option
>         Just (newState,events) = actionResult

> doIndexCmd :: Int -> UserId -> [String] -> Game ThunderstoneGame ()
> doIndexCmd index uid _ = doCmd uid [show index]

> inspectCmd :: UserId -> [String] -> Game ThunderstoneGame ()
> inspectCmd uid args = do
>     state <- gameGetState
>     doInspect state
>   where
>     doInspect state
>       | not (tsGameStarted state) =
>             gameMessageToUser uid ["There is no game being played."]
>       | null args =
>             gameMessageToUser uid ["Specify the card to inspect."]
>       | not (userInGame state uid) =
>             gameMessageToUser uid
>                 (inspectCards (unwords args) (tail cardSets))
>       | otherwise = do
>             gameMessageToUser uid
>                 (inspectCards (unwords args) cardSets)
>       where
>         cardSets = [nub cardsInHand, nub cardsShowing]
>         cardsInHand = maybe [] getHand (getPlayerId state uid)
>         getHand playerId =
>             playerStateHand $ thunderstonePlayerState (tsState state)
>                                                       playerId
>         cardsShowing = 
>             concatMap ($ thunderstonePublicState (tsState state))
>                 [publicStateDungeonHall,
>                  concatMap (map HeroCard . take 1 . snd) . publicStateHeroes,
>                  map (VillageCard . fst) . publicStateVillage]
>         inspectCards arg [] = ["No matching cards."]
>         inspectCards arg (cards:moreCards)
>           | null (matchingElements arg cards) = inspectCards arg moreCards
>           | length (matchingElements arg cards) == 1 =
>                 inspectCard $ head $ matchingElements arg cards
>           | otherwise =
>                 ["Matching cards:"]
>                 ++ [" " ++ show card | card <- matchingElements arg cards]
>     inspectCard (HeroCard card) = showDetails (heroDetails card)
>     inspectCard (VillageCard card) = showDetails (villageDetails card)
>     inspectCard (MonsterCard card) = showDetails (monsterDetails card)
>     inspectCard (ThunderstoneCard card) =
>         showDetails (thunderstoneDetails card)
>     inspectCard DiseaseCard = showDetails (diseaseDetails Disease)
>     showDetails :: Show cardType => CardDetails cardType -> [String]
>     showDetails details =
>         ["Card: " ++ cardName details,
>          "Type: " ++ show (cardType details),
>          "Classes: " ++ unwords (map show $ cardClasses details)]
>         ++ optional "Gold" cardGold
>         ++ optional "Light" cardLight
>         ++ optional "VP" cardVictoryPoints
>         ++ optional "Strength" cardStrength
>         ++ optional "Price" cardPrice
>         ++ optional "XP" cardXP
>         ++ optional "Health" cardHealth
>         ++ optional "Weight" cardWeight
>         ++ optional "Level up" cardLevelUp
>         ++ list "Text" cardText
>         ++ list "Glossary" cardGlossary
>       where
>         optional label value =
>             maybe [] ((:[]) . ((label ++ ": ") ++) . show) (value details)
>         list label items =
>             if null (items details)
>               then [] else [label ++ ":"] ++ items details

> matchingElements :: Show e => String -> [e] -> [e]
> matchingElements str elts
>   | not (null fullMatches) = fullMatches
>   | otherwise = partialMatches
>   where
>     fullMatches = filter ((== str) . show) elts
>     partialMatches = filter (isInfixOf str . show) elts

> broadcastEvents :: [ThunderstoneEvent] -> Game ThunderstoneGame ()
> broadcastEvents events = do
>     getPlayerName <- getPlayerNames
>     gameMessageToAll (concatMap (formatEvent getPlayerName) events)
>     state <- gameGetState
>     gameSetState state {
>         tsBots = map handleBotEvents (tsBots state)
>         }
>   where
>     formatEvent getPlayerName
>                 (ThunderstoneEventPlayerAction playerId action) =
>         [getPlayerName playerId ++ " takes action: " ++ show action]
>     formatEvent getPlayerName (ThunderstoneEventRevealCards playerId hand) =
>         [getPlayerName playerId ++ " reveals cards: "
>                                 ++ unwords (map show hand)]
>     formatEvent getPlayerName (ThunderstoneEventDrawCards playerId cards) =
>         [getPlayerName playerId ++ " draws cards: "
>                                 ++ unwords (map show cards)]
>     formatEvent getPlayerName (ThunderstoneEventReshuffle playerId) =
>         [getPlayerName playerId ++ " reshuffles his or her discard pile."]
>     formatEvent getPlayerName (ThunderstoneEventPurchase playerId card) =
>         [getPlayerName playerId ++ " purchases " ++ show card ++ "."]
>     formatEvent getPlayerName
>                 (ThunderstoneEventUpgrade playerId oldHero newHero) =
>         [getPlayerName playerId ++ " levels up Hero from: "
>                                 ++ show (HeroCard oldHero) ++ " to: "
>                                 ++ show (HeroCard newHero) ++ "."]
>     formatEvent getPlayerName (ThunderstoneEventDiscard playerId card) =
>         [getPlayerName playerId ++ " discards a card."]
>     formatEvent getPlayerName (ThunderstoneEventDestroyCard playerId card) =
>         [getPlayerName playerId ++ " destroys a card."]
>     formatEvent getPlayerName
>                 (ThunderstoneEventUseEffect playerId card text) =
>         [getPlayerName playerId ++ " uses a card Effect: "
>                                 ++ show card ++ " " ++ text]
>     formatEvent getPlayerName
>                 (ThunderstoneEventBattleEffect playerId card text) =
>         [getPlayerName playerId ++ " activates a Battle Effect: "
>                                 ++ show card ++ " " ++ text]
>     formatEvent getPlayerName
>                 (ThunderstoneEventSpoilsEffect playerId card text) =
>         [getPlayerName playerId ++ " activates a Spoils Effect: "
>                                 ++ show card ++ " " ++ text]
>     formatEvent getPlayerName
>                 (ThunderstoneEventBorrowCard playerId card otherPlayerId) =
>         [getPlayerName playerId ++ " borrows " ++ show card
>                                 ++ " from " ++ getPlayerName otherPlayerId]
>     formatEvent getPlayerName (ThunderstoneEventPlayerStartsTurn playerId) =
>         [getPlayerName playerId ++ " begins turn."]
>     formatEvent getPlayerName (ThunderstoneEventEquip playerId hero weapon) =
>         [getPlayerName playerId ++ " equips " ++ show hero
>                                 ++ " with " ++ show weapon ++ "."]
>     formatEvent getPlayerName
>                 (ThunderstoneEventGainDungeonCard playerId rank card)
>       | rank > 0 =
>         [getPlayerName playerId ++ " gains from Rank " ++ show rank
>                                 ++ ": " ++ show card ++ "."]
>       | otherwise =
>         [getPlayerName playerId ++ " gains " ++ show card ++ "."]
>     formatEvent getPlayerName (ThunderstoneEventReturnMonster rank card) =
>         ["Returned from Rank " ++ show rank ++ " to bottom of Dungeon Deck: "
>                                ++ show card ++ "."]
>     formatEvent getPlayerName (ThunderstoneEventBreach card text) =
>         [show card ++ ", " ++ text]
>     formatEvent getPlayerName ThunderstoneEventDungeonHallChanged =
>         ["The Dungeon Hall has changed."]
>     formatEvent getPlayerName (ThunderstoneEventWinBattle
>                                    playerId rank card) =
>         [getPlayerName playerId ++ " defeats Rank " ++ show rank ++ ": "
>                                 ++ show card ++ "."]
>     formatEvent getPlayerName (ThunderstoneEventLoseBattle
>                                    playerId rank card) =
>         [getPlayerName playerId ++ " is defeated by Rank "
>                                 ++ show rank ++ ": " ++ show card ++ "."]
>     formatEvent getPlayerName (ThunderstoneEventAttack playerId rank card) =
>         [getPlayerName playerId ++ " attacks Rank " ++ show rank ++ ": "
>                                 ++ show card ++ "."]
>     formatEvent getPlayerName (ThunderstoneEventGameOver scores) =
>         ["Game over.  Scores:"]
>         ++ [" " ++ getPlayerName playerId ++ ": " ++ show score
>             | (playerId,score) <- scores]
>     handleBotEvents bot = bot {
>         botState = botHandleEvents (botState bot) events
>         }

> userInGame :: ThunderstoneGame -> UserId -> Bool
> userInGame state uid = uid `elem` [userId player | player <- tsPlayers state]

> getPlayerId :: ThunderstoneGame -> UserId -> Maybe PlayerId
> getPlayerId state uid =
>     fmap playerId (find ((uid ==) . userId) (tsPlayers state))

> getPlayerNames :: Game ThunderstoneGame (PlayerId -> String)
> getPlayerNames = do
>     state <- gameGetState
>     playerNames <- mapM getPlayerName (tsPlayers state)
>     let botNames = map getBotName (tsBots state)
>     return (maybe "(unknown)" id . flip lookup (playerNames ++ botNames))
>   where
>     getPlayerName player = do
>         name <- gameGetName (userId player)
>         return (playerId player,name)
>     getBotName bot = (botId bot,botName bot)

> runBots :: Game ThunderstoneGame ()
> runBots = do
>     state <- gameGetState
>     runBot (tsBots state)
>   where
>     runBot [] = return ()
>     runBot (bot:bots) = do
>         state <- gameGetState
>         maybe (runBot bots)
>               (botRan bot)
>               (botPerformActions (botState bot) (tsState state))
>     botRan bot (newBotState,events,newTsState) = do
>         updateBot bot { botState = newBotState }
>         state <- gameGetState
>         gameSetState state { tsState = newTsState }
>         broadcastEvents events
>         runBots
>     updateBot newBot = do
>         state <- gameGetState
>         gameSetState state { tsBots = map update (tsBots state) }
>       where
>         update oldBot
>           | botId oldBot == botId newBot = newBot
>           | otherwise = oldBot

> getNewStdGen :: Game ThunderstoneGame StdGen
> getNewStdGen = do
>     state <- gameGetState
>     let (seed,stdGen) = next (tsStdGen state)
>     gameSetState state { tsStdGen = stdGen  }
>     return (mkStdGen seed)

> data BotState = BotState {
>     botInit :: StdGen -> PlayerId -> [PlayerId] -> BotState,
>     botHandleEvents :: [ThunderstoneEvent] -> BotState,
>     botPerformActions :: ThunderstoneState -> Maybe (BotState,
>                                                      [ThunderstoneEvent],
>                                                      ThunderstoneState)
>     }

> botList :: [(String,BotState)]
> botList = [("random",simpleBotFactory randomBotInit),
>            ("shopper",simpleBotFactory shopperBotInit),
>            ("battler",simpleBotFactory battlerBotInit)]

> data SimpleBot = SimpleBot {
>     simpleBotId :: PlayerId,
>     simpleBotStdGen :: StdGen
>     }

> simpleBotFactory :: (StdGen -> PlayerId -> [PlayerId] -> BotState)
>                  -> BotState
> simpleBotFactory init = BotState {
>     botInit = init,
>     botHandleEvents = error "simpleBotFactory:botHandleEvents",
>     botPerformActions = error "simpleBotFactory:botPerformActions"
>     }

> simpleBotInit :: (SimpleBot -> BotState)
>               -> StdGen -> PlayerId -> [PlayerId] -> BotState
> simpleBotInit makeBotState stdGen botId playerIds =
>     makeBotState SimpleBot {
>         simpleBotId = botId,
>         simpleBotStdGen = stdGen
>         }

> randomBotInit :: StdGen -> PlayerId -> [PlayerId] -> BotState
> randomBotInit = simpleBotInit botState
>   where
>     botState bot = BotState {
>         botInit = error "randomBotInit:botInit",
>         botHandleEvents = handleEvents bot,
>         botPerformActions = performActions bot
>         }
>     handleEvents bot events = botState bot
>     performActions bot tsState
>       | null options = Nothing
>       | otherwise =
>             doAction (options !! randomIndex)
>       where
>         options =
>             playerStateOptions $ thunderstonePlayerState tsState playerId
>         playerId = simpleBotId bot
>         (randomIndex,newStdGen) =
>             randomR (0,length options - 1) (simpleBotStdGen bot)
>         doAction action =
>             maybe Nothing
>                   (\ (newTsState,events) ->
>                           Just (botState bot { simpleBotStdGen = newStdGen },
>                                 events,newTsState))
>                   (thunderstoneTakeAction tsState playerId action)

> shopperBotInit :: StdGen -> PlayerId -> [PlayerId] -> BotState
> shopperBotInit = simpleBotInit botState
>   where
>     botState bot = BotState {
>         botInit = error "shopperBotState:botInit",
>         botHandleEvents = handleEvents bot,
>         botPerformActions = performActions bot
>         }
>     handleEvents bot events = botState bot
>     performActions bot tsState
>       | null options = Nothing
>       | VisitVillage `elem` options = doAction VisitVillage
>       | FinishUsingVillageEffects `elem` options =
>             doAction FinishUsingVillageEffects
>       | PurchaseCard `elem` options =
>             doAction PurchaseCard
>       | EndTurn `elem` options =
>             doAction EndTurn
>       | otherwise =
>             doAction (last options)
>       where
>         options =
>             playerStateOptions $ thunderstonePlayerState tsState playerId
>         playerId = simpleBotId bot
>         doAction action =
>             maybe Nothing
>                   (\ (newTsState,events) ->
>                           Just (botState bot,events,newTsState))
>                   (thunderstoneTakeAction tsState playerId action)

> battlerBotInit :: StdGen -> PlayerId -> [PlayerId] -> BotState
> battlerBotInit = simpleBotInit botState
>   where
>     botState bot = BotState {
>         botInit = error "battlerBotState:botInit",
>         botHandleEvents = handleEvents bot,
>         botPerformActions = performActions bot
>         }
>     handleEvents bot events = botState bot
>     performActions bot tsState
>       | null options = Nothing
>       | EnterDungeon `elem` options = doAction EnterDungeon
>       | AttackMonster `elem` options = doAction AttackMonster
>       | EndTurn `elem` options =
>             doAction EndTurn
>       | otherwise =
>             doAction (last options)
>       where
>         options =
>             playerStateOptions $ thunderstonePlayerState tsState playerId
>         playerId = simpleBotId bot
>         doAction action =
>             maybe Nothing
>                   (\ (newTsState,events) ->
>                           Just (botState bot,events,newTsState))
>                   (thunderstoneTakeAction tsState playerId action)
