{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE CPP               #-}
module Models where
import System.Random
import Miso
import Miso.Lens
import qualified Miso.Lens as Lens
import Miso.Effect
import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (liftIO)

-- | Component model state
data Model
  = Model
  { _event :: Event
  , _currentScenario :: Scenario
  , _isCombatInitiated :: Bool
  , _isPlayerTurn :: Bool
  , _combatInfo :: String
  , _selectedSkill :: String
  , _combatStatus :: (Combat Stat)
  , _combatOrder :: [Combat Stat]
  , _randomGen :: StdGen
  , _accelerateCounter :: Integer
  } deriving (Show, Eq)

event :: Lens Model Event
event = lens _event $ \record field -> record { _event = field }

currentScenario :: Lens Model Scenario
currentScenario = lens _currentScenario $ \record field -> record { _currentScenario = field }

isCombatInitiated :: Lens Model Bool
isCombatInitiated = lens _isCombatInitiated $ \record field -> record { _isCombatInitiated = field }

isPlayerTurn :: Lens Model Bool
isPlayerTurn = lens _isPlayerTurn $ \record field -> record { _isPlayerTurn = field }

combatInfo :: Lens Model String
combatInfo = lens _combatInfo $ \record field -> record { _combatInfo = field }

selectedSkill :: Lens Model String
selectedSkill = lens _selectedSkill $ \record field -> record { _selectedSkill = field }

combatStatus :: Lens Model (Combat Stat)
combatStatus = lens _combatStatus $ \record field -> record { _combatStatus = field }

combatOrder :: Lens Model [Combat Stat]
combatOrder = lens _combatOrder $ \record field -> record { _combatOrder = field }

randomGen :: Lens Model StdGen
randomGen = lens _randomGen $ \record field -> record { _randomGen = field }

accelerateCounter :: Lens Model Integer
accelerateCounter = lens _accelerateCounter $ \record field -> record { _accelerateCounter = field }

-- | App events
data Action
  = Next
  | Choose String
  | InitiateCombat
  | ChangeInfo String
  | ProcessTurn
  | SelectSkill String
  | PerformAttack String String
  | AccelerateTurn
  | ExitCombat String
  | ScheduleEvent Action

  deriving (Show, Eq)

emptyStat :: Stat
emptyStat = Stat "" 0.0 [] [] ""

  -- | Empty application state
emptyModel :: StdGen -> Model
emptyModel gen = Model (checkEvent greeting) greeting 
  False False "" "" (Player emptyStat) [] gen 0

-- | Updates model, optionally introduces side effects
updateModel :: Action -> Effect ROOT Model Action
updateModel action = do
  model <- get
  case action of
    Next        ->
      let
        scenario = model ^. currentScenario
        nextScenario = iterateScenario scenario
        nextEvent = checkEvent nextScenario
        newModel =
          model
            & Lens.set currentScenario nextScenario
            & Lens.set event nextEvent  
      in put newModel

    Choose selection ->
      let  
        choice = model ^. event
        nextScenario = choiceSelection choice selection
        nextEvent = checkEvent nextScenario
        newModel = 
          model
            & Lens.set currentScenario nextScenario
            & Lens.set event nextEvent
      in put newModel
        
    InitiateCombat -> 
      let
        (CombatEvent currentCombat winScenario loseScenario)
          = model ^. event
        turnOrder = buildTurnOrder currentCombat
        newModel = 
          model
            & Lens.set isCombatInitiated True
            & Lens.set combatStatus currentCombat
            & Lens.set combatOrder turnOrder
            & Lens.set isPlayerTurn True
      in put newModel

    PerformAttack attackName targetName ->
      let
        currentOrder = model ^. combatOrder
        nextOrder = cycleList currentOrder

        currentStatus = model ^. combatStatus
        selectedMove = getMove attackName moveList
        newCombatStatus = currentStatus >>= (attack targetName selectedMove)

        temporaryOrder = buildTurnOrder newCombatStatus
        targetEntity = getHead nextOrder
        newTurnOrder = cycleTillMatch targetEntity temporaryOrder

        statusCheck = checkCombat newCombatStatus
        msg = "Used " ++ attackName ++ " on " ++ targetName

        newModel = model
          & Lens.set combatOrder newTurnOrder
          & Lens.set combatInfo msg
          & Lens.set combatStatus newCombatStatus
          & Lens.set isPlayerTurn False
      in
        case statusCheck of
          "Continue" -> 
            newModel <# scheduleEvent (ProcessTurn)
          "Win" -> 
            newModel <# scheduleEvent (ExitCombat "Win")
          "Lose" -> 
            newModel <# scheduleEvent (ExitCombat "Lose")

    AccelerateTurn -> 
      let 
        counter = (model ^. accelerateCounter) + 1
        currentStatus = model ^. combatStatus
        newModel =
          model
            & Lens.set accelerateCounter counter
      in
        case (checkCombat currentStatus) of
          "Continue" ->
            model <# pure (ProcessTurn)
          "Win" -> 
            model <# pure (ExitCombat "Win")
          "Lose" -> 
            model <# pure (ExitCombat "Lose")

    ChangeInfo msg ->
      let
        newModel = 
          model
            & Lens.set combatInfo msg
      in put newModel

    SelectSkill skill -> 
      let
        msg = "Choosen " ++ skill
        newModel = 
          model
            & Lens.set selectedSkill skill
            & Lens.set combatInfo msg
      in put newModel
    
    ExitCombat result ->
      let
        counter = model ^. accelerateCounter
      in case counter of
        0 ->
          let 
            currentEvent = model ^. event
            (CombatEvent currentCombat winScenario loseScenario) = currentEvent
            nextScenario =
              case result of
                "Win" -> winScenario
                "Lose" -> loseScenario
            nextEvent = checkEvent nextScenario
            newModel =
              model
                & Lens.set currentScenario nextScenario
                & Lens.set event nextEvent
                & Lens.set isCombatInitiated False
          in put newModel

        _ ->
          let
            newModel =
              model
                & Lens.set accelerateCounter (counter - 1)
          in put newModel
    
    ProcessTurn ->
      let
        counter = model ^. accelerateCounter
        order@(x:xs) = model ^. combatOrder
      in case counter of
        0 ->
          case x of
            -- Skip turn
            Corpse _ ->
              let
                nextOrder = cycleList order
                newModel =
                  model
                    & Lens.set combatOrder nextOrder
              in
                newModel <# scheduleEvent (ProcessTurn)

            -- Allow player action
            Player _ ->
              put $
                model
                  & Lens.set combatInfo "Your turn"
                  & Lens.set isPlayerTurn True

            -- Enemy action
            Enemy (Stat name _ _ moves _) ->
              let
                nextOrder = cycleList order
                
                -- Get a random Double between 0 and 1
                gen = model ^. randomGen
                (randDouble, newRandomGen) = random gen :: (Double, StdGen)

                -- Enemy perform random attack
                moveName = selectRandomly randDouble moves
                selectedMove = getMove moveName moveList
                currentStatus = (model ^. combatStatus)
                nextStatus = currentStatus >>= (attack "player" selectedMove)

                -- Build new turn order
                temporaryOrder = buildTurnOrder nextStatus
                targetEntity = getHead nextOrder
                newTurnOrder = cycleTillMatch targetEntity temporaryOrder
      
                -- Create new model
                msg = name ++ " attacked player with " ++ moveName
                newModel = 
                  model
                    & Lens.set combatOrder newTurnOrder
                    & Lens.set combatInfo msg
                    & Lens.set randomGen newRandomGen
                    & Lens.set combatStatus nextStatus
              in
                case (checkCombat currentStatus) of
                  "Continue" -> 
                    newModel <# scheduleEvent (ProcessTurn)
                  "Win" -> 
                    newModel <# scheduleEvent (ExitCombat "Win")
                  "Lose" ->
                    newModel <# scheduleEvent (ExitCombat "Lose")
        _ -> 
          let
            newModel =
              model
                & Lens.set accelerateCounter (counter - 1)
          in put newModel

scheduleEvent :: Action -> JSM Action
scheduleEvent action = do
  -- Delay by 1 seconds
  liftIO $ threadDelay 1000000
  pure (action)

------------------------------UTIL-----------------------------------
selectRandomly :: Double -> [a] -> a
selectRandomly num (x:xs)
  | num - 1.0 <= 0.0 = x
  | otherwise = selectRandomly (num - 1.0) xs 

cycleList :: [a] -> [a]
cycleList (x:xs) = xs ++ [x]

getHead :: [a] -> a
getHead (x:_) = x

getStatName :: (Combat Stat) -> String
getStatName (Player (Stat name _ _ _ _)) = name
getStatName (Enemy (Stat name _ _ _ _)) = name
getStatName (Corpse (Stat name _ _ _ _)) = name

nameMatches :: (Combat Stat) -> (Combat Stat) -> Bool
nameMatches target current = (getStatName target) == (getStatName current)

cycleTillMatch :: (Combat Stat) -> [Combat Stat] -> [Combat Stat]
cycleTillMatch target (x:xs)
  | nameMatches target x = x:xs
  | otherwise = cycleTillMatch target (xs ++ [x])

-----------------------------COMBAT----------------------------------

-- | Data for list of move
data MoveList
  = MoveElement String Move
  | MoveList [MoveList]
  deriving (Show, Eq)

getMove :: String -> MoveList -> Move
getMove name (MoveList ((MoveElement moveName move):xs))
  | name == moveName = move
  | otherwise = (getMove name (MoveList xs))

-- | Data for status effect
data StatusEffect
  -- name type multiplier
  = DamageStatus String String Double
  -- name
  | StatusCleanse String
  deriving (Show, Eq)

-- | To apply status effect from attack
applyStatus :: [StatusEffect] -> [StatusEffect] -> [StatusEffect]
applyStatus [] statuses = statuses
applyStatus (x@(DamageStatus _ _ _):xs) statuses
  = applyStatus xs (x:statuses)
applyStatus (x@(StatusCleanse _):xs) statuses
  = applyStatus xs (removeStatus x statuses)

-- | To remove status effect from list of status effect
removeStatus :: StatusEffect -> [StatusEffect] -> [StatusEffect]
removeStatus (StatusCleanse target) [] = []
removeStatus (StatusCleanse target) (x@(DamageStatus name _ _):xs)
  | target == name = xs
  | otherwise = x:(removeStatus (StatusCleanse target) xs)

-- | To get damage multiplier from statuses
getMultiplier :: String -> [StatusEffect] -> Double
getMultiplier target [] = 1
getMultiplier target (x@(DamageStatus name aType multiplier):xs)
  | aType == "All" = multiplier * (getMultiplier target xs)
  | target == aType = multiplier * (getMultiplier target xs)
  | otherwise = 1.0 * (getMultiplier target xs)

data Move
  -- name afflicted-status damage
  = Attack String String [StatusEffect] Double
  -- name
  | Recover String
  deriving (Show, Eq)

reduceHP :: Double -> Double -> String -> [StatusEffect] -> Double
reduceHP hp damage aType statuses 
  = hp - ((getMultiplier aType statuses) * damage)

attack :: String -> Move -> Stat -> (Combat Stat)
attack target move@(Attack _ aType _ damage) stat@(Stat name hp statuses _ _)
  | hp <= 0.0
    = Corpse stat
  | target == name
    = checkAlive (reduceHP hp damage aType statuses) move stat
  | name == "player"
    = Player stat
  | otherwise 
    = Enemy stat

checkAlive :: Double -> Move -> Stat -> (Combat Stat)
checkAlive finalHp (Attack _ _ aStatuses _) a@(Stat name hp statuses moves source)
  | finalHp > 0.0 =
    case name of
      "player" ->
        Player (Stat name finalHp (applyStatus aStatuses statuses) moves source)
      _ -> 
        Enemy (Stat name finalHp (applyStatus aStatuses statuses) moves source)
  | otherwise = Corpse (Stat name finalHp (applyStatus aStatuses statuses) moves source)

checkCombat :: (Combat Stat) -> String
checkCombat (Combat player enemies) 
  = case ((checkLose player) + (checkWin enemies)) of
    0 -> "Continue" -- Continue battle
    1 -> "Lose" -- Lose
    _ -> "Win" -- Win

checkLose :: (Combat Stat) -> Integer
checkLose (Player _) = 0
checkLose (Corpse _) = 1

checkWin :: [Combat Stat] -> Integer
checkWin [] = 2
checkWin ((Corpse _):xs) = checkWin xs
checkWin ((Enemy _):xs) = 0

buildTurnOrder :: (Combat Stat) -> [Combat Stat]
buildTurnOrder (Combat player enemies)
  = player:enemies

data Stat
  = Stat String Double [StatusEffect] [String] String
  deriving (Show, Eq)

data Combat a 
  = Player a
  | Enemy a
  | Corpse a
  | Combat (Combat a) [Combat a]
  deriving (Show, Eq)

instance Functor Combat where
  fmap f (Player a) = Player (f a)
  fmap f (Enemy a) = Enemy (f a)
  fmap f (Combat player enemies) 
    = Combat (fmap f player) (map (fmap f) enemies)

instance Applicative Combat where
  pure x = Combat (Player x) []
  (Player f) <*> (Player a) = Player (f a)
  (Enemy f) <*> (Enemy a) = Enemy (f a)
  (Combat p1 fs) <*> (Combat p2 as) = 
    Combat (p1 <*> p2) [x <*> y | x <- fs, y <- as]

instance Monad Combat where
  Player a >>= f =
    f a
  Enemy a >>= f =
    f a
  Corpse a >>= f =
    f a
  Combat player enemies >>= f = 
    Combat (player >>= f) (fmap (>>= f) enemies)

-----------------------------SCENARIO----------------------------------

data Event
  = Dialog String String 
  | Choice String String String Scenario String Scenario
  | CombatEvent (Combat Stat) Scenario Scenario
  deriving (Show, Eq)

choiceSelection :: Event -> String -> Scenario
choiceSelection (Choice speaker msg opt1 left opt2 right) selection 
  = case selection of
    "Left" -> left
    "Right" -> right

data Scenario
  = Scenario [Event]
  deriving (Show, Eq)

checkEvent :: Scenario -> Event
checkEvent (Scenario (x:_)) = x

iterateScenario :: Scenario -> Scenario
iterateScenario (Scenario (x:xs)) = Scenario xs
iterateScenario (Scenario []) = Scenario []

greeting = Scenario 
  [ Dialog "Player" "This forest is huge"
  , Dialog "Player" "Are those wolves?"
  , Choice "Player" "Should i just go away?" "yes" left "no" right
  ]

left = Scenario 
  [ Dialog "Player" "Still 2 hours away from Arlington, better stay away"
  , Dialog "" "Game Over" 
  ]

right = Scenario 
  [ Dialog "Player" "I AM low on provision after all, let's see what happens"
  , CombatEvent combatScenario onWin onLose
  ]

onWin = Scenario 
  [ Dialog "Player" "Phew, stil got it"
  ]

onLose = Scenario 
  [ Dialog "Player" "Shouldn't have tempted fate"
  ]

-- StatusEffect
mark = DamageStatus "mark" "All" 1.5
vulnerable = DamageStatus "vulnerable" "All" 2.0
cureMark = StatusCleanse "mark" 

-- Moves
slash = Attack "slash" "slash" [] 5.0
shieldStrike = Attack "shield strike" "slash" [vulnerable] 8.0
shoot = Attack "shoot" "pierce" [] 4.0
bite = Attack "bite" "pierce" [mark] 1.0

player :: Combat Stat
player = Player (Stat "player" 10.0 [] ["Shield Strike", "Slash"] "")

enemy :: Combat Stat
enemy = Enemy (Stat "wolf 1" 10.0 [] ["Bite"] "static/wolf.png")
enemy2 = Enemy (Stat "wolf 2" 10.0 [] ["Bite"] "static/wolf.png")

combatScenario :: Combat Stat
combatScenario = Combat player [enemy, enemy2]

moveList :: MoveList
moveList = MoveList
  [ MoveElement "Slash" slash
  , MoveElement "Shield Strike" shieldStrike
  , MoveElement "Bite" bite
  ]
