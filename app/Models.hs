{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE CPP               #-}
module Models where

import System.Random
import Miso
import Miso.Lens

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

-- | App events
data Action
  = Next
  | Choose String
  | InitiateCombat
  | ChangeInfo String
  | ProcessTurn
  | SelectSkill String
  | PerformAttack String String
  deriving (Show, Eq)

emptyStat :: Stat
emptyStat = Stat "" 0.0 [] [] ""

  -- | Empty application state
emptyModel :: StdGen -> Model
emptyModel gen = Model (checkEvent greeting) greeting 
  False False "" "" (Player emptyStat) [] gen

-- | Updates model, optionally introduces side effects
updateModel :: Action -> Transition Model Action
updateModel = \case
  Next        -> do
    scenario <- use currentScenario
    currentScenario .= iterateScenario scenario
    nextScenario <- use currentScenario
    event .= checkEvent nextScenario

  Choose selection -> do
    choice <- use event
    currentScenario .= choiceSelection choice selection
    nextScenario <- use currentScenario
    event .= checkEvent nextScenario

  InitiateCombat -> do
    isCombatInitiated .= True
    currentEvent <- use event
    let (CombatEvent currentCombat winScenario loseScenario) = currentEvent
    combatStatus .= currentCombat
    -- event .= (Choice "" "" "win" winScenario "lose" loseScenario)
    combatOrder .= buildTurnOrder currentCombat
    isPlayerTurn .= True

  PerformAttack attack target -> do
    combatInfo .= "attempt to use " ++ attack ++ " on " ++ target

  ChangeInfo msg -> do
    combatInfo .= msg

  SelectSkill skill -> do
    selectedSkill .= skill

processTurn :: [Combat Stat] -> Transition Model ()
processTurn (x@(Player _):xs) = pure ()
processTurn order@(x@(Enemy (Stat _ _ _ moves _)):xs)
  = do
    gen <- use randomGen
    let (randDouble, newRandomGen) = random gen :: (Double, StdGen)
    randomGen .= newRandomGen
    combatStatus %= (>>= (attack "player" bite))
    processTurn (cycleList order)


------------------------------UTIL-----------------------------------
selectRandomly :: Double -> [a] -> a
selectRandomly num (x:xs)
  | num - 1.0 <= 0 = x
  | otherwise = selectRandomly (num - 1.0) xs 

cycleList :: [a] -> [a]
cycleList (x:xs) = xs ++ [x]

getHead :: [a] -> a
getHead (x:_) = x
-----------------------------COMBAT----------------------------------

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
  | otherwise = Corpse a

checkCombat :: (Combat Stat) -> String
checkCombat (Combat player enemies) 
  = case ((checkLose player) + (checkWin enemies)) of
    0 -> "Continue" -- Continue battle
    1 -> "Right" -- Lose
    _ -> "Left" -- Win

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

slash = Attack "slash" "slash" [] 5.0
shieldStrike = Attack "shield strike" "slash" [vulnerable] 8.0
recoil = Attack "recoil" "null" [vulnerable] 0.0
shoot = Attack "shoot" "pierce" [] 4.0
bite = Attack "bite" "pierce" [mark] 3.0

player :: Combat Stat
player = Player (Stat "player" 10 [] ["shieldStrike", "shoot", "slash"] "")

enemy :: Combat Stat
enemy = Enemy (Stat "wolf 1" 10 [] ["bite"] "static/wolf.png")
enemy2 = Enemy (Stat "wolf 2" 10 [] ["bite"] "static/wolf.png")

combatScenario :: Combat Stat
combatScenario = Combat player [enemy, enemy2]
