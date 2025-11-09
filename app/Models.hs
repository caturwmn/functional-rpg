{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE CPP               #-}
module Models where

import Miso
import Miso.Lens

-- | Component model state
data Model
  = Model
  { _event :: Event
  , _currentScenario :: Scenario
  } deriving (Show, Eq)

event :: Lens Model Event
event = lens _event $ \record field -> record { _event = field }

currentScenario :: Lens Model Scenario
currentScenario = lens _currentScenario $ \record field -> record { _currentScenario = field }

-- | App events
data Action
  = Next
  | Choose String
  deriving (Show, Eq)

  -- | Empty application state
emptyModel :: Model
emptyModel = Model (checkEvent greeting) greeting

-- | Updates model, optionally introduces side effects
updateModel :: Action -> Transition Model Action
updateModel = \case
  Next        -> do
    scenario <- use currentScenario
    currentScenario .= iterateScenario scenario
    nextScenario <- use currentScenario
    event .= checkEvent nextScenario

  Choose selection-> do
    choice <- use event
    currentScenario .= choiceSelection choice selection
    nextScenario <- use currentScenario
    event .= checkEvent nextScenario

-- | Data for status effect
data StatusEffect
  -- name type multiplier
  = AttackStatus String String Double
  -- name
  | StatusCleanse String
  deriving (Show, Eq)

-- | To apply status effect from attack
applyStatus :: [StatusEffect] -> [StatusEffect] -> [StatusEffect]
applyStatus [] statuses = statuses
applyStatus (x@(AttackStatus _ _ _):xs) statuses
  = applyStatus xs (x:statuses)
applyStatus (x@(StatusCleanse _):xs) statuses
  = applyStatus xs (removeStatus x statuses)

-- | To remove status effect from list of status effect
removeStatus :: StatusEffect -> [StatusEffect] -> [StatusEffect]
removeStatus (StatusCleanse target) [] = []
removeStatus (StatusCleanse target) (x@(AttackStatus name _ _):xs)
  | target == name = xs
  | otherwise = x:(removeStatus (StatusCleanse target) xs)

-- | To get list of multiplier from statuses
applyStatusMultiplier :: String -> [StatusEffect] -> [Double]
applyStatusMultiplier target [] = []
applyStatusMultiplier target (x@(AttackStatus name aType multiplier):xs)
  | aType == "All" = multiplier:(applyStatusMultiplier target xs)
  | target == aType = multiplier:(applyStatusMultiplier target xs)
  | otherwise = 1.0:(applyStatusMultiplier target xs)

-- | Multiply a list of double to 
getMultiplier :: [Double] -> Double
getMultiplier multipliers = foldr (*) 1 multipliers

data Move
  -- name afflicted-status damage
  = Attack String String [StatusEffect] Double
  -- name
  | Recover String
  -- to be worked on
  | Reaction [Move]
  | Opportunity [Move]
  deriving (Show, Eq)

reduceHP :: Double -> Double -> String -> [StatusEffect] -> Double
reduceHP hp damage aType statuses 
  = hp - ((getMultiplier (applyStatusMultiplier aType statuses)) * damage)

attack :: String -> Move -> 
  Stat -> (Combat Stat)
attack target (Attack aName aType aStatuses damage) (Stat name hp statuses moves)
  | target == name && name == "player" 
  = Player (Stat name 
    (reduceHP hp damage aType statuses) 
    (applyStatus aStatuses statuses)
    moves)
  | name == "player"
  = Player (Stat name hp statuses moves)
  | target == name
  = Enemy (Stat name 
    (reduceHP hp damage aType statuses) 
    (applyStatus aStatuses statuses)
    moves)
  | otherwise 
  = Enemy (Stat name hp statuses moves)

data Stat
  = Stat String Double [StatusEffect] [Move]
  deriving (Show, Eq)

data Combat a 
  = Player a
  | Enemy a
  | Combat (Combat a) [Combat a]
  deriving (Show, Eq)

instance Functor Combat where
  fmap f (Player a) = Player (f a)
  fmap f (Enemy a) = Enemy (f a)
  fmap f (Combat player enemies) = Combat (fmap f player) (map (fmap f) enemies)

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

data Event
  = Dialog String String 
  | Choice String Scenario String Scenario
  deriving (Show, Eq)

choiceSelection :: Event -> String -> Scenario
choiceSelection (Choice opt1 left opt2 right) selection 
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
  [ Dialog "A" "Hello"
  , Dialog "A" "Welcome!" 
  , Choice "Me?" left "..." right
  ]

left = Scenario 
  [ Dialog "A" "Yes, i'm calling you"
  , Dialog "Leif" "Why?" 
  ]

right = Scenario 
  [ Dialog "A" "Are you ignoring me?"
  , Dialog "Leif" "Oh, i don't know your talking to me"
  ]

-- StatusEffect
mark = AttackStatus "mark" "All" 1.5
vulnerable = AttackStatus "vulnerable" "All" 2.0
cureMark = StatusCleanse "mark" 

slash = Attack "slash" "slash" [] 5.0
shieldStrike = Attack "shield strike" "slash" [vulnerable] 8.0
recoil = Attack "recoil" "null" [vulnerable] 0.0
shoot = Attack "shoot" "pierce" [] 4.0
bite = Attack "bite" "pierce" [mark] 3.0

player :: Combat Stat
player = Player (Stat "player" 10 [] [shieldStrike, shoot, slash])

enemy :: Combat Stat
enemy = Enemy (Stat "wolf 1" 10 [] [bite])
enemy2 = Enemy (Stat "wolf 2" 10 [] [bite])

combatScenario = do 
  start <- Combat player [enemy, enemy2]
  first <- (attack "wolf 1" shieldStrike) start
  second <- (attack "wolf 1" shoot) first
  third <- (attack "player" bite) second
  (attack "player" bite) third
