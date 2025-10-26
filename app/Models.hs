{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE CPP               #-}
module Models where

import Miso
import Miso.Lens

-- | Component model state
data Model
  = Model
  { _dialog :: String, 
  _currentScenario :: Scenario
  } deriving (Show, Eq)

dialog :: Lens Model String
dialog = lens _dialog $ \record field -> record { _dialog = field }

currentScenario :: Lens Model Scenario
currentScenario = lens _currentScenario $ \record field -> record { _currentScenario = field }

-- | App events
data Action
  = Next
  deriving (Show, Eq)

  -- | Empty application state
emptyModel :: Model
emptyModel = Model "" scenario

-- | Updates model, optionally introduces side effects
updateModel :: Action -> Transition Model Action
updateModel = \case
  Next        -> do
    s <- use currentScenario
    dialog .= checkEvent s
    currentScenario .= (iterateScenario s)

data Event
  = Dialog String String
  deriving (Show, Eq)

getEvent :: Event -> String
getEvent (Dialog speaker sentence) = sentence

data Scenario
  = Scenario [Event]
  deriving (Show, Eq)

checkEvent :: Scenario -> String
checkEvent (Scenario (x:_)) = getEvent x

iterateScenario :: Scenario -> Scenario
iterateScenario (Scenario (x:xs)) = Scenario xs
iterateScenario (Scenario []) = Scenario []

scenario = Scenario 
  [ Dialog "A" "Hello"
  , Dialog "A" "Welcome!" 
  ]