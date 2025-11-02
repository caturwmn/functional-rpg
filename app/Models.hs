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