# Functional-RPG
Repository containing an attempt to recreate simple rpg features with functional programming paradigm. Requires GHCJS, GHCup, and Caball to compile as releasable build. You're welcome to try experimenting with these code to expand the features.

## Important
Haskell Miso uses GHCJS to compile the app, therefore requiring linux. If you're using windows, it is recommended to try to install wsl by following [microsoft's guide on installing wsl](https://learn.microsoft.com/en-us/windows/wsl/install). You may also need to install Haskell, Caball, and GHCJS from [ghcup](https://www.haskell.org/ghcup/) or by using [nix](https://nixos.org/)

## Haskell Miso Recommended Sources
For documentation and setup, check [dmjio's Miso library](https://github.com/dmjio/miso).
For various examples, check  [haskell-miso's organization repository](https://github.com/haskell-miso).

# Code Explanation
The app is divided into 3 modules which are [Main](app\Main.hs), [View](app\Views.hs), and [Model](app\Models.hs). Main contains the running app, View manages UI related code, and Models handle the logic off the app.

## Main
Main is where the other modules are imported and combined together to run the main app.
```haskell
{-# LANGUAGE CPP               #-}
module Main where
import System.Random
import Miso
import Views 
  ( viewModel
  )
import Models 
  ( Model
  , Action
  , emptyModel
  , updateModel 
  )

-- | `component` takes as arguments the initial model, 
-- | update function, and view function
main :: IO ()
main = do
  -- | getStdGen is used to perform random calculations and must be
  -- | setup inside of IO. 
  gen <- getStdGen
  let initialModel = emptyModel gen
  run (startApp (component initialModel updateModel viewModel))

-- | WASM export, required when compiling w/ the WASM backend.
#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif
```

The function called main will construct the necessary app from component, an empty model (initial app state), a model update function, and the view model of the app. 
The empty model of the app uses StdGen from System.Random to perform random numbers calculation and needs to be set inside of an IO. If you don't require randomness, you can delete randomGen and it's lens from Models.hs and do:
```haskell
app :: App Model Action
app = component emptyModel updateModel viewModel

main :: IO ()
main = run (startApp)
```

## Views
Views will build the UI of the app by parsing a series of function into HTML after compilation.

The main function that is used for building the UI is:
```haskell
-- | Constructs a virtual DOM from a model
viewModel :: Model -> View Model Action
viewModel m =
  H.html_ [] 
    [ H.head_ [] 
      [ H.style_ [] (CSS.renderStyleSheet sheet)
      ]
    , H.body_ [] 
      [ H.div_ [P.class_ "screen"] (chooseSection m)
      ]
    ]
```
which is equivalent to:
```html
<html>
<head>
  <style>
    /* your style sheet  */
  </style>
</head>

<body>
  <div class="screen">
    <!-- result of function chooseSection m -->
  </div>
</body>
</html>
```

### Style Sheet
The function sheet contains the style sheet used for the UI which is built in this code:
```haskell
sheet :: StyleSheet
sheet = 
  CSS.sheet_
    [ CSS.selector_ ".screen"
      [ CSS.height "600px"
      , CSS.width "800px"
      , CSS.border "1px solid black"
      , CSS.backgroundColor C.antiquewhite
      , CSS.position "relative"
      ]
    , CSS.selector_ ".dialog-box"
      [ CSS.height "270px"
      , CSS.width "766px"
      , CSS.backgroundColor C.white
      , CSS.opacity "0.8"
      , CSS.padding "10px" 
      , CSS.border "2px solid goldenrod"
      , CSS.position "relative"
      ]
    -- , etc
    ]
```
which is equivalent to:
```html
<style>
  .screen{
      height: 600px;
      width: 800px;
      border: 1px solid black;
      background-color: antiquewhite;
      position: relative;
    }

  .dialog-box {
      height: 270px;
      background-color: white;
      opacity: 0.8;
      padding: 10px;
      position: relative;
      width: 766px;
      border: 2px solid goldenrod;
    }
</style>
```

## Models
Model module stores most of the logical operation used by the app. There are various functions that are important for a haskell miso app.

### model
The model structure handles most of the app's state. An initial model is required to start the app.

### miso lenses
Miso lenses allow abstraction of get and set operation for model fields. This also allows the values from the field to be accessed without declaring a pointer to the model

### action
This stores the possible action that can be triggered by the view layer and optionally side effects that may be triggered by update model function. It is commonly used for case syntax in update model to tell it what to do given an action.

### updateModel
The updateModel function will handle all action calls from the view/Virtual DOM layer. This function will return a new model that stores the new state for the app after an action is called and optionally introduces side effect which can be another action.

## Functional Programming Implementation
There are a few examples of functional programming technique used in this app.

### High Order Function
High order functions are function that use function as an input, output, or both. They simplify the creation of complex functions that can perform operations on nested data structure like lists. They also allow complex function to be separated as simpler functions and can make the function more readable. For example, damage multiplier calculation is like this:
```haskell
-- | To get damage multiplier from statuses
getMultiplier :: String -> [StatusEffect] -> Double

-- Normal way
getMultiplier target [] = 1
getMultiplier target (x@(DamageStatus name aType multiplier):xs)
  | aType == "All" = multiplier * (getMultiplier target xs)
  | target == aType = multiplier * (getMultiplier target xs)
  | otherwise = 1.0 * (getMultiplier target xs)

-- High Order Function
getMultiplier target statuses=
  foldl checkMultiplier 1.0 statuses
  where
    -- Check multiplier definition
    checkMultiplier total (DamageStatus _ aType multiplier)
      | aType == "All"   = total * multiplier
      | aType == target  = total * multiplier
      | otherwise        = total
``` 
High Order Functions also allows abstraction and reuse of functions to do different things like with foldl:
```haskell
-- sum all content of list
foldl (+) 0 [3,5,1]
-- 3 + 5 + 1 + 0 = 9

-- multiply all content of list
foldl (*) 1 [3,5,1]
-- 3 * 5 * 1 * 1 = 15
```

### Function Composition
Function composition is the act of combining multiple functions to create a more complex function. Like high order function, This allows complex function to be separated into multiple simpler function and can make the function more readable. For example, in the function to update turn order, rather than this:
```haskell
-- Build new turn order
updateTurnOrder :: [Combat Status] -> Combat Status -> [Combat Status]
updateTurnOrder (x:_) (Combat player enemies) = 
  case x of
    (Player (Stat target _ _ _ _)) -> cycleTillMatch target player:enemies
      where
        cycleTillMatch target (y:ys)
          case y of
            (Player (Stat  name _ _ _)) ->
              | target == name == y:ys
              | otherwise = cycleTillMatch (ys ++ [y])
            (Enemy (Stat name _ _ _ _)) ->
              | target == name == y:ys
              | otherwise = cycleTillMatch (ys ++ [y])
            (Corpse (Stat name _ _ _ _)) ->
              | target == name == y:ys
              | otherwise = cycleTillMatch (ys ++ [y])
    (Enemy (Stat target _ _ _ _)) -> cycleTillMatch target player:enemies
      where
          cycleTillMatch target (y:ys)
            case y of
              (Player (Stat  name _ _ _)) ->
                | target == name == y:ys
                | otherwise = cycleTillMatch (ys ++ [y])
              (Enemy (Stat name _ _ _ _)) ->
                | target == name == y:ys
                | otherwise = cycleTillMatch (ys ++ [y])
              (Corpse (Stat name _ _ _ _)) ->
                | target == name == y:ys
                | otherwise = cycleTillMatch (ys ++ [y])
    (Corpse (Stat target _ _ _ _)) -> cycleTillMatch target player:enemies
      where
          cycleTillMatch target (y:ys)
            case y of
              (Player (Stat  name _ _ _)) ->
                | target == name == y:ys
                | otherwise = cycleTillMatch (ys ++ [y])
              (Enemy (Stat name _ _ _ _)) ->
                | target == name == y:ys
                | otherwise = cycleTillMatch (ys ++ [y])
              (Corpse (Stat name _ _ _ _)) ->
                | target == name == y:ys
                | otherwise = cycleTillMatch (ys ++ [y])
```

It can be declared as this:
```haskell
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

buildTurnOrder :: (Combat Stat) -> [Combat Stat]
buildTurnOrder (Combat player enemies)
  = player:enemies

-- Build new turn order
updateTurnOrder :: [Combat Status] -> Combat Status -> [Combat Status]
updateTurnOrder turnOrder currentState
  = cycleTillMatch (getHead turnOrder) (buildTurnOrder currentState)
```

### Functor, Applicative, and Monad
This app uses a custom monad instance for Combat data type that stores the main combat state for this app.
#### Functor
Functor allows the passing of a function to a value inside another data. This value can be another data or a list of data. The function used to apply a function in haskell is fmap which is another example of high order function.
```haskell
class Functor f where
  fmap :: (a -> b) -> f a -> f b
```
An example of the usage of fmap is this:
```haskell
-- To increment the values stored in a list
incList :: [Num] -> Num

-- Normal way
incList [] = []
incList list@(x:xs) = (x+1):(sumList xs)

-- With fmap (which is map for list)
incList list = fmap (+1) list
```
In this app, this is the declaration of functor instance for Combat data type:
```haskell
-- fmap applies function f on the value 
-- stored within Combat Functor
instance Functor Combat where
  fmap f (Player a) = Player (f a)
  fmap f (Enemy a) = Enemy (f a)
  fmap f (Combat player enemies) 
    = Combat (fmap f player) (map (fmap f) enemies)
```
The declaration of functor instance is necessary if you want to create a custom monad or applicative. 
#### Applicative
Applicative allows the passing of a function inside a data to a value inside another data of the same type. In Haskell, applicative is also a functor. This is usually done via (<*>) operator. This is also where pure function which is the default constructor for a given inner value of functor, applicative, and monad is declared.
```haskell
-- (a -> b) is the function stored
-- in applicative f
-- a is the inner value of applicative
class Functor f => Applicative f where
  pure  :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
```
In Haskell, list is also an applicative. For example, an applicative operation between a list of function and a list of integer are like this:
```haskell
[(*0),(+100),(^2)] <*> [1,2,3]
-- [1*0, 2*0, 3*0] ++ 
-- [1+100, 2+100, 3+100] ++ 
-- [1^2,2^2,3^2]
-- [0,0,0,101,102,103,1,4,9]
```
In this app, this is the declaration of applicative instance for Combat data type.
```haskell
instance Applicative Combat where
  pure x = Combat (Player x) []
  (Player f) <*> (Player a) = Player (f a)
  (Enemy f) <*> (Enemy a) = Enemy (f a)
  (Combat p1 fs) <*> (Combat p2 as) = 
    Combat (p1 <*> p2) [x <*> y | x <- fs, y <- as]
```
In this app, applicative's operation is not used but is necessary to be declared for Combat data type to create Combat monad.

#### Monad
Monad allows a data to receive a chain of operation that affects the data. This is the one of the main way that data that store states are operated in haskell. Monad allows this chain of operation via bind (>>=) which takes a function that create the same type of monad from the values of another monad.
```haskell
-- (a -> m b) is a function that create
-- another monad of type m from the values
-- inside m which is a
class Applicative m => Monad m where
  (>>=)  :: m a -> (a -> m b) -> m b
  return :: a -> m a
```
In this app, this is the declaration of monad instance for Combat data type.
```haskell
instance Monad Combat where
  Player a >>= f = f a
  Enemy a >>= f = f a
  Corpse a >>= f = f a
  Combat player enemies >>= f = 
    Combat (player >>= f) (fmap (>>= f) enemies)
```
An example of operation that will change the state of combat is attack which is used to perfrom an attack to a target in Combat state:
```haskell
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
```
When this happens:
``` haskell
nextCombatStatus = 
  currentCombatStatus >>= (attack target selectedMove)
```
What will happen is Combat will map the attack function to the player and all enemies and if the name matches the target name, the player or enemy will reduced. 