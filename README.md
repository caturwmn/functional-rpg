# Functional-RPG
Repository containing a simple rpg made using Haskell Miso. Requires GHCJS, GHCup, and Caball to compile as releasable build. You're welcome to try experimenting with these code to build your own game.

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
which is equivalent to
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
Model module is currently being refactored and is unstable. Will be fixed in future updates

## Others
### cabal.project
### app-name.cabal