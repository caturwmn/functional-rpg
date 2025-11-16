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

-- | `component` takes as arguments the initial model, update function, view function
main :: IO ()
main = do
  gen <- getStdGen
  let initialModel = emptyModel gen
  run (startApp (component initialModel updateModel viewModel))

-- | WASM export, required when compiling w/ the WASM backend.
#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif

