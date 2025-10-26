{-# LANGUAGE CPP               #-}
module Main where

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

main :: IO ()
main = run (startApp app)

-- | `component` takes as arguments the initial model, update function, view function
app :: App Model Action
app = component emptyModel updateModel viewModel

-- | WASM export, required when compiling w/ the WASM backend.
#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif

