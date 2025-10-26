{-# LANGUAGE OverloadedStrings #-}
module Views where

import           Miso
import qualified Miso.Html as H
import qualified Miso.Html.Property as P
import           Miso.Lens
import qualified Miso.CSS as CSS
import Models 
  ( Model
  , dialog
  , Action ( Next )
  )

-- | Constructs a virtual DOM from a model
viewModel :: Model -> View Model Action
viewModel m =
  H.div_ [P.className "dialog box"]
    [ text $ ms (m ^. dialog)
    , H.br_ []
    , H.button_ [ H.onClick Next ] [ text "Next dialog" ]
  ]