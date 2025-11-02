{-# LANGUAGE OverloadedStrings #-}
module Views where

import           Miso
import qualified Miso.Html as H
import qualified Miso.Html.Property as P
import           Miso.Lens
import qualified Miso.CSS as CSS
import Models 
  ( Model ( _event )
  , Event ( Dialog, Choice)
  , Action ( Next, Choose )
  )

-- | Constructs a virtual DOM from a model
viewModel :: Model -> View Model Action
viewModel m =
  H.div_ [P.className "dialog box"]
    (chooseSection m) 
    
chooseSection :: Model -> [View Model Action]
chooseSection m = 
  case _event m of
    Dialog speaker message ->
      [ text $ ms speaker
      , H.br_ []
      , text $ ms message
      , H.br_ []
      , H.button_ [ H.onClick Next ] [ text "Next dialog" ]
      ]

    Choice option1 _ option2 _ ->
      [ text "Choose"
      , H.br_ []
      , H.button_ [ H.onClick (Choose "Left") ] [ text $ ms option1 ]
      , H.br_ []
      , H.button_ [ H.onClick (Choose "Right") ] [ text $ ms option2 ]
      ]

       