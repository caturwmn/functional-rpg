{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Views where
import Text.RawString.QQ
import           Miso
import qualified Miso.Html as H
import qualified Miso.Html.Property as P
import           Miso.Lens
import qualified Miso.CSS as CSS
import qualified Miso.CSS.Color as C
import           Miso.CSS (StyleSheet)
import Models 
  ( Model 
    ( _event
    , _isCombatInitiated
    , _isPlayerTurn
    , _combatInfo 
    , _selectedSkill
    )
  , Event ( Dialog, Choice, CombatEvent)
  , Action 
    ( Next
    , Choose
    , InitiateCombat
    , ChangeInfo
    , ProcessTurn
    , SelectSkill
    , PerformAttack
    )
  , Combat 
    ( Combat
    , Player
    , Enemy
    , Corpse
    )
  , Stat (Stat)
  )

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
    
chooseSection :: Model -> [View Model Action]
chooseSection m = 
  case _event m of
    Dialog speaker message ->
      [ H.div_ [P.class_ "bottom-box"] 
        [ H.div_ [P.class_ "name-box"] 
          [ H.p_ [P.class_ "texts"] [text $ ms speaker]
          ]
        , H.div_ [P.class_ "dialog-box", H.onClick Next]
          [ H.p_ [P.class_ "texts"] [text $ ms message]
          , H.p_ 
              [P.className "texts"
              , CSS.styleInline_
                [r|position:absolute;bottom:0;right:0;
                  padding:10px;text-align:center;
                |]
              ] [text "|Next|<br>\\/"]
          ] 
        ]
      ]

    Choice speaker message option1 _ option2 _ ->
      [ H.div_ [P.className "bottom-box"] 
        [ H.div_ [P.className "name-box"] 
          [ H.p_ [P.className "texts"] [text $ ms speaker]
          ]
        , H.div_ [P.className "dialog-box"]
          [ H.p_ [P.className "texts"] [text $ ms message]
          , H.br_ []
          , H.button_ [ H.onClick (Choose "Left"), P.className "choice-button" ] 
            [ text $ ms option1 ]
          , H.br_ []
          , H.button_ [ H.onClick (Choose "Right"), P.className "choice-button" ] 
            [ text $ ms option2 ]
          , H.p_ 
              [P.className "texts"
              , CSS.styleInline_
                [r|position:absolute;bottom:0;right:0;
                  padding:10px;text-align:center;|]
              ] [text "|Next|<br>\\/"]
          ] 
        ]
      ]

    CombatEvent combat@(Combat (Player (Stat name hp _ moves _)) enemies) _ _ ->
      case (_isCombatInitiated m) of
        False ->
          [ H.div_ [P.className "combat-alert", H.onClick InitiateCombat]
            [ H.div_ [] 
              [ H.p_ [P.className "texts"] 
                [ text [r|////////////////////////////// //////////////
                  //////////////////// ///////////////////|]
                ]
              , H.br_ []
              , H.p_ [P.className "texts"] 
                [ text [r|-----------------------------COMBAT
                  ------------------------------|]]
              , H.br_ []
              , H.p_ [P.className "texts"] 
                [ text [r|////////////// //////////////////
                   ////////////////////// //////////////////////// ///|]]
              ]  
            ]
          ]
        True ->
          [ H.div_ [P.className "enemy-space"] 
            (buildEnemyBox (_isPlayerTurn m) (_selectedSkill m) enemies)
          , H.div_ [P.className "bottom-box", CSS.styleInline_ "display:flex;"] 
            [ H.div_ [P.className "skill-box"] 
              (buildSkills (_isPlayerTurn m) (_selectedSkill m) moves)
            , H.div_ 
              (computeAccess (not isPlayerTurn) 
                [P.className "info-box"
                , H.onClick ProcessTurn
                ]
              )
              [ H.p_ [P.className "texts"] [text $ ms name]
              , H.br_ []
              , H.p_ [P.className "texts"] [text $ ms ("HP: "  ++ (show hp))]
              , H.br_ []
              , H.p_ [P.className "texts"] [text $ ms (_combatInfo m)]
              ]
            ]
          ]

buildEnemyBox :: Bool -> String -> [Combat Stat] -> [View Model Action]
buildEnemyBox isPlayerTurn selectedSkill [] = []
buildEnemyBox isPlayerTurn selectedSkill (x@(Enemy (Stat name hp _ _ source)):enemies)
  = (H.div_ 
      (computeAccess isPlayerTurn
        [P.className "enemy-box"
        , H.onClick (PerformAttack selectedSkill name)
        ]
      )
      [ H.img_ [P.src_ (ms source), P.className "enemy-image", P.alt_ ""] 
      , H.div_ [P.className "enemy-details"] 
        [ H.p_ [P.className "texts"] [text $ ms name]
        , H.br_ []
        , H.p_ [P.className "texts"] [text $ ms ("HP: "  ++ (show hp))]
        ]
      ]
    ):(buildEnemyBox isPlayerTurn selectedSkill enemies)

buildSkills :: Bool -> String -> [String] -> [View Model Action]
buildSkills isPlayerTurn selectedSkill [] =
  [ H.button_ [P.className "skill-item texts"] 
    [text "Item"
    ]
  ]
buildSkills isPlayerTurn selectedSkill (x:xs) =
  (H.button_ 
    (computeAccess isPlayerTurn 
      [(isSkillSelected x selectedSkill)
      , H.onClick (SelectSkill x)
      ]) 
    [text $ ms x
    ]
  ):(buildSkills isPlayerTurn selectedSkill xs)

computeAccess :: Bool -> [Attribute action] -> [Attribute action]
computeAccess isPlayerTurn (x:y)
  | isPlayerTurn == True = x:y
  | otherwise = [x]

isSkillSelected :: String -> String -> Attribute action
isSkillSelected skill selectedSkill
  | skill == selectedSkill 
  = P.className "selected-skill-item texts"
  | otherwise
  = P.className "skill-item texts"

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
    , CSS.selector_ ".bottom-box" 
      [ CSS.margin "5px"
      , CSS.position "absolute"
      , CSS.bottom "0"
      ]
    , CSS.selector_ ".name-box"
      [ CSS.height "30px"
      , CSS.width "max-content"
      , CSS.border "2px solid goldenrod"
      , CSS.marginLeft "5px"
      , CSS.backgroundColor C.white
      ]
    , CSS.selector_ ".texts"
      [ CSS.fontSize "18px"
      , CSS.margin "3px"
      ]
    , CSS.selector_ ".choice-button"
      [ CSS.backgroundColor C.bisque
      , CSS.border "1px solid goldenrod"
      , CSS.borderRadius "6px"
      , CSS.padding "5px"
      , CSS.paddingLeft "10px"
      , CSS.paddingRight "10px"
      , CSS.margin "3px"
      ]
    , CSS.selector_ ".enemy-image"
      [ CSS.display "block"
      , CSS.marginLeft "auto"
      , CSS.marginRight "auto"
      ]
    , CSS.selector_ ".enemy-details"
      [ CSS.alignItems "center"
      , CSS.height "120px"
      , CSS.border "1px solid black"
      ]
    , CSS.selector_ ".enemy-space"
      [ CSS.display "flex"
      , CSS.justifyContent "space-around"
      , CSS.alignItems "center"
      , CSS.height "360px"
      ]
    , CSS.selector_ ".enemy-box"
      [ CSS.alignItems "center"
      , CSS.width "200px"
      , CSS.height "250px"
      , CSS.border "0px dotted black"
      ]
    , CSS.selector_ ".info-box" 
      [ CSS.height "218px"
      , CSS.opacity "0.8"
      , CSS.padding "10px"
      , CSS.position "relative"
      , CSS.width "508px"
      , CSS.border "1px solid goldenrod"
      , CSS.backgroundColor C.white
      ]
    , CSS.selector_ ".skill-box"
      [ CSS.overflow "scroll"
      , CSS.opacity "0.8"
      , CSS.height "238px"
      , CSS.width "258px"
      , CSS.border "1px solid goldenrod"
      , CSS.backgroundColor C.white
      ]
    , CSS.selector_ ".skill-item"
      [ CSS.height "40px"
      , CSS.width "230px"
      , CSS.border "1px solid black"
      , CSS.backgroundColor C.white
      ]
    , CSS.selector_ ".selected-skill-item"
      [ CSS.height "40px"
      , CSS.width "230px"
      , CSS.border "1px solid black"
      , CSS.backgroundColor C.black
      , CSS.color C.white
      ]
    , CSS.selector_ ".combat-alert"
      [ CSS.alignItems "center"
      , CSS.display "flex"
      , CSS.justifyContent "center"
      , CSS.border "4px solid red"
      , CSS.height "99%" 
      , CSS.width "99%"
      ]
    ]
       