module Profligate.Spinner where

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

spinner :: forall a b. H.HTML a b
spinner =
    HH.div
        [ HP.attr (H.AttrName "class") "spinner" ]
        [ HH.div
            [ HP.attr (H.AttrName "class") "sk-fading-circle" ]
            [ HH.div
                [ HP.attr (H.AttrName "class") "sk-circle1 sk-circle" ]
                []
            , HH.div
                [ HP.attr (H.AttrName "class") "sk-circle2 sk-circle" ]
                []
            , HH.div
                [ HP.attr (H.AttrName "class") "sk-circle3 sk-circle" ]
                []
            , HH.div
                [ HP.attr (H.AttrName "class") "sk-circle4 sk-circle" ]
                []
            , HH.div
                [ HP.attr (H.AttrName "class") "sk-circle5 sk-circle" ]
                []
            , HH.div
                [ HP.attr (H.AttrName "class") "sk-circle6 sk-circle" ]
                []
            , HH.div
                [ HP.attr (H.AttrName "class") "sk-circle7 sk-circle" ]
                []
            , HH.div
                [ HP.attr (H.AttrName "class") "sk-circle8 sk-circle" ]
                []
            , HH.div
                [ HP.attr (H.AttrName "class") "sk-circle9 sk-circle" ]
                []
            , HH.div
                [ HP.attr (H.AttrName "class") "sk-circle10 sk-circle" ]
                []
            , HH.div
                [ HP.attr (H.AttrName "class") "sk-circle11 sk-circle" ]
                []
            , HH.div
                [ HP.attr (H.AttrName "class") "sk-circle12 sk-circle" ]
                []
            ]
        ]