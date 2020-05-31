module Page.Call.Styles exposing
    ( callContainer
    , camera
    , cameraContainter
    , sharingOptionContainer
    , sharingOptionContent
    )

{-| The styles for the Call page.
-}

import Css
    exposing
        ( Style
        , alignItems
        , auto
        , backgroundColor
        , batch
        , borderRadius
        , center
        , color
        , column
        , cursor
        , displayFlex
        , flexDirection
        , fontSize
        , height
        , hex
        , justifyContent
        , lineHeight
        , margin
        , marginTop
        , padding
        , pct
        , pointer
        , px
        , textAlign
        , width
        )
import SharedStyles exposing (container)


callContainer : Style
callContainer =
    batch
        [ container
        , marginTop (px 50)
        ]


cameraContainter : Style
cameraContainter =
    batch
        [ displayFlex
        , alignItems center
        , width <| pct 100
        ]


camera : Style
camera =
    batch
        [ backgroundColor <| hex "3c4043"
        , color <| hex "fff"
        , fontSize <| px 24
        , lineHeight <| px 32
        , borderRadius <| px 8
        , width <| px 740
        , height <| px 416
        , alignItems center
        , Css.property "flex" <| "0 1 740px"
        , flexDirection column
        , justifyContent center
        , padding <| px 5
        , textAlign center
        , displayFlex
        ]


sharingOptionContainer : Style
sharingOptionContainer =
    batch
        [ displayFlex
        , width <| pct 100
        , height <| pct 100
        ]


sharingOptionContent : Style
sharingOptionContent =
    batch
        [ displayFlex
        , alignItems center
        , justifyContent center
        , width <| px 200
        , height <| px 200
        , color <| hex "fff"
        , backgroundColor <| hex "000"
        , margin auto
        , borderRadius <| px 8
        , cursor pointer
        ]
