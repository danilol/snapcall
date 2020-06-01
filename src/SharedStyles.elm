module SharedStyles exposing
    ( container
    , dataHeader
    , header
    , heroContainer
    , logo
    , resetStyles
    , shadow
    , simpleButton
    , textCenter
    )

import Css
    exposing
        ( Style
        , backgroundColor
        , batch
        , block
        , border3
        , borderBottom3
        , borderRadius
        , center
        , color
        , cursor
        , display
        , fontFamilies
        , fontSize
        , fontWeight
        , hex
        , hover
        , inlineBlock
        , int
        , lineHeight
        , margin
        , marginBottom
        , marginRight
        , maxWidth
        , middle
        , none
        , num
        , padding
        , padding2
        , padding4
        , paddingLeft
        , paddingRight
        , pct
        , pointer
        , px
        , rem
        , solid
        , textAlign
        , textDecoration
        , transparent
        , verticalAlign
        , width
        , zero
        )



-- STYLES


dataHeader : Style
dataHeader =
    batch
        [ backgroundColor (hex "3398cc")
        , padding (px 20)
        , color (hex "fff")
        ]


shadow : Style
shadow =
    batch
        [ Css.property "box-shadow" <| "0px 0px 25px -1px rgba(0,0,0,0.36)"
        , marginBottom (rem 1)
        ]


container : Style
container =
    batch
        [ width (pct 100)
        , maxWidth (px 1140)
        , paddingLeft (px 15)
        , paddingRight (px 15)
        ]


heroContainer : Style
heroContainer =
    batch
        [ Css.property "display" <| "grid"
        , Css.property "grid-template-columns" <| " 1fr 1fr"
        , Css.property "align-items" <| "center"
        , color (hex "4fa756")
        ]


header : Style
header =
    batch
        [ backgroundColor (hex "3398cc")
        , borderBottom3 (px 1) solid (hex "95ffc5")
        , padding4 (px 30) (px 0) (px 30) (px 0)
        ]


simpleButton : Style
simpleButton =
    batch
        [ display inlineBlock
        , fontWeight (int 400)
        , color (hex "fff")
        , textAlign center
        , Css.property "user-select" <| "none"
        , verticalAlign middle
        , border3 (px 1) solid transparent
        , padding2 (rem 0.5) (rem 1)
        , fontSize (rem 1.25)
        , lineHeight (num 1.5)
        , borderRadius (px 25)
        , lineHeight (num 1.5)
        , cursor pointer
        , textDecoration none
        , Css.property "transition" <| "all ease .3s"
        , backgroundColor (hex "00796b")
        , marginRight (px 15)
        , hover
            [ color (hex "000")
            , backgroundColor transparent
            , border3 (px 1) solid (hex "00796b")
            ]
        ]


textCenter : Style
textCenter =
    batch
        [ textAlign center
        ]


logo : Style
logo =
    batch
        [ display block
        , width (px 110)
        ]


resetStyles : Style
resetStyles =
    batch
        [ padding zero
        , margin zero
        , fontFamilies [ "BlinkMacSystemFont", "Segoe UI", "Roboto", "Helvetica Neue", "Arial", "Noto Sans", "sans-serif" ]
        ]
