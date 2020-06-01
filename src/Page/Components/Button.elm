module Page.Components.Button exposing
    ( Config
    , Look(..)
    , Size(..)
    , State(..)
    , lookOrdering
    , moduleName
    , sizeOrdering
    , stateOrdering
    , textButtonConfig
    , view
    )

import Color
import Color.Manipulate exposing (weightedMix)
import Css
    exposing
        ( backgroundColor
        , batch
        , border
        , borderRadius
        , boxShadow5
        , color
        , cursor
        , display
        , fontFamilies
        , fontSize
        , fontWeight
        , height
        , hover
        , inlineBlock
        , int
        , lineHeight
        , marginRight
        , notAllowed
        , nthOfType
        , pct
        , pointer
        , px
        , textTransform
        , uppercase
        , width
        , zero
        )
import Css.Transitions as Trans
import Html.Styled exposing (Attribute, Html, button, node, span, text)
import Html.Styled.Attributes as Attrs exposing (css)
import Html.Styled.Events exposing (onClick)



--import Styleguide.Introspection exposing (Example, Introspection, componentDumb)
--import Utils.Color as ColorUtils
--import Utils.Component exposing (ModuleName, createView)
-- MODULE NAME


moduleName : String
moduleName =
    "Components.Button"


type alias CssColor =
    Css.Color


type alias NativeColor =
    Color.Color


type alias FontFamily =
    { name : String
    , variants : List String
    , subsets : List String
    }



-- INTROSPECTION
-- TYPES


type alias Config msg =
    { label : String
    , look : Look
    , state : State
    , size : Size
    , onClick : msg
    , extraCss : List Css.Style
    }


type Look
    = Green
    | Grey
    | Violet
    | Pink
    | Red
    | Transparent
    | White


type State
    = Enabled
    | Disabled
    | Loading


type Size
    = Large
    | Small


lookOrdering : List Look
lookOrdering =
    [ Green, Violet, Pink, Red, Transparent, White ]


stateOrdering : List State
stateOrdering =
    [ Enabled, Loading, Disabled ]


sizeOrdering : List Size
sizeOrdering =
    [ Large, Small ]



-- INITIALIZERS


textButtonConfig : Look -> String -> msg -> Config msg
textButtonConfig look label msg =
    { label = label
    , look = look
    , state = Enabled
    , size = Small
    , onClick = msg
    , extraCss = []
    }



-- VIEW


view : Config msg -> Html msg
view ({ label, look, state, size } as config) =
    let
        stateConfig =
            case state of
                Enabled ->
                    { bgWhite = 0
                    , isPointer = True
                    , isHover = True
                    }

                Disabled ->
                    { bgWhite = 0.6
                    , isPointer = False
                    , isHover = False
                    }

                Loading ->
                    { bgWhite = 0
                    , isPointer = False
                    , isHover = False
                    }

        sizeConfig =
            case size of
                Large ->
                    { width = 256 }

                Small ->
                    { width = 150 }

        lookConfig =
            case look of
                Violet ->
                    { bgColor = colors.myViolet
                    , fontColor = colors.white
                    , bgColorHover = colors.darkerAquaMarine
                    }

                Green ->
                    { bgColor = colors.myGreen
                    , fontColor = colors.white
                    , bgColorHover = colors.darkYellowGreen
                    }

                Pink ->
                    { bgColor = colors.bluePurple
                    , fontColor = colors.white
                    , bgColorHover = colors.darkBluePurple
                    }

                Red ->
                    { bgColor = colors.myRed
                    , fontColor = colors.white
                    , bgColorHover = colors.darkWatermelon
                    }

                Transparent ->
                    { bgColor = colors.transparent
                    , fontColor = colors.bluePurple
                    , bgColorHover = colors.transparent
                    }

                White ->
                    { bgColor = colors.white
                    , fontColor = colors.bluePurple
                    , bgColorHover = colors.white
                    }

                Grey ->
                    { bgColor = colors.myGrey
                    , fontColor = colors.white
                    , bgColorHover = colors.black
                    }

        bgColor =
            weightedMix
                (cssColorToNative colors.white)
                (cssColorToNative lookConfig.bgColor)
                stateConfig.bgWhite
                |> nativeColorToCss

        shadowColor =
            setAlpha 0.15 colors.black

        styleButton =
            Css.batch
                [ backgroundColor bgColor
                , color lookConfig.fontColor
                , width (px sizeConfig.width)
                , borderRadius (px 25)
                , height (px 48)
                , border zero
                , textTransform uppercase
                , fontSize (px 14)
                , lineHeight (px 16)
                , fontWeight (int 600)
                , fontFamilies [ fontFamily |> .text |> .name ]
                , Trans.transition
                    [ Trans.background 500
                    ]
                , batchIfElse stateConfig.isPointer
                    [ cursor pointer ]
                    [ cursor notAllowed ]
                , batchIf stateConfig.isHover
                    [ hover [ backgroundColor lookConfig.bgColorHover ] ]
                , boxShadow5 zero (px 2) (px 42) zero shadowColor
                ]

        styleDot =
            Css.batch
                [ display inlineBlock
                , marginRight (px 10)
                , backgroundColor lookConfig.fontColor
                , width (px 10)
                , height (px 10)
                , borderRadius (pct 100)
                , Css.property "animation" "loading 1.4s infinite"
                , nthOfType "1"
                    [ Css.property "animation-delay" "0.4s" ]
                , nthOfType "2"
                    [ Css.property "animation-delay" "0.2s" ]
                ]

        viewDots =
            List.repeat 3 (span [ css [ styleDot ] ] [])

        viewStyle =
            node "style"
                []
                [ text <|
                    "@keyframes loading {"
                        ++ " 0%, 80%, 100% { transform: scale(0); }"
                        ++ "40% { transform: scale(1.0); }"
                        ++ "}"
                ]

        buttonAttrs =
            [ css (styleButton :: config.extraCss)
            ]
                |> addAttributesIf (state == Enabled) [ onClick config.onClick ]
    in
    button (Attrs.attribute "data-module" moduleName :: buttonAttrs)
        (List.concat
            [ [ viewStyle ]
            , case state of
                Loading ->
                    viewDots

                _ ->
                    [ text label ]
            ]
        )


addAttributesIf : Bool -> List (Attribute msg) -> List (Attribute msg) -> List (Attribute msg)
addAttributesIf condition newAttrs attributeList =
    if condition then
        attributeList ++ newAttrs

    else
        attributeList


batchIfElse : Bool -> List Css.Style -> List Css.Style -> Css.Style
batchIfElse condition stylesTrue stylesFalse =
    if condition then
        batch stylesTrue

    else
        batch stylesFalse


batchIf : Bool -> List Css.Style -> Css.Style
batchIf condition styles =
    if condition then
        batch styles

    else
        batch []


colors :
    { aquaMarine : Css.Color
    , aquaMarineDark : Css.Color
    , darkerAquaMarine : Css.Color
    , boxShadow : Css.Color
    , black : Css.Color
    , bluePurple : Css.Color
    , darkBluePurple : Css.Color
    , blueViolet : Css.Color
    , cherryPie : Css.Color
    , comet : Css.Color
    , darkGrey : Css.Color
    , darkIndigo : Css.Color
    , darkIndigoLight : Css.Color
    , darkTurquoise : Css.Color
    , darkTurquoiseLight : Css.Color
    , darkWatermelon : Css.Color
    , deepIndigo : Css.Color
    , dimGrey : Css.Color
    , greenBlue : Css.Color
    , lightBlueGrey : Css.Color
    , lightGrey : Css.Color
    , mediumOrchid : Css.Color
    , orange : Css.Color
    , paleLilac : Css.Color
    , pattensBlue : Css.Color
    , portage : Css.Color
    , purble : Css.Color
    , shadow : Css.Color
    , shadowDark : Css.Color
    , sapphire : Css.Color
    , steel : Css.Color
    , steelLight : Css.Color
    , steelTransparent : Css.Color
    , tealish : Css.Color
    , tealishLight : Css.Color
    , transparent : Css.Color
    , transparentWhite : Css.Color
    , clearWhite : Css.Color
    , turquoise : Css.Color
    , watermelon : Css.Color
    , white : Css.Color
    , whiteSmoke : Css.Color
    , yellowGreen : Css.Color
    , darkYellowGreen : Css.Color
    , yellowGreenTranslucid : Css.Color
    , myGreen : Css.Color
    , myRed : Css.Color
    , myGrey : Css.Color
    , myViolet : Css.Color
    }
colors =
    { aquaMarine = Css.rgb 0 224 197
    , aquaMarineDark = Css.rgb 0 169 172
    , darkerAquaMarine = Css.rgba 0 199 175 1
    , boxShadow = Css.rgba 51 51 51 0.15
    , black = Css.rgb 51 51 51
    , bluePurple = Css.rgb 123 49 231
    , darkBluePurple = Css.rgba 87 21 182 1
    , blueViolet = Css.rgb 90 42 180
    , cherryPie = Css.rgb 51 49 88
    , comet = Css.rgb 85 85 115
    , darkGrey = Css.rgb 48 49 51
    , darkIndigo = Css.rgb 16 13 60
    , darkIndigoLight = Css.rgba 16 13 60 0.9
    , darkTurquoise = Css.rgb 0 224 197
    , darkTurquoiseLight = Css.rgba 0 224 197 0.8
    , darkWatermelon = Css.rgb 229 55 76
    , deepIndigo = Css.rgb 28 23 69
    , dimGrey = Css.rgb 102 102 102
    , greenBlue = Css.rgb 0 175 159
    , lightBlueGrey = Css.rgb 216 231 246
    , lightGrey = Css.rgba 206 211 216 1
    , mediumOrchid = Css.rgb 200 109 215
    , orange = Css.rgb 252 156 0
    , paleLilac = Css.rgb 250 250 255
    , pattensBlue = Css.rgb 211 223 239
    , portage = Css.rgb 127 150 209
    , purble = Css.rgba 176 132 241 1
    , sapphire = Css.rgb 31 30 144
    , shadow = Css.rgba 0 0 0 0.1
    , shadowDark = Css.rgba 0 0 0 0.6
    , steel = Css.rgb 119 133 147
    , steelLight = Css.rgba 119 133 147 0.8
    , steelTransparent = Css.rgba 119 133 147 0.2
    , tealish = Css.rgb 43 189 216
    , tealishLight = Css.rgba 43 189 216 0.1
    , transparent = Css.rgba 0 0 0 0
    , transparentWhite = Css.rgba 255 255 255 0.6
    , clearWhite = Css.rgba 255 255 255 0.2
    , turquoise = Css.rgba 62 188 204 1
    , watermelon = Css.rgb 255 62 85
    , whiteSmoke = Css.rgb 233 233 233
    , yellowGreen = Css.rgb 200 255 71
    , darkYellowGreen = Css.rgba 175 249 0 1
    , yellowGreenTranslucid = Css.rgba 200 255 71 0.2

    -- my precious
    , white = Css.rgb 255 255 255
    , myGreen = Css.rgba 0 121 107 1
    , myRed = Css.rgba 197 34 31 1
    , myGrey = Css.rgba 148 148 150 1
    , myViolet = Css.rgba 132 131 218 1
    }


{-| Converts native Elm colors to elm-css colors
-}
nativeColorToCss : NativeColor -> CssColor
nativeColorToCss color =
    let
        normalize color_ =
            (color_ * 255.0)
                |> round
                |> clamp 0 255
    in
    color
        |> Color.toRgba
        |> (\{ red, green, blue, alpha } ->
                Css.rgba
                    (normalize red)
                    (normalize green)
                    (normalize blue)
                    alpha
           )


{-| Converts elm-css colors to native Elm colors
-}
cssColorToNative : CssColor -> NativeColor
cssColorToNative { red, green, blue, alpha } =
    let
        normalize color =
            toFloat color / 255.0
    in
    Color.rgba (normalize red) (normalize green) (normalize blue) alpha



-- ELM-CSS


setAlpha : Float -> CssColor -> CssColor
setAlpha pct { red, green, blue } =
    Css.rgba red green blue pct


fontFamily : { text : FontFamily, numbers : FontFamily }
fontFamily =
    { text =
        { name = "Montserrat"
        , variants = [ "Light", "Regular", "Medium", "Bold" ]
        , subsets = []
        }
    , numbers =
        { name = "Roboto Mono"
        , variants = [ "Light", "Regular" ]
        , subsets = []
        }
    }
