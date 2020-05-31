module Page.Home exposing
    ( Model
    , Msg
    , init
    , toSession
    , update
    , view
    )

{-| The homepage. You can get here via either the / or /#/ routes.
-}

import Asset
import Css
    exposing
        ( alignSelf
        , backgroundColor
        , center
        , fontSize
        , height
        , hex
        , margin
        , marginBottom
        , px
        , vh
        , width
        , zero
        )
import Html.Styled
    exposing
        ( Html
        , a
        , div
        , h1
        , h2
        , img
        , text
        )
import Html.Styled.Attributes exposing (css, href)
import Route
import Session exposing (Session)
import SharedStyles
    exposing
        ( container
        , heroContainer
        , simpleButton
        )



-- MODEL


type alias Model =
    { title : String
    , session : Session
    }


init : Session -> ( Model, Cmd Msg )
init session =
    ( { title = "Elm Example App", session = session }
    , Cmd.none
    )



-- VIEW


view : { model | title : String } -> { title : String, content : Html Msg }
view model =
    { title = model.title
    , content =
        div [ css [ backgroundColor (hex "f9ffeb") ] ]
            [ viewHero
            ]
    }


viewLogo : Html msg
viewLogo =
    div [ css [ alignSelf center, marginBottom (px 75) ] ]
        [ img [ css [ width (px 250) ], Asset.src Asset.logo ] []
        ]


viewHero : Html msg
viewHero =
    div
        [ css
            [ container
            , heroContainer
            ]
        ]
        [ div []
            [ viewLogo
            , h1 [ css [ fontSize (px 48), margin zero ] ] [ text "SnapCall" ]
            , h2 [] [ text "Snapview code challenge" ]
            , a [ css [ simpleButton ], Route.href (Route.Call Nothing) ]
                [ text "Join the Call" ]
            ]
        , div []
            [ img [ css [ height (vh 100) ], Asset.src Asset.elmIcon ] []
            ]
        ]



-- UPDATE


type Msg
    = GotSession Session


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotSession newSession ->
            ( { model | session = newSession }, Cmd.none )



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session
