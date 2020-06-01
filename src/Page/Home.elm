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
        , paddingTop
        , px
        , rem
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
        , p
        , text
        )
import Html.Styled.Attributes exposing (css, href)
import Route
import Session exposing (Session)
import SharedStyles
    exposing
        ( container
        , heroContainer
        , resetStyles
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
        div [ css [ resetStyles, backgroundColor (hex "f9ffeb") ] ]
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
    let
        scenarioStyle =
            [ simpleButton, margin zero, fontSize <| rem 1 ]
    in
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
            , div []
                [ a [ css [ simpleButton ], Route.href (Route.Call Nothing) ]
                    [ text "Join the Call" ]
                ]
            , div [ css [ paddingTop <| px 10 ] ]
                [ a [ css scenarioStyle, Route.href (Route.Call Nothing) ]
                    [ text "Scenario 1" ]
                , a [ css scenarioStyle, Route.href (Route.Call <| Just "guest") ]
                    [ text "Scenario 2" ]
                , a [ css scenarioStyle, Route.href (Route.Call <| Just "webRTC") ]
                    [ text "Scenario 3" ]
                , a [ css scenarioStyle, Route.href (Route.Call <| Just "error") ]
                    [ text "Scenario 4" ]
                ]
            , div [ css [ paddingTop <| px 10 ] ]
                [ p [] [ text "Scenario 1: presenter and VNC, mobile = false" ]
                , p [] [ text "Scenario 2: guest and VNC, mobile = true" ]
                , p [] [ text "Scenario 3: presenter and webRTC, mobile = false " ]
                , p [] [ text "Scenario 4: error with first technology and retry with the second" ]
                ]
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
