module Page.Components.Dialog exposing
    ( bootstrap
    , dialogConfig
    , view
    )

import Css
    exposing
        ( backgroundColor
        , batch
        , fontSize
        , hex
        , marginTop
        , px
        )
import Css.Global as Global
import Dialog
import Html exposing (Html, button, h2, node, text)
import Html.Attributes exposing (class, href, rel)
import Html.Events exposing (onClick)
import Html.Styled as Styled
import Html.Styled.Attributes exposing (css)


bootstrap : Html msg
bootstrap =
    node "link"
        [ href "https://stackpath.bootstrapcdn.com/bootstrap/4.1.1/css/bootstrap.min.css"
        , rel "stylesheet"
        ]
        []


{-| A `Dialog.Config` is just a few piece of optional `Html`, plus "what do we do onClose?"
-}
dialogConfig : { cancelAction : msg, message : Html msg } -> Dialog.Config msg
dialogConfig config =
    let
        body =
            config.message

        header =
            h2 [] [ text "Present:" ]

        footer =
            [ button
                [ class "btn btn-danger"
                , onClick config.cancelAction
                ]
                [ text "Cancel" ]
            ]
    in
    { closeMessage = Just config.cancelAction
    , containerClass = Nothing
    , header = Just header
    , body = Just body
    , footer = footer
    }


view : { visible : Bool, cancelAction : msg, message : Html msg } -> Styled.Html msg
view config =
    Styled.div [ css [ styles ] ]
        [ Styled.fromUnstyled bootstrap
        , Styled.fromUnstyled <|
            Dialog.view
                (if config.visible then
                    Just
                        (dialogConfig
                            { cancelAction = config.cancelAction
                            , message = config.message
                            }
                        )

                 else
                    Nothing
                )
        ]



-- overwrite component bootstrap style


styles : Css.Style
styles =
    batch
        [ Global.descendants
            [ Global.class "modal-dialog"
                [ marginTop (px 200)
                ]
            , Global.class "modal-dialog"
                [ fontSize (px 14)
                ]
            , Global.class "btn-success"
                [ backgroundColor (hex "00E0C5")
                ]
            , Global.class "btn-danger"
                [ backgroundColor (hex "FF3E55")
                ]
            ]
        ]
