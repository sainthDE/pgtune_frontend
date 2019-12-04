module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Form
import Html exposing (Html, a, button, div, img, nav, p, span, text)
import Html.Attributes exposing (alt, class, height, href, src, width)
import Url
import Url.Parser as Url


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = handleUrlRequest
        , onUrlChange = \_ -> NoOp
        }


handleUrlRequest : Browser.UrlRequest -> Msg
handleUrlRequest request =
    case request of
        Browser.Internal _ ->
            NoOp

        Browser.External href ->
            ChangeWebsite href



-- MODEL


type alias Model =
    { formModel : Form.Model
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url _ =
    ( Model (Form.init url), Cmd.none )



-- UPDATE


type Msg
    = FormMsg Form.Msg
    | ChangeWebsite String
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FormMsg m ->
            let
                ( mod, formMsg ) =
                    Form.update m model.formModel
            in
            ( { model | formModel = mod }, Cmd.map FormMsg formMsg )

        ChangeWebsite href ->
            ( model, Nav.load href )

        NoOp ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title =
        "pgTune - Tune your PostgreSQL up to eleven"
    , body =
        [ viewHeader
        , Html.map FormMsg (Form.view model.formModel)
        ]
    }


viewHeader : Html msg
viewHeader =
    Html.nav [ class "navbar" ]
        [ div [ class "container" ]
            [ div [ class "navbar-brand" ]
                [ a [ class "navbar-item", href "/" ]
                    [ img [ src "images/brand.png", alt "pgTune - Tune your PostgreSQL up to eleven" ] [] ]
                ]
            , div [ class "navbar-menu" ]
                [ div [ class "navbar-end" ]
                    [ p [ class "control" ]
                        [ a [ class "button", href "https://github.com/sainth-/pgtune_frontend" ]
                            [ span [ class "icon" ] [ img [ src "images/github.png", alt "github" ] [] ]
                            , span [] [ text "Frontend" ]
                            ]
                        ]
                    , p [ class "control" ]
                        [ a [ class "button", href "https://github.com/sainth-/pgtune" ]
                            [ span [ class "icon" ] [ img [ src "images/github.png", alt "github" ] [] ]
                            , span [] [ text "Backend" ]
                            ]
                        ]
                    ]
                ]
            ]
        ]
