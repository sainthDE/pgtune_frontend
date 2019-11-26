module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Form
import Html exposing (Html)
import Url
import Url.Parser as Url


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = \_ -> NoOp
        , onUrlChange = \_ -> NoOp
        }



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
        "pgTune - Tune up your PostgreSQL"
    , body =
        [ Html.map FormMsg (Form.view model.formModel)
        ]
    }
