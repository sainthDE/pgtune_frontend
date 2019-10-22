module Main exposing (main)

import Browser
import Html exposing (Html)
import Form


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { formModel : Form.Model
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model Form.init, Cmd.none )


-- UPDATE


type Msg
    = FormMsg Form.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FormMsg m ->
            let (mod, formMsg) = Form.update m model.formModel
            in ({ model | formModel = mod }, Cmd.map FormMsg formMsg)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    Html.map FormMsg (Form.view model.formModel)
