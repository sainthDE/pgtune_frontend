module Form exposing (Model, Msg, init, update, view)

import Data.DataStorage as DS
import Data.DbApplication as DbApp
import Data.Memory as Mem
import Data.OperatingSystem as Os
import Data.PostgresVersion as Postgres
import Data.SizeUnit as Unit
import Data.SystemConfiguration as Sysconf
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Html.SelectPrism exposing (selectp)
import Http
import Json.Encode as Encode
import List exposing ((::))
import Maybe
import Monocle.Prism exposing (Prism)
import String exposing (isEmpty)
import Tuple exposing (first, second)
import Url


type alias Model =
    { parameters : Sysconf.SystemConfiguration
    , configuration : String
    , url : Url.Url
    }


init : Url.Url -> Model
init url =
    Model (Sysconf.SystemConfiguration Postgres.V11 Os.Linux DbApp.WEB (Mem.Memory 4 Unit.GB) Nothing Nothing DS.HDD) "" url


type Msg
    = ChangeDbVersion (Result String Postgres.PostgresVersion)
    | ChangeOsType (Result String Os.OperatingSystem)
    | ChangeDbApplication (Result String DbApp.DbApplication)
    | ChangeRam Mem.Memory
    | ChangeRamUnit (Result String Unit.SizeUnit)
    | ChangeCores (Maybe Int)
    | ChangeConnections (Maybe Int)
    | ChangeDataStorage (Result String DS.DataStorage)
    | SubmitForm
    | GotConfig (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeDbVersion dbVersion ->
            let
                oldValue =
                    model.parameters

                newValue =
                    { oldValue | dbVersion = Result.withDefault Postgres.V11 dbVersion }
            in
            ( { model | parameters = newValue }, Cmd.none )

        ChangeOsType osType ->
            let
                oldValue =
                    model.parameters

                newValue =
                    { oldValue | osType = Result.withDefault Os.Linux osType }
            in
            ( { model | parameters = newValue }, Cmd.none )

        ChangeDbApplication dbApplication ->
            let
                oldValue =
                    model.parameters

                newValue =
                    { oldValue | dbApplication = Result.withDefault DbApp.WEB dbApplication }
            in
            ( { model | parameters = newValue }, Cmd.none )

        ChangeRam ram ->
            let
                oldValue =
                    model.parameters

                newValue =
                    { oldValue | ram = ram }
            in
            ( { model | parameters = newValue }, Cmd.none )

        ChangeRamUnit unit ->
            let
                oldValue =
                    model.parameters

                oldValue2 =
                    oldValue.ram

                newValue2 =
                    { oldValue2 | unit = Result.withDefault Unit.GB unit }

                newValue =
                    { oldValue | ram = newValue2 }
            in
            ( { model | parameters = newValue }, Cmd.none )

        ChangeCores cores ->
            let
                oldValue =
                    model.parameters

                newValue =
                    { oldValue | cores = cores }
            in
            ( { model | parameters = newValue }, Cmd.none )

        ChangeConnections connections ->
            let
                oldValue =
                    model.parameters

                newValue =
                    { oldValue | connections = connections }
            in
            ( { model | parameters = newValue }, Cmd.none )

        ChangeDataStorage dataStorage ->
            let
                oldValue =
                    model.parameters

                newValue =
                    { oldValue | dataStorage = Result.withDefault DS.HDD dataStorage }
            in
            ( { model | parameters = newValue }, Cmd.none )

        SubmitForm ->
            ( model
            , Http.post
                { url = "/api/configuration"
                , body = Http.jsonBody (Sysconf.encode model.parameters)
                , expect = Http.expectString GotConfig
                }
            )

        GotConfig res ->
            ( { model | configuration = Result.withDefault "err" res }, Cmd.none )


view : Model -> Html Msg
view model =
    let
        params =
            model.parameters
    in
    div [ class "container" ]
        [ div [ class "columns" ]
            [ div [ class "column", class "is-4", class "is-offset-2" ]
                [ div [ class "box" ]
                    [ toFormElement "PostgreSQL Version" Nothing (toSelectBox postgresVersionP ChangeDbVersion params.dbVersion postgresVersions)
                    , toFormElement "Operating System" (Just "The operating system of the PostgreSQL host") (toSelectBox osTypeP ChangeOsType params.osType osTypes)
                    , toFormElement "Application Area" (Just "For what type of application should be tuned") (toSelectBox dbApplicationP ChangeDbApplication params.dbApplication dbApplications)
                    , toFormElement "RAM" (Just "How much memory can PostgreSQL use") (combineToField ( input [ type_ "text", placeholder "RAM", value (String.fromInt params.ram.memory), onInput (\mem -> ChangeRam (Mem.Memory (Maybe.withDefault params.ram.memory (String.toInt mem)) params.ram.unit)), class "input" ] [], True ) ( toSelectBox sizeUnitP ChangeRamUnit params.ram.unit sizeUnits, False ))
                    , toFormElement "Number of CPU cores" (Just "How many CPU cores can PostgreSQL use\nThis refers to the number of CPUs that are recognized by the operating system.") (input [ type_ "text", placeholder "Cores", value (Maybe.withDefault "" (Maybe.map String.fromInt params.cores)), onInput (ChangeCores << String.toInt), class "input" ] [])
                    , toFormElement "Number of connections" (Just "Maximum number of PostgreSQL client connections") (input [ type_ "text", placeholder "Connections", value (Maybe.withDefault "" (Maybe.map String.fromInt params.connections)), onInput (ChangeConnections << String.toInt), class "input" ] [])
                    , toFormElement "Storage technology" Nothing (toSelectBox dataStorageP ChangeDataStorage params.dataStorage dataStorages)
                    , button [ onClick SubmitForm, class "button", class "is-success", class "is-fullwidth" ] [ text "Generate" ]
                    ]
                ]
            , div [ class "column", class "is-4", class "is-flex" ]
                [ div [ class "box", class "is-fullwidth" ]
                    [ p [ class "field" ] [ text "Adjust the values and hit Generate to get your tuned configuration. ", a [ class "tag", class "is-info", class "is-light", class "is-normal", href "https://github.com/sainth-/pgtune/wiki/Formulas" ] [ text "How does it work?" ] ]
                    , configurationToHtml model.configuration
                    ]
                ]
            ]
        , div [ class "columns" ]
            [ div [ class "column", class "is-8", class "is-offset-2" ]
                [ div [ class "box" ]
                    [ pre [ id "curl-command", class "field" ] [ text (toCurlCommand params model.url) ]
                    , div [] [ button [ class "copy-btn", class "button", class "is-info", class "", class "is-fullwidth", attribute "data-clipboard-target" "#curl-command" ] [ text "Copy to Clipboard" ] ]
                    ]
                ]
            ]
        ]


toCurlCommand : Sysconf.SystemConfiguration -> Url.Url -> String
toCurlCommand params url =
    let
        json =
            Encode.encode 0 (Sysconf.encode params)

        addPort maybePort starter =
            case maybePort of
                Nothing ->
                    starter

                Just port_ ->
                    starter ++ ":" ++ String.fromInt port_

        url2 =
            let
                http =
                    case url.protocol of
                        Url.Http ->
                            "http://"

                        Url.Https ->
                            "https://"
            in
            addPort url.port_ (http ++ url.host)
    in
    "curl -H \"Content-Type: application/json\" \\\n"
        ++ "     -H \"Accept: text/plain\" \\\n"
        ++ "     -d '"
        ++ json
        ++ "' \\\n"
        ++ "     -X POST "
        ++ url2
        ++ "/api/configuration"


configurationToHtml : String -> Html Msg
configurationToHtml configuration =
    if isEmpty configuration then
        p [] []

    else
        div []
            [ pre [ id "generated-config", class "field" ] [ text configuration ]
            , div [] [ button [ class "copy-btn", class "button", class "is-info", class "is-fullwidth", attribute "data-clipboard-target" "#generated-config" ] [ text "Copy to Clipboard" ] ]
            ]


toFormElement : String -> Maybe String -> Html Msg -> Html Msg
toFormElement titletext helptext elem =
    div [ class "field" ]
        [ label [ class "label" ] [ text titletext, text " ", Maybe.withDefault (text "") (Maybe.map (\h -> span [ class "tag", class "is-info", class "is-light", class "is-normal", title h ] [ text "Hint" ]) helptext) ]
        , div [ class "control is-expanded" ] [ elem ]
        ]


combineToField : ( Html Msg, Bool ) -> ( Html Msg, Bool ) -> Html Msg
combineToField form1 form2 =
    let
        expand1 =
            if second form1 then
                [ class "is-expanded" ]

            else
                []

        expand2 =
            if second form2 then
                [ class "is-expanded" ]

            else
                []
    in
    div [ class "field", class "has-addons" ]
        [ p (class "control" :: expand1) [ first form1 ]
        , p (class "control" :: expand2) [ first form2 ]
        ]


toSelectBox : Prism String a -> (Result String a -> msg) -> a -> List ( String, a ) -> Html msg
toSelectBox prism msger selected_ labelValues =
    span [ class "select", class "is-fullwidth" ] [ selectp prism msger selected_ [] labelValues ]


postgresVersions =
    [ ( "9.4", Postgres.V94 )
    , ( "9.5", Postgres.V95 )
    , ( "9.6", Postgres.V96 )
    , ( "10", Postgres.V10 )
    , ( "11", Postgres.V11 )
    , ( "12", Postgres.V12 )
    ]


osTypes =
    [ ( "Linux", Os.Linux )
    , ( "Windows", Os.Windows )
    , ( "MacOS", Os.MacOsX )
    ]


dbApplications =
    [ ( "Web", DbApp.WEB )
    , ( "OLTP", DbApp.OLTP )
    , ( "Data Warehouse", DbApp.DATAWAREHOUSE )
    , ( "Desktop", DbApp.DESKTOP )
    , ( "Mixed", DbApp.MIXED )
    ]


dataStorages =
    [ ( "HDD", DS.HDD )
    , ( "SAN", DS.SAN )
    , ( "SSD", DS.SSD )
    ]


sizeUnits =
    [ ( "MB", Unit.MB )
    , ( "GB", Unit.GB )
    , ( "TB", Unit.TB )
    ]


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
    input [ type_ t, placeholder p, value v, onInput toMsg ] []


postgresVersionP : Prism String Postgres.PostgresVersion
postgresVersionP =
    let
        fromString : String -> Maybe Postgres.PostgresVersion
        fromString s =
            case s of
                "V9_4" ->
                    Just Postgres.V94

                "V9_5" ->
                    Just Postgres.V95

                "V9_6" ->
                    Just Postgres.V96

                "V10" ->
                    Just Postgres.V10

                "V11" ->
                    Just Postgres.V11

                "V12" ->
                    Just Postgres.V12

                _ ->
                    Nothing

        toString : Postgres.PostgresVersion -> String
        toString v =
            case v of
                Postgres.V94 ->
                    "V9_4"

                Postgres.V95 ->
                    "V9_5"

                Postgres.V96 ->
                    "V9_6"

                Postgres.V10 ->
                    "V10"

                Postgres.V11 ->
                    "V11"

                Postgres.V12 ->
                    "V12"
    in
    Prism fromString toString


osTypeP : Prism String Os.OperatingSystem
osTypeP =
    let
        fromString : String -> Maybe Os.OperatingSystem
        fromString s =
            case s of
                "Linux" ->
                    Just Os.Linux

                "Windows" ->
                    Just Os.Windows

                "MacOS" ->
                    Just Os.MacOsX

                _ ->
                    Nothing

        toString : Os.OperatingSystem -> String
        toString v =
            case v of
                Os.Linux ->
                    "Linux"

                Os.Windows ->
                    "Windows"

                Os.MacOsX ->
                    "MacOS"
    in
    Prism fromString toString


dbApplicationP : Prism String DbApp.DbApplication
dbApplicationP =
    let
        fromString : String -> Maybe DbApp.DbApplication
        fromString s =
            case s of
                "Web" ->
                    Just DbApp.WEB

                "OLTP" ->
                    Just DbApp.OLTP

                "Data Warehouse" ->
                    Just DbApp.DATAWAREHOUSE

                "Desktop" ->
                    Just DbApp.DESKTOP

                "Mixed" ->
                    Just DbApp.MIXED

                _ ->
                    Nothing

        toString : DbApp.DbApplication -> String
        toString app =
            case app of
                DbApp.WEB ->
                    "Web"

                DbApp.OLTP ->
                    "OLTP"

                DbApp.DATAWAREHOUSE ->
                    "Data Warehouse"

                DbApp.DESKTOP ->
                    "Desktop"

                DbApp.MIXED ->
                    "Mixed"
    in
    Prism fromString toString


dataStorageP : Prism String DS.DataStorage
dataStorageP =
    let
        fromString : String -> Maybe DS.DataStorage
        fromString s =
            case s of
                "HDD" ->
                    Just DS.HDD

                "SAN" ->
                    Just DS.SAN

                "SSD" ->
                    Just DS.SSD

                _ ->
                    Nothing

        toString : DS.DataStorage -> String
        toString ds =
            case ds of
                DS.HDD ->
                    "HDD"

                DS.SAN ->
                    "SAN"

                DS.SSD ->
                    "SSD"
    in
    Prism fromString toString


sizeUnitP =
    let
        fromString : String -> Maybe Unit.SizeUnit
        fromString s =
            case s of
                "B" ->
                    Just Unit.B

                "KB" ->
                    Just Unit.KB

                "MB" ->
                    Just Unit.MB

                "GB" ->
                    Just Unit.GB

                "TB" ->
                    Just Unit.TB

                _ ->
                    Nothing

        toString : Unit.SizeUnit -> String
        toString u =
            case u of
                Unit.B ->
                    "B"

                Unit.KB ->
                    "KB"

                Unit.MB ->
                    "MB"

                Unit.GB ->
                    "GB"

                Unit.TB ->
                    "TB"
    in
    Prism fromString toString
