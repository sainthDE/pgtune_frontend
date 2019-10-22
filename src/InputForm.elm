module InputForm exposing (Model, init, Msg, update, view)

import Data.SystemConfiguration as Sysconf
import Data.PostgresVersion as Postgres
import Data.OperatingSystem as Os
import Data.DbApplication as DbApp
import Data.Memory as Mem
import Data.SizeUnit as Unit
import Data.DataStorage as DS

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Http
import Monocle.Prism exposing (Prism)
import Html.SelectPrism exposing (selectp)

type alias Model = { parameters : Sysconf.SystemConfiguration
                   , configuration : String
                   }

init : Model
init = Model (Sysconf.SystemConfiguration Postgres.V11 Os.Linux DbApp.WEB (Mem.Memory 4 Unit.GB) Nothing Nothing DS.HDD) ""

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

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
    case msg of
        ChangeDbVersion dbVersion ->
            let oldValue = model.parameters
                newValue = { oldValue | dbVersion = Result.withDefault Postgres.V11 dbVersion }
            in ({ model | parameters = newValue }, Cmd.none)
        ChangeOsType osType ->
            let oldValue = model.parameters
                newValue = { oldValue | osType = Result.withDefault Os.Linux osType }
            in ({ model | parameters = newValue }, Cmd.none)
        ChangeDbApplication dbApplication ->
            let oldValue = model.parameters
                newValue = { oldValue | dbApplication = Result.withDefault DbApp.WEB dbApplication }
            in ({ model | parameters = newValue }, Cmd.none)
        ChangeRam ram ->
            let oldValue = model.parameters
                newValue = { oldValue | ram = ram }
            in ({ model | parameters = newValue }, Cmd.none)
        ChangeRamUnit unit ->
            let oldValue = model.parameters
                oldValue2 = oldValue.ram
                newValue2 = { oldValue2 | unit = Result.withDefault Unit.GB unit }
                newValue = { oldValue | ram = newValue2 }
            in ({ model | parameters = newValue }, Cmd.none)
        ChangeCores cores ->
            let oldValue = model.parameters
                newValue = { oldValue | cores = cores }
            in ({ model | parameters = newValue }, Cmd.none)
        ChangeConnections connections ->
            let oldValue = model.parameters
                newValue = { oldValue | connections = connections }
            in ({ model | parameters = newValue }, Cmd.none)
        ChangeDataStorage dataStorage ->
            let oldValue = model.parameters
                newValue = { oldValue | dataStorage = Result.withDefault DS.HDD dataStorage }
            in ({ model | parameters = newValue }, Cmd.none)
        SubmitForm ->
            (model, Http.post
                        { url = "/api/configuration"
                        , body = Http.jsonBody (Sysconf.encode model.parameters)
                        , expect = Http.expectString GotConfig
                        })
        GotConfig res ->
            ( { model | configuration = Result.withDefault "err" res }, Cmd.none)

view : Model -> Html Msg
view model =
  let params = model.parameters
  in div []
    [ selectp postgresVersionP ChangeDbVersion params.dbVersion [] postgresVersions
    , selectp osTypeP ChangeOsType params.osType [] osTypes
    , selectp dbApplicationP ChangeDbApplication params.dbApplication [] dbApplications
    , input [ type_ "text", placeholder "RAM", value (String.fromInt params.ram.memory), onInput (\mem -> ChangeRam (Mem.Memory (Maybe.withDefault params.ram.memory (String.toInt mem)) params.ram.unit)) ] []
    , selectp sizeUnitP ChangeRamUnit params.ram.unit [] sizeUnits
    , input [ type_ "text", placeholder "Cores", value (Maybe.withDefault "" (Maybe.map String.fromInt params.cores)), onInput (ChangeCores << String.toInt) ] []
    , input [ type_ "text", placeholder "Connections", value (Maybe.withDefault "" (Maybe.map String.fromInt params.connections)), onInput (ChangeConnections << String.toInt) ] []
    , selectp dataStorageP ChangeDataStorage params.dataStorage [] dataStorages
    , button [ onClick SubmitForm ] [ text "Submit" ]
    , pre [] [text model.configuration]
    ]

postgresVersions = 
    [ ("9.4", Postgres.V94)
    , ("9.5", Postgres.V95)
    , ("9.6", Postgres.V96)
    , ("10", Postgres.V10)
    , ("11", Postgres.V11)
    , ("12", Postgres.V12)
    ]

osTypes =
    [ ("Linux", Os.Linux)
    , ("Windows", Os.Windows)
    , ("MacOS", Os.MacOsX)
    ]

dbApplications = 
    [ ("Web", DbApp.WEB)
    , ("OLTP", DbApp.OLTP)
    , ("Data Warehouse", DbApp.DATAWAREHOUSE)
    , ("Desktop", DbApp.DESKTOP)
    , ("Mixed", DbApp.MIXED)
    ]

dataStorages =
    [ ("HDD", DS.HDD)
    , ("SAN", DS.SAN)
    , ("SSD", DS.SSD)
    ]

sizeUnits = 
    [ ("MB", Unit.MB)
    , ("GB", Unit.GB)
    , ("TB", Unit.TB)
    ]

viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
  input [ type_ t, placeholder p, value v, onInput toMsg ] []

postgresVersionP : Prism String Postgres.PostgresVersion
postgresVersionP =
    let fromString : String -> Maybe Postgres.PostgresVersion
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
    in Prism fromString toString


osTypeP : Prism String Os.OperatingSystem
osTypeP =
    let fromString : String -> Maybe Os.OperatingSystem
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

    in Prism fromString toString

dbApplicationP : Prism String DbApp.DbApplication
dbApplicationP =
    let fromString : String -> Maybe DbApp.DbApplication
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
    in Prism fromString toString

dataStorageP : Prism String DS.DataStorage
dataStorageP =
    let fromString : String -> Maybe DS.DataStorage
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
    in Prism fromString toString

sizeUnitP =
    let fromString : String -> Maybe Unit.SizeUnit
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
    in Prism fromString toString
