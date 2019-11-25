{-
   pgtune
   A service to generate some optimized configuration parameters for PostgreSQL based on best practices.

   The version of the OpenAPI document: 1.0
   Contact: sainth@sainth.de

   NOTE: This file is auto generated by the openapi-generator.
   https://github.com/openapitools/openapi-generator.git
   Do not edit this file manually.
-}


module Data.DataStorage exposing (DataStorage(..), decoder, encode)

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode


type DataStorage
    = HDD
    | SSD
    | SAN



decoder : Decoder DataStorage
decoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "HDD" ->
                        Decode.succeed HDD

                    "SSD" ->
                        Decode.succeed SSD

                    "SAN" ->
                        Decode.succeed SAN

                    other ->
                        Decode.fail <| "Unknown type: " ++ other
            )



encode : DataStorage -> Encode.Value
encode model =
    case model of
        HDD ->
            Encode.string "HDD"

        SSD ->
            Encode.string "SSD"

        SAN ->
            Encode.string "SAN"

