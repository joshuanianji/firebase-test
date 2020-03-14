module Data.Error exposing (..)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline


type alias Error =
    { code : Maybe String
    , message : Maybe String
    , credential : Maybe String
    }


empty : Error
empty =
    { code = Nothing, credential = Nothing, message = Nothing }



-- JSON DECODE


decode : Decoder Error
decode =
    Decode.succeed Error
        |> Pipeline.required "code" (Decode.nullable Decode.string)
        |> Pipeline.required "message" (Decode.nullable Decode.string)
        |> Pipeline.required "credential" (Decode.nullable Decode.string)


fromJsonError : Decode.Error -> Error
fromJsonError decodeError =
    Decode.errorToString decodeError
        |> fromString



-- HELPERS


fromString : String -> Error
fromString message =
    { code = Nothing, credential = Nothing, message = Just message }


toString : Error -> String
toString errorData =
    Maybe.withDefault "" errorData.code ++ " " ++ Maybe.withDefault "" errorData.credential ++ " " ++ Maybe.withDefault "" errorData.message
