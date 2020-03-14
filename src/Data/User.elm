module Data.User exposing (..)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode


type User
    = Anonymous
    | Identified Info


type alias Info =
    { token : String
    , email : String
    , uid : String
    }



--JSON DECODE
-- would be "null" if anonymous


decode : Decoder User
decode =
    Decode.oneOf
        [ Decode.null Anonymous
        , Decode.map Identified infoDecode
        ]


infoDecode : Decoder Info
infoDecode =
    Decode.succeed Info
        |> Pipeline.required "token" Decode.string
        |> Pipeline.required "email" Decode.string
        |> Pipeline.required "uid" Decode.string



-- JSON ENCODE (used for Message.elm)


encode : User -> Encode.Value
encode u =
    case u of
        Anonymous ->
            Encode.null

        Identified info ->
            Encode.object
                [ ( "token", Encode.string info.token )
                , ( "email", Encode.string info.email )
                , ( "uid", Encode.string info.uid )
                ]



-- HELPERS


fromInfo : Info -> User
fromInfo =
    Identified


toString : User -> String
toString u =
    case u of
        Identified data ->
            "Email: " ++ data.email

        Anonymous ->
            "Anonymous"



-- compares two users


isEqualTo : User -> User -> Bool
isEqualTo u1 u2 =
    case ( u1, u2 ) of
        ( Identified user1, Identified user2 ) ->
            user1.uid == user2.uid

        _ ->
            False
