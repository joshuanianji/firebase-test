module Data.Message exposing (..)

import Data.User as User exposing (User)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode
import Time exposing (Posix, Zone)


type alias Message =
    { content : String
    , time : Posix
    , from : User
    }



-- HELPERS


extractReadableTime : ( Posix, Zone ) -> Message -> String
extractReadableTime ( now, zone ) m =
    -- if it was sent less than a day ago just write the time
    if Time.posixToMillis now - Time.posixToMillis m.time < 86400000 then
        let
            hours =
                Time.toHour zone m.time
                    |> (\hrs -> hrs |> modBy 24)
                    |> String.fromInt

            minutes =
                Time.toMinute zone m.time
                    |> (\mins -> mins |> modBy 60)
                    |> String.fromInt
                    |> String.padLeft 2 '0'
        in
        hours ++ ":" ++ minutes

    else
        -- just use month/day/year lmaoooo
        -- will add more cases later
        let
            year =
                Time.toYear zone m.time
                    |> String.fromInt
                    |> String.right 2

            month =
                Time.toMonth zone m.time
                    |> toNumberMonth
                    |> String.fromInt

            day =
                Time.toDay zone m.time
                    |> String.fromInt
        in
        String.join "/" [ month, day, year ]


toNumberMonth : Time.Month -> Int
toNumberMonth month =
    case month of
        Time.Jan ->
            1

        Time.Feb ->
            2

        Time.Mar ->
            3

        Time.Apr ->
            4

        Time.May ->
            5

        Time.Jun ->
            6

        Time.Jul ->
            7

        Time.Aug ->
            8

        Time.Sep ->
            9

        Time.Oct ->
            10

        Time.Nov ->
            11

        Time.Dec ->
            12



-- JSON DECODERS


decodeList : Decoder (List Message)
decodeList =
    Decode.succeed identity
        |> Pipeline.required "messages" (Decode.list decode)


decode : Decoder Message
decode =
    Decode.succeed Message
        |> Pipeline.required "content" Decode.string
        |> Pipeline.required "time" (Decode.int |> Decode.map Time.millisToPosix)
        |> Pipeline.required "from" User.decode



-- JSON ENCODERS


encode : Message -> Encode.Value
encode dater =
    Encode.object
        [ ( "content", Encode.string dater.content )
        , ( "time", Encode.int <| Time.posixToMillis dater.time )
        , ( "from", User.encode dater.from )
        ]
