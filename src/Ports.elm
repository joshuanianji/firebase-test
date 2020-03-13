port module Ports exposing (..)

import Json.Encode


port signIn : () -> Cmd msg


port signInInfo : (Json.Encode.Value -> msg) -> Sub msg


port signInError : (Json.Encode.Value -> msg) -> Sub msg


port signOut : () -> Cmd msg


port saveMessage : Json.Encode.Value -> Cmd msg


port receiveMessages : (Json.Encode.Value -> msg) -> Sub msg
