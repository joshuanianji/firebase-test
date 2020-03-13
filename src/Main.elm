module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Element exposing (spacing, text)
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Iso8601
import Json.Decode
import Json.Decode.Pipeline
import Json.Encode
import Ports
import Time exposing (Posix)



---- MODEL ----


type alias Message =
    { content : String
    , time : Posix
    }


type alias ErrorData =
    { code : Maybe String
    , message : Maybe String
    , credential : Maybe String
    }


type alias UserData =
    { token : String
    , email : String
    , uid : String
    }


type alias Model =
    { userData : Maybe UserData
    , error : ErrorData
    , inputContent : String
    , messages : List Message
    }


init : ( Model, Cmd Msg )
init =
    ( { userData = Nothing, error = emptyError, inputContent = "", messages = [] }, Cmd.none )



---- UPDATE ----


type Msg
    = LogIn
    | LogOut
    | LoggedInData (Result Json.Decode.Error UserData)
    | LoggedInError (Result Json.Decode.Error ErrorData)
    | SaveMessage
    | InputChanged String
    | MessagesReceived (Result Json.Decode.Error (List Message))


emptyError : ErrorData
emptyError =
    { code = Nothing, credential = Nothing, message = Nothing }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LogIn ->
            ( model, Ports.signIn () )

        LogOut ->
            ( { model | userData = Nothing, error = emptyError }, Ports.signOut () )

        LoggedInData result ->
            case result of
                Ok value ->
                    ( { model | userData = Just value }, Cmd.none )

                Err error ->
                    ( { model | error = messageToError <| Json.Decode.errorToString error }, Cmd.none )

        LoggedInError result ->
            case result of
                Ok value ->
                    ( { model | error = value }, Cmd.none )

                Err error ->
                    ( { model | error = messageToError <| Json.Decode.errorToString error }, Cmd.none )

        SaveMessage ->
            ( model, Ports.saveMessage <| messageEncoder model )

        InputChanged value ->
            ( { model | inputContent = value }, Cmd.none )

        MessagesReceived result ->
            case result of
                Ok value ->
                    ( { model | messages = value }, Cmd.none )

                Err error ->
                    ( { model | error = messageToError <| Json.Decode.errorToString error }, Cmd.none )


messageEncoder : Model -> Json.Encode.Value
messageEncoder model =
    Json.Encode.object
        [ ( "content", Json.Encode.string model.inputContent )
        , ( "uid"
          , case model.userData of
                Just userData ->
                    Json.Encode.string userData.uid

                Nothing ->
                    Json.Encode.null
          )
        ]


messageToError : String -> ErrorData
messageToError message =
    { code = Nothing, credential = Nothing, message = Just message }


errorPrinter : ErrorData -> String
errorPrinter errorData =
    Maybe.withDefault "" errorData.code ++ " " ++ Maybe.withDefault "" errorData.credential ++ " " ++ Maybe.withDefault "" errorData.message


userDataDecoder : Json.Decode.Decoder UserData
userDataDecoder =
    Json.Decode.succeed UserData
        |> Json.Decode.Pipeline.required "token" Json.Decode.string
        |> Json.Decode.Pipeline.required "email" Json.Decode.string
        |> Json.Decode.Pipeline.required "uid" Json.Decode.string


logInErrorDecoder : Json.Decode.Decoder ErrorData
logInErrorDecoder =
    Json.Decode.succeed ErrorData
        |> Json.Decode.Pipeline.required "code" (Json.Decode.nullable Json.Decode.string)
        |> Json.Decode.Pipeline.required "message" (Json.Decode.nullable Json.Decode.string)
        |> Json.Decode.Pipeline.required "credential" (Json.Decode.nullable Json.Decode.string)


messagesDecoder : Json.Decode.Decoder Message
messagesDecoder =
    Json.Decode.succeed Message
        |> Json.Decode.Pipeline.required "content" Json.Decode.string
        |> Json.Decode.Pipeline.required "time" (Json.Decode.int |> Json.Decode.map Time.millisToPosix)


messageListDecoder : Json.Decode.Decoder (List Message)
messageListDecoder =
    Json.Decode.succeed identity
        |> Json.Decode.Pipeline.required "messages" (Json.Decode.list messagesDecoder)



---- VIEW ----


view : Model -> Html Msg
view model =
    Element.column
        []
        [ Element.el [] <| Element.text "Your Elm App is working! Yay!!"
        , case model.userData of
            Just data ->
                Input.button []
                    { onPress = Just LogOut
                    , label = text "Logout from Google"
                    }

            Nothing ->
                Input.button []
                    { onPress = Just LogIn
                    , label = text "Login from Google"
                    }
        , Element.text <|
            case model.userData of
                Just data ->
                    data.email ++ " " ++ data.uid ++ " " ++ data.token

                Nothing ->
                    ""
        , case model.userData of
            Just data ->
                Element.column []
                    [ Input.text []
                        { onChange = InputChanged
                        , text = model.inputContent
                        , placeholder = Just <| Input.placeholder [] (Element.text "Message to save")
                        , label = Input.labelAbove [] <| Element.text "Awesome."
                        }
                    , Input.button []
                        { onPress = Just SaveMessage
                        , label = text "Save new message"
                        }
                    ]

            Nothing ->
                Element.none
        , Element.column []
            [ Element.text "Previous messages"
            , Element.column [] <|
                List.map
                    (\message ->
                        Element.column
                            [ spacing 4 ]
                            [ Element.el [ Font.bold, Font.size 10 ] <| text <| posixToString message.time
                            , Element.el [] <| text message.content
                            ]
                    )
                    model.messages
            ]
        , Element.el [] <| text <| errorPrinter model.error
        ]
        |> Element.layout []


posixToString : Posix -> String
posixToString =
    Iso8601.fromTime



---- PROGRAM ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Ports.signInInfo (Json.Decode.decodeValue userDataDecoder >> LoggedInData)
        , Ports.signInError (Json.Decode.decodeValue logInErrorDecoder >> LoggedInError)
        , Ports.receiveMessages (Json.Decode.decodeValue messageListDecoder >> MessagesReceived)
        ]


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }
