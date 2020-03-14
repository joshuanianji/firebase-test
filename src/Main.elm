module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Colours
import Data.Error as Error exposing (Error)
import Data.Message as Message exposing (Message)
import Data.User as User exposing (User)
import Element exposing (Element, centerX, fill, fillPortion, height, padding, rgb, spacing, text, width)
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Iso8601
import Json.Decode as Json
import Modules.ChatInterface as ChatInterface
import Modules.Navbar as Navbar
import Ports
import Task
import Time



---- BROWSER ----
{--
-- routing LATER

main : Program () Model Msg
main =
    Browser.application
        { init : flags -> Url -> Key -> ( model, Cmd msg )
        , view : model -> Document msg
        , update : msg -> model -> ( model, Cmd msg )
        , subscriptions : model -> Sub msg
        , onUrlRequest : UrlRequest -> msg
        , onUrlChange : Url -> msg
        }
--}


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }



---- MODEL ----


type alias Model =
    { user : User.User -- can be Anonymous so no need for a maybe type
    , error : Error.Error
    , inputContent : String
    , messages : List Message.Message
    , navbarToggleOpened : Bool
    , time : Time.Posix
    , zone : Time.Zone
    }


init : ( Model, Cmd Msg )
init =
    ( { user = User.Anonymous
      , error = Error.empty
      , inputContent = ""
      , messages = []
      , navbarToggleOpened = False
      , time = Time.millisToPosix 0
      , zone = Time.utc
      }
    , Task.perform AdjustTimeZone Time.here
    )



---- VIEW ----


view : Model -> Html Msg
view model =
    Element.column
        [ centerX
        , spacing 32
        , width fill
        ]
        [ navbar model
        , chatInterface model
            |> surround
        , Element.el [] <| text <| Error.toString model.error
        ]
        |> Element.layout [ width fill ]


surround : Element Msg -> Element Msg
surround e =
    Element.row
        [ width fill, height fill ]
        [ Element.el [ width <| fillPortion 1 ] Element.none
        , Element.el [ width <| fillPortion 3 ] e
        , Element.el [ width <| fillPortion 1 ] Element.none
        ]


navbar : Model -> Element Msg
navbar model =
    let
        state =
            { toggleOpened = model.navbarToggleOpened
            , user = model.user
            }
    in
    Navbar.default
        { dropdown = ToggleMenu
        , login = LogIn
        , logout = LogOut
        }
        |> Navbar.view state


chatInterface : Model -> Element Msg
chatInterface model =
    ChatInterface.default
        { onSend = SaveMessage
        , onChange = InputChanged
        }
        model.user
        |> ChatInterface.withUserInput model.inputContent
        |> ChatInterface.withHeight 40
        |> ChatInterface.withTimeAndZone model.time model.zone
        |> ChatInterface.view model.messages
        |> Element.el
            [ padding 16
            , width fill
            , height fill
            ]



---- UPDATE ----


type Msg
    = LogIn
    | LogOut
    | LoggedInData (Result Json.Error User)
    | LoggedInError (Result Json.Error Error)
    | SaveMessage
    | SendMessage Time.Posix
    | InputChanged String
    | MessagesReceived (Result Json.Error (List Message))
    | ToggleMenu
    | Tick Time.Posix
    | AdjustTimeZone Time.Zone


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LogIn ->
            ( model, Ports.signIn () )

        LogOut ->
            ( { model
                | user = User.Anonymous
                , error = Error.empty
                , inputContent = ""
                , navbarToggleOpened = False
              }
            , Ports.signOut ()
            )

        LoggedInData result ->
            case result of
                Ok user ->
                    ( { model | user = user }, Cmd.none )

                Err error ->
                    ( { model | error = Error.fromJsonError error }, Cmd.none )

        LoggedInError result ->
            case result of
                Ok value ->
                    ( { model | error = value }, Cmd.none )

                Err error ->
                    ( { model | error = Error.fromJsonError error }, Cmd.none )

        -- first gets the time
        SaveMessage ->
            if model.inputContent == "" then
                ( model, Cmd.none )

            else
                ( model, Task.perform SendMessage Time.now )

        -- actually sends out the message
        SendMessage time ->
            ( { model | inputContent = "" }
            , Ports.saveMessage <|
                Message.encode
                    { content = model.inputContent
                    , from = model.user
                    , time = time
                    }
            )

        InputChanged value ->
            ( { model | inputContent = value }, Cmd.none )

        MessagesReceived result ->
            case result of
                Ok value ->
                    ( { model | messages = value }, Cmd.none )

                Err error ->
                    ( { model | error = Error.fromJsonError error }, Cmd.none )

        ToggleMenu ->
            ( { model | navbarToggleOpened = not model.navbarToggleOpened }, Cmd.none )

        Tick time ->
            ( { model | time = time }, Cmd.none )

        AdjustTimeZone zone ->
            ( { model | zone = zone }, Cmd.none )



---- PROGRAM ----


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Ports.signInInfo (Json.decodeValue User.decode >> LoggedInData)
        , Ports.signInError (Json.decodeValue Error.decode >> LoggedInError)
        , Ports.receiveMessages (Json.decodeValue Message.decodeList >> MessagesReceived)
        , Time.every 1000 Tick
        ]
