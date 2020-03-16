module Modules.ChatInterface exposing (..)

import Data.Message as Message exposing (Message)
import Data.User as User exposing (User)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import FontAwesome.Solid as Solid
import Html exposing (Html)
import InfiniteList
import Modules.Icon as Icon
import Time


type ChatInterface msg
    = ChatInterface (Options msg)


{-| TODO:

  - theme
  - other cool stuff
  - possibly change infinite list view to infinite scroll to make my code better

-}
type alias Options msg =
    { onSend : msg
    , onChange : String -> msg
    , currentUser : User
    , infiniteList : InfiniteList.Model
    , userInput : Maybe String
    , height : Int -- displayed height of the chat screen
    , time : Time.Posix
    , zone : Time.Zone
    , messageState : MessageState
    }


type MessageState
    = CannotSend -- greyed out
    | Idle -- theme color
    | Sending -- loading sign



-- HELPERS


type alias Msgs msg =
    { onSend : msg
    , onChange : String -> msg
    }


default : Msgs msg -> User -> ChatInterface msg
default msgs currUser =
    ChatInterface
        { onSend = msgs.onSend
        , onChange = msgs.onChange
        , currentUser = currUser
        , infiniteList = InfiniteList.init
        , userInput = Nothing
        , height = 0
        , time = Time.millisToPosix 0
        , zone = Time.utc
        , messageState = Idle
        }



-- i will give the ChatInterface a way to get what is being written at the moment.


withUserInput : String -> ChatInterface msg -> ChatInterface msg
withUserInput str (ChatInterface options) =
    ChatInterface { options | userInput = Just str }


withHeight : Int -> ChatInterface msg -> ChatInterface msg
withHeight height (ChatInterface options) =
    ChatInterface { options | height = height }


withTimeAndZone : Time.Posix -> Time.Zone -> ChatInterface msg -> ChatInterface msg
withTimeAndZone time zone (ChatInterface options) =
    ChatInterface
        { options
            | time = time
            , zone = zone
        }


withMessageState : MessageState -> ChatInterface msg -> ChatInterface msg
withMessageState msgState (ChatInterface options) =
    ChatInterface { options | messageState = msgState }



-- VIEW


view : List Message -> ChatInterface msg -> Element msg
view messages (ChatInterface options) =
    let
        sendButtonTemplate maybeMsg elem =
            Input.labelRight
                ([ Font.bold
                 , Font.size 16
                 , centerY
                 , width (px 24)
                 ]
                    ++ extraAttributes maybeMsg
                )
            <|
                elem

        extraAttributes maybeMsg =
            case maybeMsg of
                Nothing ->
                    [ Font.color <| rgb 0.2 0.2 0.2 ]

                Just msg ->
                    [ Font.color <| rgb255 0 0 139, Events.onClick msg, pointer ]

        airplane =
            Icon.default Solid.paperPlane
                |> Icon.view

        spinner =
            Icon.default Solid.spinner
                |> Icon.withAnimation Icon.Spin
                |> Icon.view

        sendButton =
            case options.messageState of
                CannotSend ->
                    sendButtonTemplate Nothing airplane

                Idle ->
                    sendButtonTemplate (Just options.onSend) airplane

                Sending ->
                    sendButtonTemplate Nothing spinner
    in
    Element.column
        [ width fill
        , height fill
        , spacing 32
        ]
        [ InfiniteList.view (config options) options.infiniteList messages
            |> Element.html
        , Input.text
            [ Input.focusedOnLoad
            , width fill
            , Border.color <| rgb 0 0 0
            , Border.rounded 8
            , Border.width 2
            ]
            { onChange = options.onChange
            , text = Maybe.withDefault "" options.userInput
            , placeholder = Just <| Input.placeholder [ Font.color <| rgb 0.5 0.5 0.5 ] (text "Chat online!")
            , label = sendButton
            }
        ]



-- view singular message


config : Options msg -> InfiniteList.Config Message msg
config options =
    InfiniteList.config
        { itemView = itemView options
        , itemHeight = InfiniteList.withConstantHeight options.height
        , containerHeight = 1000
        }


itemView : Options msg -> Int -> Int -> Message -> Html msg
itemView options _ _ message =
    let
        sameUser =
            message.from |> User.isEqualTo options.currentUser

        align =
            if sameUser then
                alignRight

            else
                alignLeft

        timeStamp =
            Element.paragraph
                [ Background.color <| rgb 0 0 0
                , Font.color <| rgb 1 1 1
                , Font.size 12
                , centerX
                ]
                [ text <| Message.extractReadableTime ( options.time, options.zone ) message ]
    in
    Element.paragraph
        [ spacing 15
        , Background.color <| rgb 0.8 0.8 0.8
        , Border.rounded 8
        , align
        , maximum 500 shrink
            |> width
        , padding 8
        , if sameUser then
            Element.onRight timeStamp

          else
            Element.onLeft timeStamp
        ]
        [ text message.content ]
        -- the InfiniteList library needs our itemView to be an Html
        |> Element.layout []
