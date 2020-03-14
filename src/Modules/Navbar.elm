module Modules.Navbar exposing (..)

{-| Simplistic navbar with little flexibility - I just want to get things working loll

    I have a dropdown on the right for the User login and account stuff
    I will provide a list of links on the left, though right now they will not have dropdown capabilities

-}

import Data.User as User exposing (User)
import Element exposing (..)
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input



-- TYPES


{-| Though rather simplistic this can scale. it captures the entire state of my navbar and is held in the model of the elm file where the navbar resides.
-}
type alias NavbarState =
    { toggleOpened : Bool
    , user : User
    }


type Navbar msg
    = Navbar (Options msg)


type alias Options msg =
    { dropdownToggleMsg : msg
    , logInMsg : msg
    , logOutMsg : msg
    , links : List Link
    }


type Link
    = Link Text Url


type alias Text =
    String


type alias Url =
    String


linkFromTuple : ( String, String ) -> Link
linkFromTuple ( text, url ) =
    Link text url


linkToElement : Link -> Element msg
linkToElement (Link labelString url) =
    link
        []
        { url = url
        , label = text labelString
        }



-- HELPERs
-- If i make themes or make the navbar more complicated I'll add more things here lol


type alias Msgs msg =
    { dropdown : msg
    , login : msg
    , logout : msg
    }


default : Msgs msg -> Navbar msg
default msgs =
    Navbar
        { dropdownToggleMsg = msgs.dropdown
        , logInMsg = msgs.login
        , logOutMsg = msgs.logout
        , links = List.map linkFromTuple [ ( "General Chat", "#" ), ( "Private Chats", "#" ) ]
        }



-- VIEW


view : NavbarState -> Navbar msg -> Element msg
view state (Navbar options) =
    let
        user =
            case state.user of
                User.Anonymous ->
                    Input.button []
                        { onPress = Just options.logInMsg
                        , label = text "Login with Google"
                        }

                User.Identified _ ->
                    row
                        [ spacing 10
                        , Events.onClick options.dropdownToggleMsg
                        , pointer
                        , below <|
                            if state.toggleOpened then
                                column
                                    [ spacing 8
                                    , padding 16
                                    ]
                                    [ Input.button []
                                        { onPress = Just options.logOutMsg
                                        , label = text "Logout"
                                        }
                                    , linkToElement <| linkFromTuple ( "Settings", "#" )
                                    ]

                            else
                                none
                        ]
                        [ text "Logged in!"
                        , text <|
                            if state.toggleOpened then
                                "▲"

                            else
                                "▼"
                        ]
    in
    row
        [ width fill
        , spacing 8
        , padding 16

        -- use behindContent so it would actually center without being interfered by the other elements
        , behindContent <|
            el
                [ Font.bold
                , Font.size 24
                , centerX
                , padding 16
                ]
            <|
                text "JoshuaTalk"
        ]
        [ List.map linkToElement options.links
            |> row
                [ alignLeft
                , spacing 8
                , Font.size 16
                ]
        , el [ alignRight, Font.size 16 ] user
        ]
