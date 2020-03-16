module Modules.Icon exposing (..)

{-| The latest version of Elm-FontAwesome is 4.0.0

    But Elm only downloads 1.0.1 and throws an error when I manually change it in the elm.json
    "The dependencies in your elm.json are not compatible."

    hHH it's so annoying. I have to stick with 1.0.1

-}

import Element exposing (Element)
import FontAwesome.Attributes
import Html


type Icon msg
    = Icon (Options msg)


type alias FontAwesomeIcon msg =
    List (Html.Attribute msg) -> Html.Html msg


type alias Options msg =
    { icon : FontAwesomeIcon msg
    , animation : Animation
    }


type Animation
    = Spin
    | Pulse
    | NoAnimation


default : FontAwesomeIcon msg -> Icon msg
default icon =
    Icon
        { icon = icon
        , animation = NoAnimation
        }


withAnimation : Animation -> Icon msg -> Icon msg
withAnimation animation (Icon options) =
    Icon { options | animation = animation }



-- VIEW


view : Icon msg -> Element msg
view (Icon options) =
    let
        animation =
            case options.animation of
                Spin ->
                    [ FontAwesome.Attributes.spin ]

                Pulse ->
                    [ FontAwesome.Attributes.pulse ]

                NoAnimation ->
                    []
    in
    options.icon animation
        |> Element.html
