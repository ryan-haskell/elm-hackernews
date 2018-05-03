module Types exposing (..)

import Navigation


type Page
    = Home
    | Story Int
    | NotFound


type alias Context =
    { location : Navigation.Location
    }
