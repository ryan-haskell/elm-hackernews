module Routes exposing (page, parse)

import Navigation exposing (Location)
import Types exposing (Page(..))
import UrlParser exposing ((</>), Parser, s, top)


route : Parser (Page -> Page) Page
route =
    UrlParser.oneOf
        [ UrlParser.map Home <| top
        , UrlParser.map Story <| s "stories" </> UrlParser.int
        ]


page : Location -> Page
page location =
    case parse location of
        Just page ->
            page

        Nothing ->
            NotFound


parse : Location -> Maybe Page
parse location =
    UrlParser.parseHash route location
