module Main exposing (..)

import Color exposing (Color)
import Date
import DateFormat
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import HackerNews
    exposing
        ( Story
        , StoryItems
        )
import Html exposing (Html)
import Time


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }


type Msg
    = SetTopStories (Result String StoryItems)
    | LoadMoreStories


pageSize : Int
pageSize =
    10


type alias Model =
    { storyItems : StoryItems
    }


init : ( Model, Cmd Msg )
init =
    Model HackerNews.emptyStories ! [ HackerNews.getTopStories SetTopStories ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetTopStories (Ok storyItems) ->
            { model
                | storyItems = storyItems
            }
                ! []

        SetTopStories (Err error) ->
            let
                _ =
                    Debug.log "SetTopStories" error
            in
            model ! []

        LoadMoreStories ->
            loadNext pageSize model


loadNext : Int -> Model -> ( Model, Cmd Msg )
loadNext count model =
    model
        ! [ HackerNews.viewMoreStories pageSize model.storyItems SetTopStories ]


view : Model -> Html Msg
view model =
    layout
        []
        (page model)


page : Model -> Element Msg
page { storyItems } =
    column
        [ Background.color colors.softGray
        ]
        [ viewNavbar
        , container <| viewStories (HackerNews.stories storyItems)
        , container <|
            el
                [ padding 24
                , width fill
                ]
            <|
                if HackerNews.hasMoreStories storyItems then
                    Input.button
                        [ centerX
                        , paddingXY 24 12
                        , Border.rounded 4
                        , Border.shadow softShadow
                        , Background.color colors.white
                        ]
                        { label = text "View more"
                        , onPress = Just LoadMoreStories
                        }
                else
                    el [ centerX ] (text "That's all the stories!")
        ]


type alias Colors =
    { orange : Color
    , white : Color
    , shadow : Color
    , softGray : Color
    }


colors : Colors
colors =
    { orange = Color.rgb 255 102 0
    , white = Color.white
    , shadow = Color.rgba 0 0 0 0.15
    , softGray = Color.rgb 235 235 235
    }


type alias Shadow =
    { offset : ( Float, Float )
    , blur : Float
    , color : Color
    , size : Float
    }


softShadow : Shadow
softShadow =
    { offset = ( 0, 2 )
    , blur = 4
    , color = colors.shadow
    , size = 0
    }


viewNavbar : Element Msg
viewNavbar =
    row
        [ padding 16
        , Background.color colors.orange
        , Font.color colors.white
        , Border.shadow softShadow
        , width fill
        ]
        [ container <|
            link []
                { url = "/"
                , label =
                    image
                        [ width (px 32)
                        , height (px 32)
                        ]
                        { src = "/logo.svg"
                        , description = "Elm logo"
                        }
                }
        ]


viewStories : List Story -> Element Msg
viewStories stories =
    column
        [ paddingXY 0 12
        , spacing 12
        ]
        (List.map storyCard stories)


container : Element msg -> Element msg
container =
    el
        [ centerX
        , width <| fillBetween { min = Nothing, max = Just 640 }
        ]


cardTitle : String -> Element msg
cardTitle title =
    paragraph
        [ Font.size 20
        , Font.semiBold
        ]
        [ text title
        ]


date : Int -> String
date =
    toFloat
        >> (*) Time.second
        >> Date.fromTime
        >> DateFormat.format
            [ DateFormat.monthNameFull
            , DateFormat.text " "
            , DateFormat.dayOfMonthSuffix
            , DateFormat.text " - "
            , DateFormat.hourNumber
            , DateFormat.text ":"
            , DateFormat.minuteFixed
            , DateFormat.text " "
            , DateFormat.amPmUppercase
            ]


storyCard : Story -> Element Msg
storyCard { title, time, by } =
    card [ spacing 8 ]
        [ row
            [ Font.size 16
            , Font.color Color.darkCharcoal
            ]
            [ el [ alignLeft ] (text by)
            , el [ alignRight ] (text <| date time)
            ]
        , cardTitle title
        ]


disabledCard : Element Msg
disabledCard =
    empty


loadingCard : Element Msg
loadingCard =
    card
        [ Font.color Color.gray
        , Background.color Color.lightGray
        , padding 24
        ]
        [ el [ centerX ] (text "Loading...") ]


errorCard : String -> Element Msg
errorCard message =
    card
        [ Background.color Color.lightGray
        , Font.color Color.darkRed
        ]
        [ text message ]


card : List (Attribute msg) -> List (Element msg) -> Element msg
card attrs children =
    column
        ([ Background.color colors.white
         , Border.shadow softShadow
         , paddingXY 16 28
         , height shrink
         , Font.size 18
         ]
            ++ attrs
        )
        children
