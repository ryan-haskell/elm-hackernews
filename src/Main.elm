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
        ( HackerNews
        , Story
        , topStories
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
    = InitializeTopStories (Result String (List (HackerNews Story)))
    | AddTopStory Int (Result String Story)
    | LoadMoreStories


type alias Model =
    { topStories : List (HackerNews Story)
    }


init : ( Model, Cmd Msg )
init =
    Model [] ! [ HackerNews.fetch topStories InitializeTopStories ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InitializeTopStories (Ok topStories) ->
            loadNext { model | topStories = topStories }

        InitializeTopStories (Err error) ->
            let
                _ =
                    Debug.log "SetTopStories" error
            in
            model ! []

        AddTopStory id (Ok story) ->
            { model | topStories = HackerNews.updateWithItem ( id, story ) model.topStories } ! []

        AddTopStory id (Err reason) ->
            { model | topStories = HackerNews.updateWithError ( id, reason ) model.topStories } ! []

        LoadMoreStories ->
            loadNext model


loadNext : Model -> ( Model, Cmd Msg )
loadNext model =
    HackerNews.viewMoreStories model.topStories AddTopStory
        |> (\( topStories, cmds ) ->
                ( { model | topStories = topStories }, cmds )
           )


view : Model -> Html Msg
view model =
    layout
        []
        (page model)


page : Model -> Element Msg
page { topStories } =
    column
        [ Background.color colors.softGray
        ]
        [ viewNavbar
        , container <| viewStories (HackerNews.items topStories)
        , container <|
            el
                [ padding 24
                , width fill
                ]
            <|
                if HackerNews.hasMore topStories then
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
            row []
                [ link [ alignLeft ]
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
                , el [ alignRight ] (text "top")
                ]
        ]


viewStories : List Story -> Element Msg
viewStories stories =
    column
        [ paddingXY 0 12
        , spacing 12
        ]
        (List.indexedMap storyCard stories)


containerWidth : Attribute msg
containerWidth =
    width <|
        fillPortionBetween
            { portion = 100000
            , min = Nothing
            , max = Just 512
            }


container : Element msg -> Element msg
container child =
    row [ width fill ]
        [ el [ width <| fillPortion 1 ] empty
        , el
            [ containerWidth
            ]
            child
        , el [ width <| fillPortion 1 ] empty
        ]


cardTitle : String -> Element msg
cardTitle title =
    paragraph
        [ Font.size 20
        , Font.semiBold
        , Font.color Color.darkOrange
        ]
        [ text title
        ]


date : Int -> String
date =
    toFloat
        >> (*) Time.second
        >> Date.fromTime
        >> DateFormat.format
            [ DateFormat.monthNumber
            , DateFormat.text "/"
            , DateFormat.dayOfMonthNumber
            , DateFormat.text ", "
            , DateFormat.hourNumber
            , DateFormat.text ":"
            , DateFormat.minuteFixed
            , DateFormat.text " "
            , DateFormat.amPmUppercase
            ]


paddingRight : Int -> Attribute msg
paddingRight num =
    paddingEach
        { bottom = 0
        , left = 0
        , right = num
        , top = 0
        }


storyCard : Int -> Story -> Element Msg
storyCard index { title, time, by, score } =
    card
        [ spacing 8
        , padding 24
        , onLeft <|
            el
                [ paddingRight 16
                , Font.color colors.shadow
                , Font.size 24
                , Font.semiBold
                , centerY
                ]
                (text <| toString (index + 1))
        ]
        [ row
            [ spacing 24 ]
            [ column
                [ spacing 8
                ]
                [ cardTitle title
                , row
                    [ Font.size 16
                    , Font.color Color.darkCharcoal
                    ]
                    [ row
                        [ alignLeft
                        ]
                        [ el [] (text by)
                        , text <| " - " ++ date time
                        ]
                    ]
                ]
            , el
                [ width shrink
                , paddingXY 0 4
                ]
                (viewScore score)
            ]
        ]


viewScore : Int -> Element msg
viewScore score =
    column
        [ height shrink, centerY ]
        [ el
            [ Font.color Color.charcoal
            , Font.semiBold
            , Font.size 12
            , centerX
            ]
            (text <| String.toUpper "Score")
        , el
            [ Font.color Color.charcoal
            , Font.size 24
            , Font.semiBold
            , centerX
            ]
            (text <| toString score)
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
