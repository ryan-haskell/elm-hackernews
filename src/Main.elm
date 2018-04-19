module Main exposing (..)

import Color exposing (Color)
import Date
import DateFormat
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Http
import Json.Decode as Decode
    exposing
        ( Decoder
        , bool
        , int
        , list
        , string
        )
import Json.Decode.Pipeline
    exposing
        ( decode
        , optional
        , required
        )
import Time


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }


type alias ItemId =
    Int


type Msg
    = LoadTopStories (Result Http.Error (List ItemId))
    | LoadStory ItemId (Result Http.Error StoryItem)
    | LoadMoreStories


type HackerNewsData a
    = ReadyToFetch ItemId
    | Fetching ItemId
    | Success a
    | Failure String


pageSize : Int
pageSize =
    10


type alias Model =
    { stories : List (HackerNewsData StoryItem)
    }


init : ( Model, Cmd Msg )
init =
    Model [] ! [ requestTopStories ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadTopStories (Ok storyIds) ->
            let
                storiesToFetch =
                    List.take pageSize storyIds

                idsToSave =
                    List.drop pageSize storyIds
            in
            { model
                | stories =
                    List.concat
                        [ List.map Fetching storiesToFetch
                        , List.map ReadyToFetch idsToSave
                        ]
            }
                ! List.map requestStory storiesToFetch

        LoadTopStories (Err error) ->
            { model | stories = [] } ! []

        LoadMoreStories ->
            loadNext pageSize model

        LoadStory id (Ok story) ->
            { model | stories = List.map (setStory id story) model.stories } ! []

        LoadStory id (Err error) ->
            { model | stories = List.map (setError id (toString error)) model.stories } ! []


loadNext : Int -> Model -> ( Model, Cmd Msg )
loadNext count model =
    let
        ( fetchableItems, fetchedItems ) =
            List.partition hasItemId model.stories

        itemsToLoad =
            List.take count fetchableItems
                |> List.map (getItemId >> Maybe.withDefault 0 >> Fetching)

        idsToLoad =
            List.map (getItemId >> Maybe.withDefault 0) itemsToLoad

        itemsReadyToFetch =
            List.drop count fetchableItems
    in
    { model
        | stories = fetchedItems ++ itemsToLoad ++ itemsReadyToFetch
    }
        ! List.map requestStory idsToLoad


getItemId : HackerNewsData a -> Maybe ItemId
getItemId data =
    case data of
        ReadyToFetch id ->
            Just id

        Fetching id ->
            Just id

        Success _ ->
            Nothing

        Failure _ ->
            Nothing


hasItemId : HackerNewsData a -> Bool
hasItemId =
    getItemId >> (/=) Nothing


setStory : ItemId -> StoryItem -> HackerNewsData StoryItem -> HackerNewsData StoryItem
setStory id item data =
    case data of
        Fetching itemId ->
            if itemId == id then
                Success item
            else
                data

        _ ->
            data


setError : ItemId -> String -> HackerNewsData StoryItem -> HackerNewsData StoryItem
setError id message data =
    case data of
        Fetching itemId ->
            if itemId == id then
                Failure message
            else
                data

        _ ->
            data


requestStory : ItemId -> Cmd Msg
requestStory id =
    Http.get
        ("https://hacker-news.firebaseio.com/v0/item/" ++ toString id ++ ".json")
        storyItemDecoder
        |> Http.send (LoadStory id)


requestTopStories : Cmd Msg
requestTopStories =
    Http.get "https://hacker-news.firebaseio.com/v0/topstories.json" itemIdDecoder
        |> Http.send LoadTopStories


itemIdDecoder : Decode.Decoder (List ItemId)
itemIdDecoder =
    Decode.list Decode.int


storyItemDecoder : Decoder StoryItem
storyItemDecoder =
    decode StoryItem
        |> required "id" int
        |> optional "deleted" bool False
        |> required "by" string
        |> required "time" int
        |> optional "text" string ""
        |> optional "dead" bool False
        |> optional "kids" (list int) []
        |> optional "url" string ""
        |> optional "score" int 0
        |> optional "title" string ""
        |> optional "descendants" int 0


type alias UnixTime =
    Int


type alias Username =
    String


type alias ItemType =
    String


type alias JobItem =
    { id : ItemId
    , deleted : Bool
    , by : Username
    , time : UnixTime
    , text : String
    , dead : Bool
    , url : String
    , score : Int
    , title : String
    }


type alias AskItem =
    { id : ItemId
    , deleted : Bool
    , by : Username
    , time : UnixTime
    , text : String
    , dead : Bool
    , kids : List ItemId
    , url : String
    , score : Int
    , title : String
    , descendants : Int
    }


type alias StoryItem =
    { id : ItemId
    , deleted : Bool
    , by : Username
    , time : UnixTime
    , text : String
    , dead : Bool
    , kids : List ItemId
    , url : String
    , score : Int
    , title : String
    , descendants : Int
    }


type alias CommentItem =
    { id : ItemId
    , deleted : Bool
    , by : Username
    , time : UnixTime
    , text : String
    , dead : Bool
    , parent : ItemId
    , kids : List ItemId
    }


type alias PollItem =
    { id : ItemId
    , deleted : Bool
    , by : Username
    , time : UnixTime
    , text : String
    , dead : Bool
    , kids : List ItemId
    , score : Int
    , title : String
    , parts : List ItemId
    , descendants : Int
    }


type alias PollOptionItem =
    { id : ItemId
    , deleted : Bool
    , by : Username
    , time : UnixTime
    , text : String
    , dead : Bool
    , poll : ItemId
    , score : Int
    }


view : Model -> Html Msg
view model =
    layout
        []
        (page model)


page : Model -> Element Msg
page { stories } =
    column
        [ Background.color colors.softGray
        ]
        [ viewNavbar
        , container <| viewStories stories
        , container <|
            el
                [ padding 24
                , width fill
                ]
            <|
                if hasMoreStoriesToFetch stories then
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


hasMoreStoriesToFetch : List (HackerNewsData a) -> Bool
hasMoreStoriesToFetch =
    List.any hasItemId


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


viewStories : List (HackerNewsData StoryItem) -> Element Msg
viewStories stories =
    column
        [ paddingXY 0 12
        , spacing 12
        ]
        (List.map viewStory stories)


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


viewStory : HackerNewsData StoryItem -> Element Msg
viewStory data =
    case data of
        ReadyToFetch _ ->
            disabledCard

        Fetching _ ->
            loadingCard

        Success story ->
            storyCard story

        Failure message ->
            errorCard message


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


storyCard : StoryItem -> Element Msg
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
