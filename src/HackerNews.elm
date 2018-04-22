module HackerNews
    exposing
        ( Story
        , StoryItems
        , emptyStories
        , getTopStories
        , hasMoreStories
        , stories
        , viewMoreStories
        )

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
import Task exposing (Task)


type alias ItemId =
    Int


type alias UnixTime =
    Int


type alias Username =
    String


type alias ItemType =
    String


type alias Story =
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


storyItemDecoder : Decoder Story
storyItemDecoder =
    decode Story
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


commentItemDecoder : Decoder CommentItem
commentItemDecoder =
    decode CommentItem
        |> required "id" int
        |> optional "deleted" bool False
        |> required "by" string
        |> required "time" int
        |> optional "text" string ""
        |> optional "dead" bool False
        |> required "parent" int
        |> optional "kids" (list int) []


httpToMessage : Http.Error -> String
httpToMessage error =
    Debug.log "TODO" toString error


toTask : Http.Request a -> Task String a
toTask =
    Http.toTask >> Task.mapError httpToMessage


itemTask : Decoder a -> ItemId -> Task String a
itemTask decoder id =
    Http.get
        ("https://hacker-news.firebaseio.com/v0/item/" ++ toString id ++ ".json")
        decoder
        |> toTask


getItem : Decoder a -> ItemId -> (Result String a -> msg) -> Cmd msg
getItem decoder id msg =
    itemTask decoder id
        |> Task.attempt msg


getItems : Decoder a -> List ItemId -> (Result String (List a) -> msg) -> Cmd msg
getItems decoder ids msg =
    List.map (itemTask decoder) ids
        |> Task.sequence
        |> Task.attempt msg



-- STORIES


type Data a
    = Unfetched ItemId
    | Fetched a


type StoryItems
    = StoryItems (List (Data Story))


emptyStories : StoryItems
emptyStories =
    StoryItems []


initializeStoryItems : List ItemId -> StoryItems
initializeStoryItems ids =
    StoryItems (List.map Unfetched ids)


getTopStories : (Result String StoryItems -> msg) -> Cmd msg
getTopStories msg =
    Http.get "https://hacker-news.firebaseio.com/v0/topstories.json" (list int)
        |> toTask
        |> Task.map initializeStoryItems
        |> Task.andThen (viewMoreStoriesTask 10)
        |> Task.attempt msg


idFromItem : Data a -> Maybe ItemId
idFromItem item =
    case item of
        Unfetched id ->
            Just id

        Fetched _ ->
            Nothing


type alias Identifiable a =
    { a | id : ItemId }


setFetchedItem : Identifiable a -> Data (Identifiable a) -> Data (Identifiable a)
setFetchedItem item data =
    case data of
        Unfetched id ->
            if item.id == id then
                Fetched item
            else
                data

        Fetched _ ->
            data


replaceUnfetchedItems : List (Data (Identifiable a)) -> List (Identifiable a) -> List (Data (Identifiable a))
replaceUnfetchedItems initialItems identifiableItems =
    List.map
        (\data ->
            case data of
                Unfetched id ->
                    case
                        List.head
                            (List.filter
                                (\item -> item.id == id)
                                identifiableItems
                            )
                    of
                        Just item ->
                            Fetched item

                        Nothing ->
                            data

                Fetched item ->
                    data
        )
        initialItems


viewMoreStoriesTask : Int -> StoryItems -> Task String StoryItems
viewMoreStoriesTask count (StoryItems items) =
    items
        |> List.filterMap idFromItem
        |> List.take count
        |> List.map (itemTask storyItemDecoder)
        |> Task.sequence
        |> Task.map (replaceUnfetchedItems items)
        |> Task.map StoryItems


viewMoreStories : Int -> StoryItems -> (Result String StoryItems -> msg) -> Cmd msg
viewMoreStories count storyItems msg =
    viewMoreStoriesTask count storyItems
        |> Task.attempt msg


getStory : (Result String Story -> msg) -> ItemId -> Cmd msg
getStory msg id =
    getItem storyItemDecoder id msg


stories : StoryItems -> List Story
stories (StoryItems items) =
    List.filterMap
        (\data ->
            case data of
                Unfetched _ ->
                    Nothing

                Fetched item ->
                    Just item
        )
        items


hasMoreStories : StoryItems -> Bool
hasMoreStories (StoryItems items) =
    List.any
        (\data ->
            case data of
                Unfetched _ ->
                    True

                Fetched _ ->
                    False
        )
        items



-- COMMENTS


type alias ItemWithComments a =
    { a | kids : List ItemId }


getComments : ItemWithComments a -> (Result String (List CommentItem) -> msg) -> Cmd msg
getComments { kids } =
    getItems commentItemDecoder kids
