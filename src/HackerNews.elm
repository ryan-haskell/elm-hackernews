module HackerNews
    exposing
        ( Comment
        , HackerNews
        , Story
        , bestStories
        , fetch
        , hasMore
        , items
        , newsStories
        , topStories
        , updateWithError
        , updateWithItem
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
import List.Extra
import Task exposing (Task)


type alias ItemId =
    Int


type alias UnixTime =
    Int


type alias Username =
    String


type alias Story =
    { deleted : Bool
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


storyDecoder : Decoder Story
storyDecoder =
    decode Story
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


type alias Comment =
    { deleted : Bool
    , by : Username
    , time : UnixTime
    , text : String
    , dead : Bool
    , parent : ItemId
    , kids : List ItemId
    }


commentDecoder : Decoder Comment
commentDecoder =
    decode Comment
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


getItem : Decoder a -> (Result String a -> msg) -> ItemId -> Cmd msg
getItem decoder msg id =
    itemTask decoder id
        |> Task.attempt msg


getItems : Decoder a -> List ItemId -> (Result String (List a) -> msg) -> Cmd msg
getItems decoder ids msg =
    List.map (itemTask decoder) ids
        |> Task.sequence
        |> Task.attempt msg



-- SOURCES


type Source
    = TopStories
    | NewsStories
    | BestStories


topStories : Source
topStories =
    TopStories


newsStories : Source
newsStories =
    NewsStories


bestStories : Source
bestStories =
    BestStories


sourceUrl : Source -> String
sourceUrl source =
    case source of
        TopStories ->
            "https://hacker-news.firebaseio.com/v0/topstories.json"

        NewsStories ->
            "https://hacker-news.firebaseio.com/v0/newsstories.json"

        BestStories ->
            "https://hacker-news.firebaseio.com/v0/beststories.json"


fetch : Source -> (Result String (List (HackerNews a)) -> msg) -> Cmd msg
fetch source msg =
    Http.get (sourceUrl source) (list int)
        |> toTask
        |> Task.map (List.map init)
        |> Task.attempt msg



-- HackerNews


type RemoteData a
    = Ready
    | Loading
    | Success a
    | Failure String


type HackerNews a
    = HackerNews ItemId (RemoteData a)



-- Setting state of HackerNews


init : ItemId -> HackerNews a
init id =
    HackerNews id Ready


load : HackerNews a -> HackerNews a
load (HackerNews id _) =
    HackerNews id Loading


success : a -> HackerNews a -> HackerNews a
success data (HackerNews id _) =
    HackerNews id (Success data)


failure : String -> HackerNews a -> HackerNews a
failure reason (HackerNews id _) =
    HackerNews id (Failure reason)



-- Getting information from HackerNews


id : HackerNews a -> ItemId
id (HackerNews id _) =
    id


item : HackerNews a -> Maybe a
item (HackerNews _ data) =
    case data of
        Ready ->
            Nothing

        Loading ->
            Nothing

        Success item ->
            Just item

        Failure _ ->
            Nothing


items : List (HackerNews a) -> List a
items =
    List.filterMap item


hasMore : List (HackerNews a) -> Bool
hasMore =
    List.any isReady


isReady : HackerNews a -> Bool
isReady (HackerNews _ status) =
    status == Ready



-- Fetching items


pageSize : Int
pageSize =
    10


listSplit : Int -> List a -> ( List a, List a )
listSplit count list =
    ( List.take count list
    , List.drop count list
    )


viewMore : Decoder a -> List (HackerNews a) -> (ItemId -> Result String a -> msg) -> ( List (HackerNews a), Cmd msg )
viewMore decoder list msg =
    let
        ( ready, notReady ) =
            List.partition isReady list

        ( toFetch, toKeep ) =
            listSplit pageSize ready

        cmds =
            toFetch
                |> List.map id
                |> List.map (\id -> getItem decoder (msg id) id)

        loadingFetchedThings =
            toFetch
                |> List.map load
    in
    List.concat
        [ notReady
        , loadingFetchedThings
        , toKeep
        ]
        ! cmds


viewMoreStories : List (HackerNews Story) -> (ItemId -> Result String Story -> msg) -> ( List (HackerNews Story), Cmd msg )
viewMoreStories =
    viewMore storyDecoder



-- Update in place


matchesId : ItemId -> HackerNews a -> Bool
matchesId id (HackerNews itemId _) =
    id == itemId


updateWithItem : ( ItemId, a ) -> List (HackerNews a) -> List (HackerNews a)
updateWithItem ( id, item ) =
    List.Extra.replaceIf
        (matchesId id)
        (HackerNews id (Success item))


updateWithError : ( ItemId, String ) -> List (HackerNews a) -> List (HackerNews a)
updateWithError ( id, reason ) =
    List.Extra.replaceIf
        (matchesId id)
        (HackerNews id (Failure reason))
