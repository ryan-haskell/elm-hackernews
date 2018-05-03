module Main exposing (..)

import Element exposing (Element, layout)
import Html exposing (Html)
import Navigation
import Pages.Home
import Pages.Story
import Routes
import Tuple
import Types exposing (Page(..))


main : Program Never Model Msg
main =
    Navigation.program
        RouteChange
        { init = Types.Context >> init
        , view = view >> layout []
        , update = update
        , subscriptions = always Sub.none
        }


type alias Model =
    { context : Types.Context
    , page : PageModel
    }


type PageModel
    = HomeModel Pages.Home.Model
    | StoryModel Pages.Story.Model
    | NotFoundModel


type Msg
    = RouteChange Navigation.Location
    | HomeMsg Pages.Home.Msg
    | StoryMsg Pages.Story.Msg


init : Types.Context -> ( Model, Cmd Msg )
init context =
    let
        ( pageModel, pageCmd ) =
            initPage context
    in
    Model context pageModel ! [ pageCmd ]


initPage : Types.Context -> ( PageModel, Cmd Msg )
initPage context =
    case Routes.page context.location of
        Home ->
            initialize
                Pages.Home.init
                HomeModel
                HomeMsg

        Story itemId ->
            initialize
                (Pages.Story.init itemId)
                StoryModel
                StoryMsg

        NotFound ->
            ( NotFoundModel, Cmd.none )


initialize :
    ( a, Cmd b )
    -> (a -> PageModel)
    -> (b -> Msg)
    -> ( PageModel, Cmd Msg )
initialize init toModel toMsg =
    init
        |> Tuple.mapFirst toModel
        |> Tuple.mapSecond (Cmd.map toMsg)


view : Model -> Element Msg
view { context, page } =
    case page of
        HomeModel model ->
            Element.map (HomeMsg model) (Pages.Home.view context model)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.page ) of
        ( RouteChange location, _ ) ->
            { model | context = Types.Context location } ! []

        ( HomeMsg msg, HomeModel page ) ->
            let
                ( pageModel, cmd ) =
                    Pages.Home.update msg page
            in
            ( { model | page = HomeModel pageModel }, cmd )

        ( HomeMsg msg, _ ) ->
            model ! []
