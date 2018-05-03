module Pages.Story exposing (Model, Msg, init, update, view)

import Element exposing (..)
import HackerNews exposing (Comment, Story)
import Types


type alias Model =
    { id : Int
    , story : Maybe Story
    , comments : List Comment
    }


type Msg
    = FetchComments


init : Int -> ( Model, Cmd Msg )
init id =
    Model id Nothing [] ! []


update : Types.Context -> Msg -> Model -> Model
update context msg model =
    case msg of
        FetchComments ->
            model


view : Types.Context -> Model -> Element Msg
view context model =
    el [] (text <| toString model.id)
