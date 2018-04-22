module RemoteData exposing (..)


type RemoteData a
    = Ready
    | Loading
    | Success a
    | Failure String
