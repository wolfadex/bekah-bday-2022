port module Main exposing (main)

import Browser
import Element exposing (..)
import Json.Decode exposing (Decoder)
import Json.Encode exposing (Value)
import Random exposing (Seed)
import Update.Pipeline


main : Program Value Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { reel1 : List Symbol
    , reel2 : List Symbol
    , reel3 : List Symbol
    , reel4 : List Symbol
    , reel5 : List Symbol
    , money : Int
    , seed : Seed
    }


type Symbol
    = Ace
    | King
    | Queen
    | Jack
    | Ten
    | Nine


init : Value -> ( Model, Cmd Msg )
init flags =
    case Json.Decode.decodeValue decodeFlags flags of
        Ok ( initialSeed, savedModel ) ->
            ( { savedModel | seed = Random.initialSeed initialSeed }, Cmd.none )

        Err _ ->
            Debug.todo "TODO"


decodeFlags : Decoder ( Int, Model )
decodeFlags =
    Json.Decode.map2 Tuple.pair
        (Json.Decode.field "initialSeed" Json.Decode.int)
        (Json.Decode.field "savedModel" decodeModel)


decodeModel : Decoder Model
decodeModel =
    Json.Decode.map7 Model
        (Debug.todo "TODO")
        (Debug.todo "TODO")
        (Debug.todo "TODO")
        (Debug.todo "TODO")
        (Debug.todo "TODO")
        (Debug.todo "TODO")
        (Debug.todo "TODO")


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


type Msg
    = NoOp
    | Spin


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            Update.Pipeline.save model

        Spin ->
            Update.Pipeline.save model


port saveModel : Value -> Cmd msg


view : Model -> Browser.Document Msg
view model =
    { title = "Reels of Fun!"
    , body = [ layout [ width fill, height fill ] (viewModel model) ]
    }


viewModel : Model -> Element Msg
viewModel model =
    text "Reels of Fun!"
