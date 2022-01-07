port module Main exposing (main)

import Browser
import Browser.Events
import Element exposing (..)
import Element.Font as Font
import Html exposing (Html)
import Html.Attributes
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
    { reels : Reels
    , money : Int
    , seed : Seed
    , windowSize : { width : Int, height : Int }
    }


type Symbol
    = Ace
    | King
    | Queen
    | Jack
    | Ten
    | Nine
    | HBDH
    | HBDP
    | HBDY
    | HBDB
    | HBDD
    | HBDDash
    | HBDE
    | Blank


defaultModel : Model
defaultModel =
    { reels =
        { reel1 = []
        , reel2 = []
        , reel3 = []
        , reel4 = []
        , reel5 = []
        }
    , money = 500
    , seed = Random.initialSeed 0
    , windowSize = { width = 800, height = 600 }
    }


type alias Reels =
    { reel1 : List Symbol
    , reel2 : List Symbol
    , reel3 : List Symbol
    , reel4 : List Symbol
    , reel5 : List Symbol
    }


bdayReels : Reels
bdayReels =
    { reel1 = [ HBDH, HBDB, HBDB, Blank ]
    , reel2 = [ Ace, HBDDash, HBDE, Blank ]
    , reel3 = [ HBDP, HBDD, King, Blank ]
    , reel4 = [ HBDP, Ace, Ace, Blank ]
    , reel5 = [ HBDY, HBDY, HBDH, Blank ]
    }


init : Value -> ( Model, Cmd Msg )
init flags =
    case Json.Decode.decodeValue decodeFlags flags |> Debug.log "flags" of
        Ok ( initialSeed, model, windowSize ) ->
            let
                ( reels, seed ) =
                    Random.step reelsGenerator (Random.initialSeed initialSeed)
            in
            ( { model
                | seed = seed
                , windowSize = windowSize
                , reels = reels
              }
            , Cmd.none
            )

        Err _ ->
            ( defaultModel, Cmd.none )


reelsGenerator : Random.Generator Reels
reelsGenerator =
    Random.map5 Reels
        reelGenerator
        reelGenerator
        reelGenerator
        reelGenerator
        reelGenerator


reelGenerator : Random.Generator (List Symbol)
reelGenerator =
    Random.list 10 symbolGenerator


symbolGenerator : Random.Generator Symbol
symbolGenerator =
    Random.weighted ( 1, Ace )
        [ ( 3, King )
        , ( 7, Queen )
        , ( 18, Jack )
        , ( 25, Ten )
        , ( 40, Nine )
        ]


decodeFlags : Decoder ( Int, Model, { width : Int, height : Int } )
decodeFlags =
    Json.Decode.map3 (\seed model windowSize -> ( seed, model, windowSize ))
        (Json.Decode.field "initialSeed" Json.Decode.int)
        (Json.Decode.field "savedModel" decodeModel)
        (Json.Decode.field "windowSize" decodeWindowSize)


decodeWindowSize : Decoder { width : Int, height : Int }
decodeWindowSize =
    Json.Decode.map2 (\w h -> { width = w, height = h })
        (Json.Decode.field "width" Json.Decode.int)
        (Json.Decode.field "height" Json.Decode.int)


decodeModel : Decoder Model
decodeModel =
    Json.Decode.oneOf
        [ Json.Decode.map
            (\money ->
                { defaultModel | money = money }
            )
            (Json.Decode.field "money" Json.Decode.int)
        , Json.Decode.succeed defaultModel
        ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onResize WindowResized


type Msg
    = Spin
    | WindowResized Int Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    (case msg of
        Spin ->
            Update.Pipeline.save model

        WindowResized w h ->
            Update.Pipeline.save { model | windowSize = { width = w, height = h } }
    )
        |> (\( m, cmd ) -> ( m, Cmd.batch [ cmd, saveModel (encodeModel m) ] ))


encodeModel : Model -> Value
encodeModel model =
    Json.Encode.object
        [ ( "money", Json.Encode.int model.money )
        ]


port saveModel : Value -> Cmd msg


view : Model -> Browser.Document Msg
view model =
    { title = "Reels of Fun!"
    , body = [ layout [ width fill, height fill ] (viewModel model) ]
    }


viewModel : Model -> Element Msg
viewModel model =
    let
        tileSize : Int
        tileSize =
            min (model.windowSize.width // 6) (model.windowSize.height // 5)
    in
    column
        [ centerX ]
        [ el [ Font.size 64, centerX ] (text "Reels of Fun!")
        , Html.div
            [ Html.Attributes.style "display" "grid"
            , Html.Attributes.style "grid"
                ("repeat(1, "
                    ++ String.fromInt (tileSize + 1)
                    ++ "px) / repeat(5, "
                    ++ String.fromInt (tileSize + 1)
                    ++ "px)"
                )
            ]
            [ viewReel tileSize model.reels.reel1
            , viewReel tileSize model.reels.reel2
            , viewReel tileSize model.reels.reel3
            , viewReel tileSize model.reels.reel4
            , viewReel tileSize model.reels.reel5
            ]
            |> html
            |> el [ centerX ]
        ]


viewReel : Int -> List Symbol -> Html msg
viewReel tileSize symbols =
    Html.div
        [ Html.Attributes.style "width" (String.fromInt tileSize ++ "px")
        , Html.Attributes.style "height" (String.fromInt (tileSize * 4) ++ "px")
        , Html.Attributes.style "overflow" "hidden"
        ]
        (List.map (viewSymbol tileSize) symbols)


viewSymbol : Int -> Symbol -> Html msg
viewSymbol tileSize symbol =
    Html.div
        [ Html.Attributes.style "width" (String.fromInt tileSize ++ "px")
        , Html.Attributes.style "height" (String.fromInt tileSize ++ "px")
        , Html.Attributes.style "background" (symbolToSprite tileSize symbol)
        ]
        []


symbolToSprite : Int -> Symbol -> String
symbolToSprite tileSize symbol =
    case symbol of
        Ace ->
            symbolToSpriteHelper 0 0

        King ->
            symbolToSpriteHelper tileSize 0

        Queen ->
            symbolToSpriteHelper (tileSize * 2) 0

        Jack ->
            symbolToSpriteHelper (tileSize * 3) 0

        Ten ->
            symbolToSpriteHelper (tileSize * 4) 0

        Nine ->
            symbolToSpriteHelper 0 tileSize

        HBDH ->
            symbolToSpriteHelper (tileSize * 4) (tileSize * 4)

        HBDP ->
            symbolToSpriteHelper (tileSize * 3) (tileSize * 4)

        HBDY ->
            symbolToSpriteHelper (tileSize * 2) (tileSize * 4)

        HBDB ->
            symbolToSpriteHelper tileSize (tileSize * 4)

        HBDD ->
            symbolToSpriteHelper 0 (tileSize * 4)

        HBDDash ->
            symbolToSpriteHelper (tileSize * 4) (tileSize * 3)

        HBDE ->
            symbolToSpriteHelper (tileSize * 3) (tileSize * 3)

        Blank ->
            symbolToSpriteHelper (tileSize * 2) (tileSize * 3)


symbolToSpriteHelper : Int -> Int -> String
symbolToSpriteHelper x y =
    "transparent url(Symbols.png) no-repeat -" ++ String.fromInt x ++ "px -" ++ String.fromInt y ++ "px / 500% scroll"
