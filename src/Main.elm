port module Main exposing (main)

import Animator exposing (Timeline)
import Animator.Inline
import Browser
import Browser.Events
import Css
import Html.Styled exposing (Html)
import Html.Styled.Attributes
import Html.Styled.Events
import Json.Decode exposing (Decoder)
import Json.Encode exposing (Value)
import Random exposing (Seed)
import Task
import Time exposing (Month(..), Posix, Zone)


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
    , windowSize : { width : Float, height : Float }
    , spinning : Timeline Spinning
    , today : Posix
    , zone : Zone
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


type Spinning
    = NotYetSpun
    | Spinning
    | Stopped


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
    , spinning = Animator.init NotYetSpun
    , today = Time.millisToPosix 0
    , zone = Time.utc
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



---- INIT ----


init : Value -> ( Model, Cmd Msg )
init flags =
    case Json.Decode.decodeValue decodeFlags flags of
        Ok ( initialSeed, model, windowSize ) ->
            let
                ( reels, seed ) =
                    Random.step reelsGenerator (Random.initialSeed initialSeed)
            in
            ( { model
                | seed = seed
                , windowSize = windowSize
                , reels = reels
                , today = Time.millisToPosix initialSeed
              }
            , Task.perform GotZone Time.here
            )

        Err _ ->
            ( defaultModel, Task.perform GotZone Time.here )


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
    Random.list 100 symbolGenerator


symbolGenerator : Random.Generator Symbol
symbolGenerator =
    Random.weighted ( 1, Ace )
        [ ( 3, King )
        , ( 7, Queen )
        , ( 18, Jack )
        , ( 25, Ten )
        , ( 40, Nine )
        ]


decodeFlags : Decoder ( Int, Model, { width : Float, height : Float } )
decodeFlags =
    Json.Decode.map3 (\seed model windowSize -> ( seed, model, windowSize ))
        (Json.Decode.field "initialSeed" Json.Decode.int)
        (Json.Decode.field "savedModel" decodeModel)
        (Json.Decode.field "windowSize" decodeWindowSize)


decodeWindowSize : Decoder { width : Float, height : Float }
decodeWindowSize =
    Json.Decode.map2 (\w h -> { width = toFloat w, height = toFloat h })
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



---- UPDATE ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onResize WindowResized
        , Animator.toSubscription Tick model reelAnimator
        , Time.every (30 * 60 * 1000) CurrentTime
        ]


reelAnimator : Animator.Animator Model
reelAnimator =
    Animator.watching .spinning
        (\spinning model -> { model | spinning = spinning })
        Animator.animator


type Msg
    = Spin
    | Stop
    | WindowResized Int Int
    | Tick Time.Posix
    | GotZone Zone
    | CurrentTime Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    (case msg of
        GotZone zone ->
            ( { model | zone = zone }, Cmd.none )

        CurrentTime today ->
            ( { model | today = today }, Cmd.none )

        Spin ->
            case Animator.current model.spinning of
                Spinning ->
                    ( model, Cmd.none )

                NotYetSpun ->
                    let
                        ( reels, seed ) =
                            Random.step reelsGenerator model.seed
                    in
                    ( { model
                        | spinning = respin model.spinning
                        , seed = seed
                        , reels = addBdayToReels model.zone model.today reels
                      }
                    , Cmd.none
                    )

                Stopped ->
                    let
                        ( reels, seed ) =
                            Random.step reelsGenerator model.seed
                    in
                    ( { model
                        | spinning = respin model.spinning
                        , seed = seed
                        , reels =
                            addBdayToReels
                                model.zone
                                model.today
                                { reel1 = reels.reel1 ++ List.take 5 model.reels.reel1
                                , reel2 = reels.reel2 ++ List.take 5 model.reels.reel2
                                , reel3 = reels.reel3 ++ List.take 5 model.reels.reel3
                                , reel4 = reels.reel4 ++ List.take 5 model.reels.reel4
                                , reel5 = reels.reel5 ++ List.take 5 model.reels.reel5
                                }
                      }
                    , Cmd.none
                    )

        Stop ->
            ( { model
                | spinning =
                    case Animator.current model.spinning of
                        Spinning ->
                            Animator.interrupt [ Animator.event Animator.immediately Stopped ] model.spinning

                        _ ->
                            model.spinning
              }
            , Cmd.none
            )

        Tick newTime ->
            if Animator.arrivedAt Stopped newTime model.spinning then
                Debug.todo "arrived"

            else
                ( Animator.update newTime reelAnimator model, Cmd.none )

        WindowResized w h ->
            ( { model | windowSize = { width = toFloat w, height = toFloat h } }, Cmd.none )
    )
        |> (\( m, cmd ) -> ( m, Cmd.batch [ cmd, saveModel (encodeModel m) ] ))


addBdayToReels : Zone -> Posix -> Reels -> Reels
addBdayToReels zone today reels =
    let
        isBday : Bool
        isBday =
            (Time.toMonth zone today == Feb) && (Time.toDay zone today == 2)
    in
    if isBday then
        { reel1 = bdayReels.reel1 ++ reels.reel1
        , reel2 = bdayReels.reel2 ++ reels.reel2
        , reel3 = bdayReels.reel3 ++ reels.reel3
        , reel4 = bdayReels.reel4 ++ reels.reel4
        , reel5 = bdayReels.reel5 ++ reels.reel5
        }

    else
        reels


respin : Timeline Spinning -> Timeline Spinning
respin =
    Animator.queue
        [ Animator.event Animator.immediately NotYetSpun
        , Animator.event (Animator.seconds 3) Spinning
        , Animator.event Animator.immediately Stopped
        ]


encodeModel : Model -> Value
encodeModel model =
    Json.Encode.object
        [ ( "money", Json.Encode.int model.money )
        ]


port saveModel : Value -> Cmd msg



---- VIEW ----


view : Model -> Browser.Document Msg
view model =
    { title = "Reels of Fun!"
    , body = [ Html.Styled.toUnstyled (viewModel model) ]
    }


viewModel : Model -> Html Msg
viewModel model =
    let
        tileSize : Float
        tileSize =
            min (model.windowSize.width / 6) (model.windowSize.height / 6)
    in
    Html.Styled.div
        [ Html.Styled.Attributes.css
            [ Css.displayFlex
            , Css.flexDirection Css.column
            , Css.alignItems Css.center
            , Css.fontFamily Css.sansSerif
            , Css.padding (Css.rem 1)
            ]
        ]
        [ Html.Styled.span
            [ Html.Styled.Attributes.css
                [ Css.fontSize (Css.rem 4) ]
            ]
            [ Html.Styled.text "Reels of Fun!" ]
        , Html.Styled.div
            [ Html.Styled.Attributes.css
                [ Css.displayFlex
                , Css.marginTop (Css.rem 1)
                , Css.marginBottom (Css.rem 1)
                , Css.border3 (Css.px 8) Css.solid (Css.rgb 128 256 256)
                ]
            ]
            [ viewReel model.spinning tileSize model.reels.reel1
            , viewReel model.spinning tileSize model.reels.reel2
            , viewReel model.spinning tileSize model.reels.reel3
            , viewReel model.spinning tileSize model.reels.reel4
            , viewReel model.spinning tileSize model.reels.reel5
            ]
        , Html.Styled.button
            [ Html.Styled.Events.onClick <|
                case Animator.current model.spinning of
                    Spinning ->
                        Stop

                    Stopped ->
                        Spin

                    NotYetSpun ->
                        Spin
            , Html.Styled.Attributes.css
                [ Css.fontSize (Css.rem 3)
                , Css.padding2 (Css.rem 0.5) (Css.rem 1)
                , Css.textTransform Css.uppercase
                , Css.cursor Css.pointer
                ]
            ]
            [ Html.Styled.text <|
                case Animator.current model.spinning |> Debug.log "spinning state" of
                    NotYetSpun ->
                        "Spin"

                    Stopped ->
                        "Spin"

                    Spinning ->
                        "Stop"
            ]
        ]


viewReel : Timeline Spinning -> Float -> List Symbol -> Html msg
viewReel spinning tileSize symbols =
    let
        tileCount : Float
        tileCount =
            toFloat (List.length symbols)
    in
    Html.Styled.div
        [ Html.Styled.Attributes.css
            [ Css.width (Css.px tileSize)
            , Css.height (Css.px (tileSize * 4))
            , Css.overflow Css.hidden
            ]
        ]
        (List.map (viewSymbol spinning tileSize tileCount) symbols)


viewSymbol : Timeline Spinning -> Float -> Float -> Symbol -> Html msg
viewSymbol spinning tileSize tileCount symbol =
    Html.Styled.div
        [ Html.Styled.Attributes.style "background" (symbolToSprite tileSize symbol)
        , Html.Styled.Attributes.css
            [ Css.width (Css.px tileSize)
            , Css.height (Css.px tileSize)
            ]
        , Html.Styled.Attributes.fromUnstyled <|
            Animator.Inline.transform
                { position =
                    { x = 0
                    , y =
                        Animator.move spinning <|
                            \state ->
                                case state of
                                    Spinning ->
                                        Animator.at 0

                                    Stopped ->
                                        Animator.at 0

                                    NotYetSpun ->
                                        Animator.at (tileSize * -(tileCount - 4))
                    }
                , rotate = 0
                , scale = 1
                }
        ]
        []


symbolToSprite : Float -> Symbol -> String
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


symbolToSpriteHelper : Float -> Float -> String
symbolToSpriteHelper x y =
    "transparent url(Symbols.png) no-repeat -" ++ String.fromFloat x ++ "px -" ++ String.fromFloat y ++ "px / 500% scroll"
