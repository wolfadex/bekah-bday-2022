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
import Process
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
    , bet : Int
    , calculatedWin : Bool
    , seed : Seed
    , windowSize : { width : Float, height : Float }
    , spinning : Timeline Spinning
    , today : Posix
    , zone : Zone
    , wins : List Win
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
    , bet = 1
    , calculatedWin = False
    , seed = Random.initialSeed 0
    , windowSize = { width = 800, height = 600 }
    , spinning = Animator.init NotYetSpun
    , today = Time.millisToPosix 0
    , zone = Time.utc
    , wins = []
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
        , ( 80, Nine )
        , ( 1, HBDDash )
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
            (\money -> { defaultModel | money = money })
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
    | DecreaseBet
    | IncreaseBet
    | NoOp
    | ResetWinCalc
    | ResetGame


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    (case msg of
        GotZone zone ->
            ( { model | zone = zone }, Cmd.none )

        ResetGame ->
            ( { model | money = defaultModel.money }
            , Cmd.none
            )

        CurrentTime today ->
            ( { model | today = today }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )

        IncreaseBet ->
            ( { model | bet = min (model.bet + 1) model.money }, Cmd.none )

        DecreaseBet ->
            ( { model | bet = max (model.bet - 1) 1 }, Cmd.none )

        Spin ->
            if model.money > 0 then
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
                            , wins = []
                            , money = model.money - model.bet
                            , bet = min model.bet model.money
                            , reels =
                                addBdayToReels model.zone model.today reels

                            -- { reel1 = testReel
                            -- , reel2 = testReel
                            -- , reel3 = testReel
                            -- , reel4 = testReel
                            -- , reel5 = testReel
                            -- }
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
                            , wins = []
                            , money = model.money - model.bet
                            , bet = min model.bet model.money
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

                            -- { reel1 = testReel
                            -- , reel2 = testReel
                            -- , reel3 = testReel
                            -- , reel4 = testReel
                            -- , reel5 = testReel
                            -- }
                          }
                        , Process.sleep 15
                            |> Task.perform (\() -> ResetWinCalc)
                        )

            else
                ( model, Cmd.none )

        ResetWinCalc ->
            ( { model
                | calculatedWin = False
                , wins = []
              }
            , Cmd.none
            )

        Stop ->
            ( { model
                | spinning =
                    case Animator.current model.spinning of
                        Spinning ->
                            Animator.interrupt
                                [ Animator.event Animator.immediately Stopped
                                ]
                                model.spinning

                        _ ->
                            model.spinning
              }
            , Cmd.none
            )

        Tick newTime ->
            if Animator.arrived model.spinning == Stopped then
                let
                    nextModel : Model
                    nextModel =
                        Animator.update newTime reelAnimator model

                    wins : List Win
                    wins =
                        findWins nextModel.reels
                in
                ( { nextModel
                    | wins = wins
                    , money =
                        if model.calculatedWin then
                            model.money

                        else
                            model.money + (List.length wins * model.bet)
                    , calculatedWin = True
                  }
                , Cmd.none
                )

            else
                ( Animator.update newTime reelAnimator model, Cmd.none )

        WindowResized w h ->
            ( { model | windowSize = { width = toFloat w, height = toFloat h } }, Cmd.none )
    )
        |> (\( m, cmd ) -> ( m, Cmd.batch [ cmd, saveModel (encodeModel m) ] ))


type alias Win =
    { reel1 : Int
    , reel2 : Int
    , reel3 : Int
    , reel4 : Int
    , reel5 : Int
    }


findWins : Reels -> List Win
findWins reels =
    Maybe.map5
        (\column1 column2 column3 column4 column5 ->
            List.filter
                (\line ->
                    Maybe.map5
                        (\s1 s2 s3 s4 s5 ->
                            case List.filter (isWild >> not) [ s1, s2, s3, s4, s5 ] of
                                [] ->
                                    True

                                [ _ ] ->
                                    True

                                first :: rest ->
                                    List.all ((==) first) rest
                        )
                        (getSymbolInRow line.reel1 column1)
                        (getSymbolInRow line.reel2 column2)
                        (getSymbolInRow line.reel3 column3)
                        (getSymbolInRow line.reel4 column4)
                        (getSymbolInRow line.reel5 column5)
                        |> Maybe.withDefault False
                )
                winningLines
        )
        (getRows reels.reel1)
        (getRows reels.reel2)
        (getRows reels.reel3)
        (getRows reels.reel4)
        (getRows reels.reel5)
        |> Maybe.withDefault []


isWild : Symbol -> Bool
isWild symbol =
    List.any ((==) symbol)
        [ HBDH
        , HBDP
        , HBDY
        , HBDB
        , HBDD
        , HBDDash
        , HBDE
        , Blank
        ]


winningLines : List { reel1 : Int, reel2 : Int, reel3 : Int, reel4 : Int, reel5 : Int }
winningLines =
    [ { reel1 = 1, reel2 = 1, reel3 = 1, reel4 = 1, reel5 = 1 }
    , { reel1 = 2, reel2 = 2, reel3 = 2, reel4 = 2, reel5 = 2 }
    , { reel1 = 3, reel2 = 3, reel3 = 3, reel4 = 3, reel5 = 3 }
    , { reel1 = 4, reel2 = 4, reel3 = 4, reel4 = 4, reel5 = 4 }
    ]


getSymbolInRow : Int -> Column -> Maybe Symbol
getSymbolInRow row column =
    case row of
        1 ->
            Just column.row1

        2 ->
            Just column.row2

        3 ->
            Just column.row3

        4 ->
            Just column.row4

        _ ->
            Nothing


type alias Column =
    { row1 : Symbol
    , row2 : Symbol
    , row3 : Symbol
    , row4 : Symbol
    }


getRows : List Symbol -> Maybe Column
getRows column =
    case column of
        row1 :: row2 :: row3 :: row4 :: _ ->
            Just { row1 = row1, row2 = row2, row3 = row3, row4 = row4 }

        _ ->
            Nothing


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

        viewReelHelper : List Symbol -> Html msg
        viewReelHelper =
            viewReel model.spinning tileSize
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
                , Css.border3 (Css.px 8) Css.solid (Css.rgb 128 255 255)
                ]
            ]
            [ viewReelHelper model.reels.reel1
            , viewReelHelper model.reels.reel2
            , viewReelHelper model.reels.reel3
            , viewReelHelper model.reels.reel4
            , viewReelHelper model.reels.reel5
            ]

        -- Win lines
        , Html.Styled.div
            [ Html.Styled.Attributes.css
                [ Css.position Css.absolute
                , Css.top (Css.rem 5)
                ]
            ]
            [ Html.Styled.div
                [ Html.Styled.Attributes.css
                    [ Css.displayFlex
                    , Css.marginTop (Css.rem 1)
                    , Css.marginBottom (Css.rem 1)
                    , Css.border3 (Css.px 8) Css.solid (Css.rgba 0 0 0 0)
                    ]
                ]
                (List.map (viewWinLine tileSize) model.wins)
            ]

        -- Spin button
        , Html.Styled.div
            [ Html.Styled.Attributes.css
                [ Css.displayFlex
                , Css.alignItems Css.center
                ]
            ]
            [ Html.Styled.span
                [ Html.Styled.Attributes.css
                    [ Css.fontSize (Css.rem 3)
                    , Css.marginLeft (Css.rem 1)
                    , Css.textTransform Css.uppercase
                    , Css.cursor Css.pointer
                    ]
                ]
                [ Html.Styled.text ("Funds: $" ++ String.fromInt model.money) ]
            , Html.Styled.button
                [ Html.Styled.Attributes.css
                    [ Css.fontSize (Css.rem 3)
                    , Css.marginLeft (Css.rem 5)
                    , Css.textTransform Css.uppercase
                    , Css.cursor Css.pointer
                    ]
                , Html.Styled.Events.onClick <|
                    case Animator.current model.spinning of
                        Spinning ->
                            NoOp

                        Stopped ->
                            DecreaseBet

                        NotYetSpun ->
                            DecreaseBet
                ]
                [ Html.Styled.text "-" ]
            , Html.Styled.span
                [ Html.Styled.Attributes.css
                    [ Css.fontSize (Css.rem 3)
                    , Css.marginLeft (Css.rem 1)
                    , Css.textTransform Css.uppercase
                    , Css.cursor Css.pointer
                    ]
                ]
                [ Html.Styled.text ("$" ++ String.fromInt model.bet) ]
            , Html.Styled.button
                [ Html.Styled.Attributes.css
                    [ Css.fontSize (Css.rem 3)
                    , Css.marginLeft (Css.rem 1)
                    ]
                , Html.Styled.Events.onClick <|
                    case Animator.current model.spinning of
                        Spinning ->
                            NoOp

                        Stopped ->
                            IncreaseBet

                        NotYetSpun ->
                            IncreaseBet
                ]
                [ Html.Styled.text "+" ]
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
                    , Css.marginLeft (Css.rem 3)
                    ]
                ]
                [ Html.Styled.text <|
                    case Animator.current model.spinning of
                        NotYetSpun ->
                            "Spin"

                        Stopped ->
                            "Spin"

                        Spinning ->
                            "Stop"
                ]
            , Html.Styled.button
                [ Html.Styled.Events.onClick <|
                    case Animator.current model.spinning of
                        Spinning ->
                            NoOp

                        Stopped ->
                            ResetGame

                        NotYetSpun ->
                            ResetGame
                , Html.Styled.Attributes.css
                    [ Css.fontSize (Css.rem 3)
                    , Css.padding2 (Css.rem 0.5) (Css.rem 1)
                    , Css.textTransform Css.uppercase
                    , Css.cursor Css.pointer
                    , Css.marginLeft (Css.rem 3)
                    ]
                ]
                [ Html.Styled.text <|
                    case Animator.current model.spinning of
                        NotYetSpun ->
                            "Reset Game"

                        Stopped ->
                            "Reset Game"

                        Spinning ->
                            "Spinning..."
                ]
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


viewWinLine : Float -> Win -> Html msg
viewWinLine tileSize win =
    let
        viewWinTile offset =
            Html.Styled.div
                [ Html.Styled.Attributes.css
                    [ Css.width (Css.px tileSize)
                    , Css.height (Css.px tileSize)
                    , Css.marginTop (Css.px (tileSize * (toFloat offset - 1)))
                    , Css.border3 (Css.px 3) Css.dashed (Css.rgb 255 100 255)
                    ]
                ]
                []
    in
    Html.Styled.div
        [ Html.Styled.Attributes.css
            [ Css.width (Css.px (tileSize * 5))
            , Css.height (Css.px tileSize)
            , Css.displayFlex
            , Css.position Css.absolute
            , Css.transform (Css.translateX (Css.pct -50))
            ]
        ]
        [ viewWinTile win.reel1
        , viewWinTile win.reel2
        , viewWinTile win.reel3
        , viewWinTile win.reel4
        , viewWinTile win.reel5
        ]


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


testReel : List Symbol
testReel =
    [ Ace
    , King
    , Queen
    , Jack
    , Ten
    , Nine
    , Ace
    , King
    , Queen
    , Jack
    , Ace
    , King
    , Queen
    , Jack
    , Ten
    , Nine
    , Ace
    , King
    , Queen
    , Jack
    , Ace
    , King
    , Queen
    , Jack
    , Ten
    , Nine
    , Ace
    , King
    , Queen
    , Jack
    , Ace
    , King
    , Queen
    , Jack
    , Ten
    , Nine
    , Ace
    , King
    , Queen
    , Jack
    , Ace
    , King
    , Queen
    , Jack
    , Ten
    , Nine
    , Ace
    , King
    , Queen
    , Jack
    , Ace
    , King
    , Queen
    , Jack
    , Ten
    , Nine
    , Ace
    , King
    , Queen
    , Jack
    , Ace
    , King
    , Queen
    , Jack
    , Ten
    , Nine
    , Ace
    , King
    , Queen
    , Jack
    , Ace
    , King
    , Queen
    , Jack
    , Ten
    , Nine
    , Ace
    , King
    , Queen
    , Jack
    , Ace
    , King
    , Queen
    , Jack
    , Ten
    , Nine
    , Ace
    , King
    , Queen
    , Jack
    , Ace
    , King
    , Queen
    , Jack
    , Ten
    , Nine
    , Ace
    , King
    , Queen
    , Jack
    ]
