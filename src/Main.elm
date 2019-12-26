module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Array exposing (Array)
import Browser exposing (Document)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder, field, int, list)
import Tuple exposing (..)



-- MAIN


main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { game : Array (Array SudokuCell)
    , difficulty : Difficulty
    , gameStatus : GameStatus
    }


type GameStatus
    = Loading
    | Loaded
    | Error


type Difficulty
    = Easy
    | Medium
    | Hard
    | Random


type alias SudokuCell =
    { y : Int
    , x : Int
    , value : Int
    , cellType : CellType
    }


type CellType
    = StaticCell
    | InputCell
    | EmptyCell


getInitialGame : Array (Array SudokuCell)
getInitialGame =
    createSudokuFromLists
        [ [ 0, 0, 8, 0, 0, 3, 0, 0, 2 ]
        , [ 0, 4, 0, 5, 6, 0, 1, 0, 0 ]
        , [ 0, 2, 6, 7, 0, 0, 0, 0, 4 ]
        , [ 0, 5, 0, 0, 3, 0, 0, 2, 0 ]
        , [ 0, 0, 0, 2, 8, 9, 0, 0, 0 ]
        , [ 0, 8, 0, 0, 5, 0, 0, 9, 0 ]
        , [ 2, 0, 0, 0, 0, 4, 8, 6, 0 ]
        , [ 0, 0, 3, 0, 9, 5, 0, 7, 0 ]
        , [ 8, 0, 0, 1, 0, 0, 3, 0, 0 ]
        ]


createSudokuFromLists : List (List Int) -> Array (Array SudokuCell)
createSudokuFromLists state =
    Array.fromList (List.indexedMap convertSudokuRow state)


convertSudokuRow : Int -> List Int -> Array SudokuCell
convertSudokuRow y row =
    let
        convertCell x val =
            case val of
                0 ->
                    { x = x, y = y, value = 0, cellType = EmptyCell }

                _ ->
                    { x = x, y = y, value = val, cellType = StaticCell }
    in
    Array.fromList (List.indexedMap convertCell row)


init : () -> ( Model, Cmd Msg )
init _ =
    ( { game = getInitialGame, difficulty = Easy, gameStatus = Loaded }, Cmd.none )



-- UPDATE


type Msg
    = MakeInput Int Int
    | LoadGame
    | LoadedBoard (Result Http.Error (List (List Int)))
    | DifficultyChanged Difficulty


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MakeInput y x ->
            ( convertToStaticCell y x model, Cmd.none )

        DifficultyChanged difficulty ->
            ( { model | difficulty = difficulty }, Cmd.none )

        LoadGame ->
            ( { model | gameStatus = Loading }, loadGame model.difficulty )

        LoadedBoard result ->
            case result of
                Ok newGame ->
                    let
                        game =
                            createSudokuFromLists newGame
                    in
                    ( { model | gameStatus = Loaded, game = game }, Cmd.none )

                Err _ ->
                    ( { model | gameStatus = Error }, Cmd.none )


loadGame : Difficulty -> Cmd Msg
loadGame difficulty =
    -- Currently using API from: https://github.com/berto/sugoku
    -- Eventually we'll auto-create our own Sudoku puzzles, but for now...
    Http.get
        { url = "https://sugoku.herokuapp.com/board?difficulty=" ++ String.toLower (difficultyToString difficulty)
        , expect = Http.expectJson LoadedBoard boardDecoder
        }


boardDecoder : Decoder (List (List Int))
boardDecoder =
    field "board" (list (list int))


convertGameToList : Array (Array SudokuCell) -> List (List SudokuCell)
convertGameToList game =
    Array.toList (Array.map Array.toList game)


convertToStaticCell : Int -> Int -> Model -> Model
convertToStaticCell y x model =
    let
        { game } =
            model

        selectedRow =
            Maybe.withDefault
                (Array.initialize 9 (\n -> { x = n, y = y, value = n, cellType = StaticCell }))
                (Array.get y game)

        updatedRow =
            Array.set x { cellType = StaticCell, value = 10, x = x, y = y } selectedRow

        updatedGame =
            Array.set y updatedRow game
    in
    { game = updatedGame, difficulty = model.difficulty, gameStatus = Loaded }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Document Msg
view model =
    { title = "SUDOKU!!!"
    , body = pageContents model
    }


difficultyFromString : String -> Difficulty
difficultyFromString difficulty =
    case difficulty of
        "Easy" ->
            Easy

        "Medium" ->
            Medium

        "Hard" ->
            Hard

        "Random" ->
            Random

        _ ->
            Easy


difficultyToString : Difficulty -> String
difficultyToString difficulty =
    case difficulty of
        Easy ->
            "Easy"

        Medium ->
            "Medium"

        Hard ->
            "Hard"

        Random ->
            "Random"


emitSelectedDifficulty : String -> Msg
emitSelectedDifficulty difficulty =
    DifficultyChanged (difficultyFromString difficulty)


difficultySelect : Model -> Html Msg
difficultySelect model =
    select [ onInput emitSelectedDifficulty ]
        [ option [ value "Easy", selected (model.difficulty == Easy) ] [ text "Easy" ]
        , option [ value "Medium", selected (model.difficulty == Medium) ] [ text "Medium" ]
        , option [ value "Hard", selected (model.difficulty == Hard) ] [ text "Hard" ]
        , option [ value "Random", selected (model.difficulty == Random) ] [ text "Random" ]
        ]


changeDifficultyBtn : Html Msg
changeDifficultyBtn =
    button [ type_ "button", onClick LoadGame ] [ text "New Game" ]


pageContents : Model -> List (Html Msg)
pageContents model =
    [ div [ class "container" ]
        [ div []
            [ h1 [] [ text "SUDOKU!!!!" ]
            , h3 []
                [ text "Difficulty: "
                , difficultySelect model
                , changeDifficultyBtn
                ]
            , displayTable model
            ]
        ]
    ]


displayTable : Model -> Html Msg
displayTable model =
    let
        { game } =
            model

        convertedGame =
            convertGameToList game
    in
    div [ class "board" ] (displayRows convertedGame)


displayRows : List (List SudokuCell) -> List (Html Msg)
displayRows rows =
    let
        displayRow rowIdx row =
            div [ class "flex" ] (displayColumns row rowIdx)
    in
    List.indexedMap displayRow rows


displayColumns : List SudokuCell -> Int -> List (Html Msg)
displayColumns columns rowIdx =
    let
        displayColumn x column =
            case column.cellType of
                StaticCell ->
                    div [ class "cell flex static" ] [ text (String.fromInt column.value) ]

                InputCell ->
                    div [ class "cell flex", onClick (MakeInput rowIdx x) ] [ text (String.fromInt column.value) ]

                EmptyCell ->
                    div [ class "cell flex", onClick (MakeInput rowIdx x) ] [ text "" ]

        cellContents =
            List.indexedMap displayColumn columns
    in
    [ div [ class "flex" ] cellContents ]
