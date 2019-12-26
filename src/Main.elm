module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Array exposing (Array)
import Browser exposing (Document)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder, field, int, list)



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
    , value : String
    , cellType : CellType
    }


type CellType
    = StaticCell
    | RegularCell


createSudokuFromLists : List (List String) -> Array (Array SudokuCell)
createSudokuFromLists state =
    Array.fromList (List.indexedMap convertSudokuRowFromList state)


convertSudokuRowFromList : Int -> List String -> Array SudokuCell
convertSudokuRowFromList y row =
    Array.fromList (List.indexedMap (convertCell y) row)


convertCell : Int -> Int -> String -> SudokuCell
convertCell y x val =
    case val of
        "0" ->
            { x = x, y = y, value = "0", cellType = RegularCell }

        _ ->
            { x = x, y = y, value = val, cellType = StaticCell }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { game = Array.fromList [ Array.fromList [] ], difficulty = Easy, gameStatus = Loading }, loadGame Easy )



-- UPDATE


type Msg
    = LoadGame
    | LoadedBoard (Result Http.Error (List (List Int)))
    | DifficultyChanged Difficulty
    | SetCellValue CellChangeEvent
    | NoOp


type alias CellChangeEvent =
    { x : Int
    , y : Int
    , value : String
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        DifficultyChanged difficulty ->
            ( { model | difficulty = difficulty }, Cmd.none )

        LoadGame ->
            ( { model | gameStatus = Loading }, loadGame model.difficulty )

        SetCellValue changeEvent ->
            ( { model | game = setFromCoordinates changeEvent model.game }, Cmd.none )

        LoadedBoard result ->
            case result of
                Ok newGame ->
                    let
                        parsedResponse =
                            List.map (\column -> List.map (\cell -> String.fromInt cell) column) newGame

                        game =
                            createSudokuFromLists parsedResponse
                    in
                    ( { model | gameStatus = Loaded, game = game }, Cmd.none )

                Err _ ->
                    ( { model | gameStatus = Error }, Cmd.none )



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


loadGame : Difficulty -> Cmd Msg
loadGame difficulty =
    -- Currently using API from: https://github.com/berto/sugoku
    -- Eventually we'll auto-create our own Sudoku puzzles, but for now...
    Http.get
        -- { url = "" ++ String.toLower (difficultyToString difficulty)
        { url = "https://sugoku.herokuapp.com/board?difficulty=" ++ String.toLower (difficultyToString difficulty)
        , expect = Http.expectJson LoadedBoard boardDecoder
        }


boardDecoder : Decoder (List (List Int))
boardDecoder =
    field "board" (list (list int))


setFromCoordinates : CellChangeEvent -> Array (Array SudokuCell) -> Array (Array SudokuCell)
setFromCoordinates { y, x, value } listOfLists =
    let
        cellValue =
            { y = y, x = x, value = value, cellType = RegularCell }

        setInnerVal arr =
            Array.set x cellValue arr

        innerArr =
            case Array.get y listOfLists of
                Nothing ->
                    Array.fromList []

                Just val ->
                    val
    in
    Array.set y (setInnerVal innerArr) listOfLists



-- Eventually may be useful?
-- getFromCoordinates : Int -> Int -> Array (Array SudokuCell) -> SudokuCell
-- getFromCoordinates y x game =
--     let
--         defaultCellValue =
--             { y = y
--             , x = x
--             , value = ""
--             , cellType = RegularCell
--             }
--         outerVal =
--             case Array.get y game of
--                 Nothing ->
--                     Array.fromList [ defaultCellValue ]
--                 Just row ->
--                     row
--         innerVal =
--             case Array.get x outerVal of
--                 Nothing ->
--                     defaultCellValue
--                 Just cell ->
--                     cell
--     in
--     innerVal


convertGameToList : Array (Array t) -> List (List t)
convertGameToList game =
    Array.toList (Array.map Array.toList game)


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


changeDifficultyBtn : GameStatus -> Html Msg
changeDifficultyBtn status =
    button [ type_ "button", onClick LoadGame, disabled (status == Loading) ] [ text "New Game" ]


pageContents : Model -> List (Html Msg)
pageContents model =
    [ div [ class "container" ]
        [ div []
            [ h1 [] [ text "SUDOKU!!!!" ]
            , h3 []
                [ text "Difficulty: "
                , difficultySelect model
                , changeDifficultyBtn model.gameStatus
                ]
            , getBoardElems model
            ]
        ]
    ]


getBoardElems : Model -> Html Msg
getBoardElems model =
    case model.gameStatus of
        Loading ->
            loadingMessage

        Loaded ->
            displayGameBoard model

        Error ->
            div []
                [ h6 [] [ text "Error loading game, try again maybe?" ]
                , loadingMessage
                ]


displayGameBoard : Model -> Html Msg
displayGameBoard model =
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
        displayRow row =
            div [ class "flex" ] (displayColumns row)
    in
    List.map displayRow rows


displayColumns : List SudokuCell -> List (Html Msg)
displayColumns columns =
    let
        displayColumn column =
            displayCell column

        cellContents =
            List.map displayColumn columns
    in
    [ div [ class "flex" ] cellContents ]


displayCell : SudokuCell -> Html Msg
displayCell cell =
    let
        isLeft =
            modBy 3 cell.x == 0

        isRight =
            modBy 3 cell.x == 2

        isTop =
            modBy 3 cell.y == 0

        isBottom =
            modBy 3 cell.y == 2

        divContents =
            [ input
                [ class "cell-input"
                , type_ "text"
                , maxlength 1
                , size 2
                , readonly (cell.cellType == StaticCell)
                , value
                    (if cell.value == "0" then
                        ""

                     else
                        cell.value
                    )
                , onInput (\val -> SetCellValue { y = cell.y, x = cell.x, value = val })
                ]
                []
            ]
    in
    div
        [ classList
            [ ( "cell", True )
            , ( "flex", True )
            , ( "static", cell.cellType == StaticCell )
            , ( "cell-left", isLeft )
            , ( "cell-right", isRight )
            , ( "cell-top", isTop )
            , ( "cell-bottom", isBottom )
            ]
        ]
        divContents


loadingMessage : Html Msg
loadingMessage =
    let
        rowsCreator i =
            if i /= 4 then
                div [ class "flex" ] (List.repeat 9 (div [ class "cell flex" ] [ text "" ]))

            else
                div [ class "flex" ]
                    [ div [ class "cell flex" ] [ text "" ]
                    , div [ class "cell flex static" ] [ text "L" ]
                    , div [ class "cell flex static" ] [ text "O" ]
                    , div [ class "cell flex static" ] [ text "A" ]
                    , div [ class "cell flex static" ] [ text "D" ]
                    , div [ class "cell flex static" ] [ text "I" ]
                    , div [ class "cell flex static" ] [ text "N" ]
                    , div [ class "cell flex static" ] [ text "G" ]
                    , div [ class "cell flex" ] [ text "" ]
                    ]

        rows =
            List.range 0 8
                |> List.map (\i -> rowsCreator i)
    in
    div [ class "board" ] rows
