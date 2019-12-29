module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Array exposing (Array)
import Browser exposing (Document)
import Html exposing (..)
import Html.Attributes exposing (class, classList, disabled, maxlength, readonly, selected, size, type_, value)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder, field, int, list)
import Set exposing (Set)



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
    | InvalidCell


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
    ( { game = Array.fromList [ Array.empty ], difficulty = Easy, gameStatus = Loading }, loadGame Easy )



-- UPDATE


type Msg
    = LoadGame
    | LoadedBoard (Result Http.Error (List (List Int)))
    | DifficultyChanged Difficulty
    | SetCellValue SudokuCell
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        DifficultyChanged difficulty ->
            ( { model | difficulty = difficulty }, Cmd.none )

        LoadGame ->
            ( { model | gameStatus = Loading }, loadGame model.difficulty )

        SetCellValue newCell ->
            let
                updatedGame =
                    setFromCoordinates newCell model.game
            in
            ( { model | game = updatedGame }, Cmd.none )

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
        { url = "https://sugoku.herokuapp.com/board?difficulty=" ++ String.toLower (difficultyToString difficulty)
        , expect = Http.expectJson LoadedBoard boardDecoder
        }


boardDecoder : Decoder (List (List Int))
boardDecoder =
    field "board" (list (list int))


setFromCoordinates : SudokuCell -> Array (Array SudokuCell) -> Array (Array SudokuCell)
setFromCoordinates ({ y, x, value } as change) game =
    let
        cellValue =
            { y = y, x = x, value = value, cellType = getCellType y x value game }

        setInnerVal arr =
            Array.set x cellValue arr

        innerArr =
            case Array.get y game of
                Nothing ->
                    Array.empty

                Just val ->
                    val
    in
    Array.set y (setInnerVal innerArr) game


getCellType : Int -> Int -> String -> Array (Array SudokuCell) -> CellType
getCellType y x value game =
    let
        region =
            ( y // 3, x // 3 )

        validValues =
            Array.foldl outerFold Array.empty game

        outerFold row uniqueList =
            Array.filter isCellValid row
                |> Array.map (\cell -> cell.value)
                |> Array.append uniqueList

        regionCoords =
            getRegionBounds ( y, x )

        ( ( lowerY, lowerX ), ( upperX, upperY ) ) =
            regionCoords

        isXRow cell =
            cell.x == x

        isYRow cell =
            cell.y == y

        isInsideRegion cell =
            cell.x >= lowerX && cell.x <= upperX && cell.y >= lowerY && cell.y <= upperY

        isCellValid cell =
            (cell.x /= x && cell.y /= y)
                && (isXRow cell || isYRow cell || isInsideRegion cell)
                && (cell.value /= "")

        alreadyContainsValue =
            Array.toList validValues
                |> Set.fromList
                |> Set.member value
    in
    if alreadyContainsValue then
        InvalidCell

    else
        RegularCell


getRegionBounds : ( Int, Int ) -> ( ( Int, Int ), ( Int, Int ) )
getRegionBounds bounds =
    -- Convert (y, x) coordinates to the upper / lower bounds in a 9x9 grid
    case bounds of
        ( 0, 0 ) ->
            ( ( 0, 0 )
            , ( 2, 2 )
            )

        ( 1, 0 ) ->
            ( ( 3, 0 )
            , ( 5, 2 )
            )

        ( 2, 0 ) ->
            ( ( 6, 0 )
            , ( 8, 2 )
            )

        ( 0, 1 ) ->
            ( ( 0, 3 )
            , ( 2, 5 )
            )

        ( 1, 1 ) ->
            ( ( 3, 3 )
            , ( 5, 5 )
            )

        ( 2, 1 ) ->
            ( ( 6, 3 )
            , ( 8, 5 )
            )

        ( 0, 2 ) ->
            ( ( 0, 6 )
            , ( 2, 8 )
            )

        ( 1, 2 ) ->
            ( ( 3, 6 )
            , ( 5, 8 )
            )

        ( 2, 2 ) ->
            ( ( 6, 6 )
            , ( 8, 8 )
            )

        _ ->
            Debug.log "INVALID REGION SUPPLIED!" ( ( 999, 999 ), ( 999, 999 ) )


getFromCoordinates : Int -> Int -> Array (Array SudokuCell) -> SudokuCell
getFromCoordinates y x game =
    let
        defaultCellValue =
            { y = y
            , x = x
            , value = ""
            , cellType = RegularCell
            }

        outerVal =
            case Array.get y game of
                Nothing ->
                    Array.fromList [ defaultCellValue ]

                Just row ->
                    row

        innerVal =
            case Array.get x outerVal of
                Nothing ->
                    defaultCellValue

                Just cell ->
                    cell
    in
    innerVal


createSetFromCells : List String -> List String -> List String -> Set String
createSetFromCells list1 list2 list3 =
    let
        set1 =
            Set.fromList list1

        set2 =
            Set.fromList list2

        set3 =
            Set.fromList list3

        allValues =
            Set.union (Set.union set1 set2) set3
    in
    Set.filter (\x -> x /= "0") allValues


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
                , onInput (\val -> SetCellValue { cell | value = val })
                ]
                []
            ]
    in
    div
        [ classList
            [ ( "cell", True )
            , ( "flex", True )
            , ( "static", cell.cellType == StaticCell )
            , ( "invalid", cell.cellType == InvalidCell )
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
