module Main exposing (Model(..), Msg(..), init, main, subscriptions, update, view)

import Array exposing (Array)
import Browser exposing (Document)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder, field, string)
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


type Model
    = DisplayGame (Array (Array SudokuCell))


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
    ( DisplayGame getInitialGame, Cmd.none )



-- UPDATE


type Msg
    = None
    | MakeInput Int Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MakeInput y x ->
            ( convertToStaticCell y x model, Cmd.none )

        _ ->
            ( model, Cmd.none )


convertGameToList : Array (Array SudokuCell) -> List (List SudokuCell)
convertGameToList game =
    Array.toList (Array.map Array.toList game)


convertToStaticCell : Int -> Int -> Model -> Model
convertToStaticCell y x (DisplayGame game) =
    let
        selectedRow =
            Maybe.withDefault
                (Array.initialize 9 (\n -> { x = n, y = y, value = n, cellType = StaticCell }))
                (Array.get y game)

        updatedRow =
            Array.set x { cellType = StaticCell, value = 10, x = x, y = y } selectedRow

        updatedGame =
            Array.set y updatedRow game
    in
    DisplayGame updatedGame



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


pageContents : Model -> List (Html Msg)
pageContents model =
    [ div [ class "container" ]
        [ div []
            [ h1 [] [ text "SUDOKU!!!!" ]
            , displayTable model
            ]
        ]
    ]


displayTable : Model -> Html Msg
displayTable (DisplayGame game) =
    let
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
