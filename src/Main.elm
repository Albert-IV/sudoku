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



-- type SudokuCell
--     = Array (Array SudokuCell)


type SudokuCell
    = StaticCell Int
    | InputCell Int
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
    Array.fromList (List.map convertSudokuRow state)


convertSudokuRow : List Int -> Array SudokuCell
convertSudokuRow row =
    let
        convertCell val =
            case val of
                0 ->
                    EmptyCell

                _ ->
                    StaticCell val
    in
    Array.fromList (List.map convertCell row)


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
                (Array.initialize 9 (\n -> StaticCell n))
                (Array.get y game)

        updatedRow =
            Array.set x (StaticCell 10) selectedRow

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
        displayColumn columnIdx column =
            case column of
                StaticCell value ->
                    div [ class "cell flex static", onClick (MakeInput rowIdx columnIdx) ] [ text (String.fromInt value) ]

                InputCell value ->
                    div [ class "cell flex", onClick (MakeInput rowIdx columnIdx) ] [ text (String.fromInt value) ]

                EmptyCell ->
                    div [ class "cell flex", onClick (MakeInput rowIdx columnIdx) ] [ text "" ]

        cellContents =
            List.indexedMap displayColumn columns
    in
    [ div [ class "flex" ] cellContents ]
