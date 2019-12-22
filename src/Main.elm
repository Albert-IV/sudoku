module Main exposing (Model(..), Msg(..), init, main, subscriptions, update, view)

import Array exposing (..)
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder, field, string)



-- MAIN


main =
    Browser.element
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
    = StaticValue Int
    | InputValue Int
    | Nothing


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
                    Nothing

                _ ->
                    StaticValue val
    in
    Array.fromList (List.map convertCell row)


init : () -> ( Model, Cmd Msg )
init _ =
    ( DisplayGame getInitialGame, Cmd.none )



-- UPDATE


type Msg
    = None


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    -- TODO: handle events
    -- case msg of
    --     MorePlease ->
    --         ( Loading, getRandomCatGif )
    --     GotGif result ->
    --         case result of
    --             Ok url ->
    --                 ( Success url, Cmd.none )
    --             Err _ ->
    --                 ( Failure, Cmd.none )
    ( model, Cmd.none )


convertGameToList : Array (Array SudokuCell) -> List (List SudokuCell)
convertGameToList game =
    Array.toList (Array.map Array.toList game)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "SUDOKU!!!!" ]
        , displayTable model
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
        displayRow row =
            div [ class "flex" ] (displayColumns row)
    in
    List.map displayRow rows


displayColumns : List SudokuCell -> List (Html Msg)
displayColumns columns =
    let
        displayColumn column =
            case column of
                StaticValue value ->
                    div [ class "cell flex" ] [ text (String.fromInt value) ]

                InputValue value ->
                    div [ class "cell flex" ] [ text (String.fromInt value) ]

                Nothing ->
                    div [ class "cell flex" ] [ text "" ]

        cellContents =
            List.map displayColumn columns
    in
    [ div [ class "flex" ] cellContents ]
