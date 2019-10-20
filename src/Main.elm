module Main exposing (Clue, Direction(..), Flags, Model, Msg(..), Puzzle, init, initClues, initPuzzle, main, subscriptions, update, view)

import Array exposing (Array)
import Browser
import Browser.Events exposing (onKeyDown)
import Browser.Navigation as Nav exposing (Key)
import Html exposing (Html, b, div, text)
import Html.Attributes
import Html.Events
import Json.Decode as Decode
import Url exposing (Url)


type Key
    = Character Char
    | Control String


keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.map toKey (Decode.field "key" Decode.string)


toKey : String -> Msg
toKey string =
    let
        rawKey =
            Debug.log "RAW: " string
    in
    case String.uncons string of
        Just ( char, "" ) ->
            KeyPress (Character char)

        _ ->
            KeyPress (Control string)


squareSize : Int
squareSize =
    40


type alias Player =
    -- TODO: think of refactorign this to be all within one maybe... it makes `updateSelection` weird
    { selection : Maybe ( Int, Int )
    , selectionDirection : Direction
    , activeClue : Maybe Clue
    }


type Square
    = Blank
    | Empty
    | Filled Char


type alias Puzzle =
    { title : String
    , author : String
    , width : Int
    , height : Int
    , clues : List Clue
    , grid : Array (Array Square)
    }


type alias Clue =
    { direction : Direction
    , number : Int
    , x : Int
    , y : Int
    , answer : String
    , clue : String
    }


type Direction
    = Across
    | Down


type alias Model =
    { puzzle : Puzzle
    , player : Player
    }


type alias Flags =
    {}


type Msg
    = None
    | SquareClick ( Int, Int )
    | OnUrlRequest Browser.UrlRequest
    | OnUrlChange Url
    | KeyPress Key


toGrid : Int -> Int -> List Clue -> Array (Array Square)
toGrid height width clues =
    Array.initialize height (toRow width clues)


toRow : Int -> List Clue -> Int -> Array Square
toRow width clues rowNum =
    Array.initialize width (toSquare clues rowNum)


toSquare : List Clue -> Int -> Int -> Square
toSquare clues rowNum colNum =
    case List.foldr (squareFor rowNum colNum) Nothing clues of
        Just square ->
            square

        Nothing ->
            Blank


squareFor : Int -> Int -> Clue -> Maybe Square -> Maybe Square
squareFor rowNum colNum clue curSquareVal =
    let
        -- How far into the word would we be assuming we are on the right line
        adjustedX =
            colNum - clue.x

        -- How far into the word would we be assuming we are on the right column
        adjustedY =
            rowNum - clue.y
    in
    case clue.direction of
        Across ->
            if clue.y == rowNum then
                case Array.get adjustedX (Array.fromList (String.toList clue.answer)) of
                    Just c ->
                        Just Empty

                    Nothing ->
                        curSquareVal

            else
                curSquareVal

        Down ->
            if clue.x == colNum then
                case Array.get adjustedY (Array.fromList (String.toList clue.answer)) of
                    Just c ->
                        Just Empty

                    Nothing ->
                        curSquareVal

            else
                curSquareVal


isActiveClue : Player -> Clue -> Bool
isActiveClue player clue =
    case player.selection of
        Just sel ->
            withinClue sel clue && clue.direction == player.selectionDirection

        Nothing ->
            False


withinClue : ( Int, Int ) -> Clue -> Bool
withinClue ( selX, selY ) clue =
    case clue.direction of
        Across ->
            let
                lowerX =
                    clue.x

                upperX =
                    clue.x + String.length clue.answer

                y =
                    clue.y
            in
            selY == y && selX >= lowerX && selX <= upperX

        Down ->
            let
                lowerY =
                    clue.y

                upperY =
                    clue.y + String.length clue.answer

                x =
                    clue.x
            in
            selX == x && selY >= lowerY && selY <= upperY



-- TODO is secondarily highlighted square


initPlayer : Player
initPlayer =
    Player Nothing Across Nothing


initPuzzle : Puzzle
initPuzzle =
    Puzzle "Test" "Adam" 7 7 initClues (toGrid 7 7 initClues)


initClues : List Clue
initClues =
    mini


mini : List Clue
mini =
    [ Clue Across 1 2 0 "PAD" "Follower of the 'lily; and 'i'"
    , Clue Across 4 1 1 "HIDEF" "the 'HD' of HDTV, for short"
    , Clue Across 6 0 2 "DILBERT" "long running office themeed comic"
    , Clue Across 8 0 3 "SHARPEI" "Wrinkly dog breed"
    , Clue Across 9 0 4 "LATEFEE" "Credit card penalty"
    , Clue Across 10 1 5 "TEARS" "Runs down cheaks"
    , Clue Across 11 2 6 "SKY" "Big ___ Country, nickname for montana"
    , Clue Down 1 2 0 "PILATES" "Popular exercise regime"
    , Clue Down 2 3 0 "ADBREAK" "Interruption in a YouTube video"
    , Clue Down 3 4 0 "DEEPFRY" "Cook submerged in oil"
    , Clue Down 4 1 1 "HIHAT" "Drum kit part"
    , Clue Down 5 5 1 "FREES" "Lets go"
    , Clue Down 6 0 2 "DSL" "Internet connection option"
    , Clue Down 7 6 2 "TIE" "How to ___ a ___, popular Google search"
    ]



-- usaToday : List Clue
-- usaToday =
--     [ Clue Across 1 2 0 "PAD" "Follower of the 'lily; and 'i'" ]


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( Model initPuzzle initPlayer, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    { title = model.puzzle.title
    , body = [ viewGame model ]
    }


viewGame : Model -> Html Msg
viewGame model =
    div [ Html.Attributes.style "display" "flex" ]
        [ div [ Html.Attributes.style "width" "350px" ]
            [ viewAbout model
            , viewClues model
            ]
        , viewBoard model
        ]


viewAbout : Model -> Html Msg
viewAbout model =
    div
        []
        [ text model.puzzle.title
        , text model.puzzle.author
        ]


viewClues : Model -> Html Msg
viewClues model =
    div []
        [ Html.h1 [] [ text "Across" ]
        , Html.ol [] (List.map (viewClue model.player) (List.filter (clueDirection Across) model.puzzle.clues))
        , Html.h1 [] [ text "Down" ]
        , Html.ol [] (List.map (viewClue model.player) (List.filter (clueDirection Down) model.puzzle.clues))
        ]


clueDirection : Direction -> Clue -> Bool
clueDirection direction clue =
    clue.direction == direction


viewClue : Player -> Clue -> Html Msg
viewClue player clue =
    Html.li (clueStyles player clue)
        [ text clue.clue
        ]


clueStyles : Player -> Clue -> List (Html.Attribute Msg)
clueStyles player clue =
    if isActiveClue player clue then
        [ Html.Attributes.style "background-color" "#ddddff"
        ]

    else
        []


viewBoard : Model -> Html Msg
viewBoard model =
    div
        [ Html.Attributes.style "width" "fit-content"
        , Html.Attributes.style "display" "flex"
        , Html.Attributes.style "flex-direction" "row"
        , Html.Attributes.style "flex-wrap" "wrap"
        , Html.Attributes.style "border" "1px solid black"
        , Html.Attributes.style "border-width" "0 0 0 1px"
        , Html.Attributes.style "width" (String.fromInt (squareSize * model.puzzle.width) ++ "px")
        , Html.Attributes.style "height" (String.fromInt (squareSize * model.puzzle.height) ++ "px")
        ]
        (model.puzzle.grid
            |> Array.indexedMap (viewRow model)
            |> Array.toList
            |> List.foldr List.append []
        )


viewRow : Model -> Int -> Array Square -> List (Html Msg)
viewRow model y row =
    Array.toList (Array.indexedMap (viewSquare model y) row)


viewSquare : Model -> Int -> Int -> Square -> Html Msg
viewSquare model y x square =
    case square of
        Empty ->
            case squareNumber model x y of
                Just num ->
                    Html.div (fillableSquareStyle model.player y x)
                        [ div
                            [ Html.Attributes.style "font-size" "14px"
                            , Html.Attributes.style "padding-left" "3px"
                            , Html.Attributes.style "float" "left"
                            ]
                            [ text num ]
                        ]

                Nothing ->
                    Html.div (fillableSquareStyle model.player y x)
                        []

        Filled c ->
            case squareNumber model x y of
                Just num ->
                    Html.div (fillableSquareStyle model.player y x)
                        [ div
                            [ Html.Attributes.style "font-size" "14px"
                            , Html.Attributes.style "float" "left"
                            ]
                            [ text num ]
                        , text (String.toUpper (String.fromChar c))
                        ]

                Nothing ->
                    Html.div (fillableSquareStyle model.player y x)
                        [ text (String.toUpper (String.fromChar c))
                        ]

        Blank ->
            Html.div emptySquareStyle []


squareNumber : Model -> Int -> Int -> Maybe String
squareNumber model x y =
    model.puzzle.clues
        |> List.filter (clueAt x y)
        |> List.head
        |> Maybe.map (\clue -> clue.number)
        |> Maybe.map (\number -> String.fromInt number)


clueAt : Int -> Int -> Clue -> Bool
clueAt x y clue =
    clue.x == x && clue.y == y


squareStyle : List (Html.Attribute Msg)
squareStyle =
    [ Html.Attributes.style "display" "inline-block"
    , Html.Attributes.style "border" "1px solid black"
    , Html.Attributes.style "border-width" "0 1px 1px 0"
    , Html.Attributes.style "height" "39px"
    , Html.Attributes.style "width" "39px"
    , Html.Attributes.style "text-align" "center"
    , Html.Attributes.style "font-size" "34px"
    , Html.Attributes.style "user-select" "none"
    ]


fillableSquareStyle : Player -> Int -> Int -> List (Html.Attribute Msg)
fillableSquareStyle player y x =
    selectionStyles player ( x, y )
        ++ squareStyle


selectionStyles : Player -> ( Int, Int ) -> List (Html.Attribute Msg)
selectionStyles player location =
    case player.selection of
        Just selection ->
            if selection == location then
                [ Html.Attributes.style "background-color" "#8787ff"
                , Html.Events.onClick (SquareClick location)
                ]

            else
                case player.activeClue of
                    Just c ->
                        if withinClue location c then
                            [ Html.Attributes.style "background-color" "#e0e0fb"
                            , Html.Events.onClick (SquareClick location)
                            ]

                        else
                            [ Html.Attributes.style "background-color" "white"
                            , Html.Events.onClick (SquareClick location)
                            ]

                    Nothing ->
                        [ Html.Attributes.style "background-color" "white"
                        , Html.Events.onClick (SquareClick location)
                        ]

        Nothing ->
            [ Html.Attributes.style "background-color" "white"
            , Html.Events.onClick (SquareClick location)
            ]


emptySquareStyle : List (Html.Attribute Msg)
emptySquareStyle =
    Html.Attributes.style "background-color" "black" :: squareStyle


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        SquareClick location ->
            ( updateActiveClue (moveSelection location model), Cmd.none )

        KeyPress key ->
            case key of
                Character c ->
                    ( progressSelection (fillSquare model c), Cmd.none )

                Control s ->
                    case s of
                        "ArrowRight" ->
                            ( nextClue model, Cmd.none )

                        "ArrowDown" ->
                            ( nextClue model, Cmd.none )

                        "ArrowUp" ->
                            ( prevClue model, Cmd.none )

                        "ArrowLeft" ->
                            ( prevClue model, Cmd.none )

                        "Shift" ->
                            ( changeDirection model, Cmd.none )

                        _ ->
                            ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


changeDirection : Model -> Model
changeDirection model =
    let
        player =
            model.player

        newPlayer =
            { player | selectionDirection = toggleDirection player.selectionDirection }
    in
    { model | player = newPlayer } |> seekClue clueUnder


nextClue : Model -> Model
nextClue model =
    seekClue clueAfter model


prevClue : Model -> Model
prevClue model =
    seekClue clueBefore model


seekClue : (Model -> Int -> Direction -> Maybe Clue) -> Model -> Model
seekClue seekFn model =
    case model.player.activeClue of
        Just c ->
            let
                player =
                    model.player
            in
            case seekFn model c.number player.selectionDirection of
                Just clue ->
                    let
                        newPlayer =
                            { player
                                | activeClue = Just clue
                                , selection = nextOpenSpace clue model.puzzle.grid
                                , selectionDirection = clue.direction
                            }
                    in
                    { model | player = newPlayer }

                Nothing ->
                    model

        Nothing ->
            let
                player =
                    model.player
            in
            case seekFn model 0 Across of
                Just clue ->
                    let
                        newPlayer =
                            { player
                                | activeClue = Just clue
                                , selection = nextOpenSpace clue model.puzzle.grid
                                , selectionDirection = clue.direction
                            }
                    in
                    { model | player = newPlayer }

                Nothing ->
                    model


nextOpenSpace : Clue -> Array (Array Square) -> Maybe ( Int, Int )
nextOpenSpace clue grid =
    List.filter (\a -> not (hasLetter grid clue a)) (possibleSelections clue)
        |> List.head


possibleSelections : Clue -> List ( Int, Int )
possibleSelections clue =
    case clue.direction of
        Across ->
            List.range clue.x (clue.x + String.length clue.answer - 1)
                |> List.map (\a -> ( a, clue.y ))

        Down ->
            List.range clue.y (clue.y + String.length clue.answer - 1)
                |> List.map (\a -> ( clue.x, a ))


hasLetter : Array (Array Square) -> Clue -> ( Int, Int ) -> Bool
hasLetter grid clue spot =
    let
        square =
            grid
                |> Array.get (Tuple.second spot)
                |> Maybe.withDefault Array.empty
                |> Array.get (Tuple.first spot)
    in
    case square of
        Just (Filled _) ->
            True

        _ ->
            False


clueUnder : Model -> Int -> Direction -> Maybe Clue
clueUnder model number direction =
    let
        otherDirectionClues =
            model.puzzle.clues
                |> List.reverse
                |> List.filter (\a -> a.direction == toggleDirection direction)
                |> List.filter (\a -> not (wordFilled model.puzzle.grid a))
    in
    model.puzzle.clues
        |> List.reverse
        |> List.filter (\a -> a.number == number && a.direction == direction)
        |> List.filter (\a -> not (wordFilled model.puzzle.grid a))
        |> reverseArgs List.append otherDirectionClues
        |> List.head


clueBefore : Model -> Int -> Direction -> Maybe Clue
clueBefore model number direction =
    let
        otherDirectionClues =
            model.puzzle.clues
                |> List.reverse
                |> List.filter (\a -> a.direction == toggleDirection direction)
                |> List.filter (\a -> not (wordFilled model.puzzle.grid a))
    in
    model.puzzle.clues
        |> List.reverse
        |> List.filter (\a -> a.number < number && a.direction == direction)
        |> List.filter (\a -> not (wordFilled model.puzzle.grid a))
        |> reverseArgs List.append otherDirectionClues
        |> List.head


wordFilled : Array (Array Square) -> Clue -> Bool
wordFilled grid clue =
    case nextOpenSpace clue grid of
        Just _ ->
            False

        Nothing ->
            True


clueAfter : Model -> Int -> Direction -> Maybe Clue
clueAfter model number direction =
    let
        otherDirectionClues =
            model.puzzle.clues
                |> List.filter (\a -> a.direction == toggleDirection direction)
                |> List.filter (\a -> not (wordFilled model.puzzle.grid a))
    in
    model.puzzle.clues
        |> List.filter (\a -> a.number > number && a.direction == direction)
        |> List.filter (\a -> not (wordFilled model.puzzle.grid a))
        |> reverseArgs List.append otherDirectionClues
        |> List.head


reverseArgs : (a -> a -> a) -> a -> a -> a
reverseArgs fn a1 a2 =
    fn a2 a1


toggleDirection : Direction -> Direction
toggleDirection direction =
    case direction of
        Across ->
            Down

        Down ->
            Across


activeClue : Model -> Maybe Clue
activeClue model =
    model.puzzle.clues
        |> List.filter (isActiveClue model.player)
        |> List.head


updateActiveClue : Model -> Model
updateActiveClue model =
    let
        player =
            model.player

        newPlayer =
            { player | activeClue = activeClue model }
    in
    { model | player = newPlayer }


moveSelection : ( Int, Int ) -> Model -> Model
moveSelection location model =
    let
        player =
            model.player
    in
    case player.selection of
        Just sel ->
            if sel == location then
                let
                    newPlayer =
                        { player
                            | selectionDirection = toggleDirection player.selectionDirection
                        }
                in
                { model | player = newPlayer }

            else
                let
                    newPlayer =
                        { player | selection = Just location }
                in
                { model | player = newPlayer }

        Nothing ->
            let
                newPlayer =
                    { player | selection = Just location }
            in
            { model | player = newPlayer }


progressSelection : Model -> Model
progressSelection model =
    { model | player = progressPlayerSelection model }


progressPlayerSelection : Model -> Player
progressPlayerSelection model =
    let
        player =
            model.player
    in
    case player.selection of
        Just ( x, y ) ->
            { player | selection = nextSpot model }

        Nothing ->
            player


nextSpot : Model -> Maybe ( Int, Int )
nextSpot model =
    case model.player.selection of
        Just ( curX, curY ) ->
            case activeClue model of
                Just clue ->
                    case model.player.selectionDirection of
                        Across ->
                            if curX < clue.x + String.length clue.answer - 1 then
                                Just ( curX + 1, curY )

                            else
                                Just ( curX, curY )

                        Down ->
                            if curY < clue.y + String.length clue.answer - 1 then
                                Just ( curX, curY + 1 )

                            else
                                Just ( curX, curY )

                Nothing ->
                    Nothing

        Nothing ->
            Nothing



-- no selection can't update anything.


fillSquare : Model -> Char -> Model
fillSquare model char =
    case model.player.selection of
        Just location ->
            fillAt model char location

        Nothing ->
            -- no selection
            model


fillAt : Model -> Char -> ( Int, Int ) -> Model
fillAt model char ( x, y ) =
    case Array.get y model.puzzle.grid of
        Just row ->
            let
                newRow =
                    Array.set x (Filled char) row

                puzzle =
                    model.puzzle

                newPuzzle =
                    { puzzle | grid = Array.set y newRow puzzle.grid }
            in
            { model | puzzle = newPuzzle }

        Nothing ->
            -- Selection was out of bounds on the y axis... should be impossible.
            model


subscriptions : Model -> Sub Msg
subscriptions model =
    onKeyDown keyDecoder


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = OnUrlRequest
        , onUrlChange = OnUrlChange
        }
