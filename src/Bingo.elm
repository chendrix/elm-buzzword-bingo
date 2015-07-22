module Bingo where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Signal
import List
import Result
import String

import Json.Decode

import StartApp

-- MAIN
main =
  StartApp.start { model = model, view = view, update = update }


-- VIEW
view : Signal.Address Action -> State -> Html
view address model =
  div
    []
    [ div
        []
        [ viewPhraseInput address model.phraseInput
        , viewPointsInput address model.pointsInput
        , button
            [ onClick address SubmitEntry ]
            [ text "Add" ]
        ]
    , div
        []
        (viewEntries address model.entries)
    , button
        [ onClick address SortEntries ]
        [ text "Sort" ]
    , div
      []
      [ viewTotal address (pointTotal model.entries) ]
    ]


viewPhraseInput : Signal.Address Action -> String -> Html
viewPhraseInput address phrase =
  input
    [ on "input" targetValue (Signal.message address << UpdatePhrase)
    , onEnter address SubmitEntry
    , placeholder "Phrase"
    , value phrase
    ]
    []


viewPointsInput : Signal.Address Action -> Int -> Html
viewPointsInput address points =
  input
    [ on "input" targetValue (
        \strPoints ->
          String.toInt strPoints
          |> Result.toMaybe
          |> Maybe.withDefault 0
          |> Signal.message address << UpdatePoints
      )
    , onEnter address SubmitEntry
    , placeholder "0"
    , value (toString points)
    , type' "number"
    , Html.Attributes.min "0"
    ]
    []


viewEntries : Signal.Address Action -> List Entry -> List Html
viewEntries address entries =
  List.indexedMap (viewEntry address) entries
  |> List.map (\html -> li [] [ html ])


viewEntry : Signal.Address Action -> Int -> Entry -> Html
viewEntry address n entry =
  div
    [ onClick address (ToggleEntry n)
    , classList
        [ ( "entry", True )
        , ( "entry--heard", entry.heard )
        ]
    ]
    [ span [] [ text entry.phrase ]
    , span [] [ text (toString entry.points) ]
    , button [ onClick address (DeleteEntry n) ] [ text "✗" ]
    ]


viewTotal : Signal.Address Action -> Int -> Html
viewTotal address total =
  div
    []
    [ text "Points: "
    , text (toString total)
    ]

-- MODEL
type alias State =
  { entries : List Entry
  , phraseInput : String
  , pointsInput : Int
  }


type alias Entry =
  { phrase : String
  , points : Int
  , heard : Bool
  }


model : State
model =
  { entries = []
  , phraseInput = ""
  , pointsInput = 0
  }


extractEntry : State -> Entry
extractEntry model =
  { phrase = model.phraseInput
  , points = model.pointsInput
  , heard = False
  }


pointTotal : List Entry -> Int
pointTotal entries =
  List.filter (.heard) entries
  |> List.map (.points)
  |> List.sum


-- UPDATE
type Action
  = UpdatePhrase String
  | UpdatePoints Int
  | SubmitEntry
  | SortEntries
  | ToggleEntry Int
  | DeleteEntry Int


update : Action -> State -> State
update action model =
  case action of
    UpdatePhrase phrase ->
      { model | phraseInput <- phrase }

    UpdatePoints points ->
      { model | pointsInput <- points }

    SubmitEntry ->
      { model | entries <- ((extractEntry model) :: model.entries)
              , pointsInput <- 0
              , phraseInput <- ""
      }

    SortEntries ->
      { model | entries <- (sortEntries model.entries) }

    ToggleEntry n ->
      { model | entries <- (applyToMember n toggleEntry model.entries) }

    DeleteEntry n ->
      { model | entries <- (dropN n model.entries) }


toggleEntry : Entry -> Entry
toggleEntry entry =
  { entry | heard <- (not entry.heard) }


sortEntries : List Entry -> List Entry
sortEntries =
  List.sortBy .points


-- LIBRARY
applyToMember : Int -> (a -> a) -> List a -> List a
applyToMember n f =
  List.indexedMap (\i x -> if i == n then f x else x)


dropN : Int -> List a -> List a
dropN n l =
  case l of
    [] ->
      []

    (x::xs) ->
      if  | n <= 0 ->
              xs
          | otherwise ->
              x :: (dropN (n - 1) l)


onEnter : Signal.Address a -> a -> Attribute
onEnter address value =
    on "keydown"
      (Json.Decode.customDecoder keyCode is13)
      (\_ -> Signal.message address value)


is13 : Int -> Result String ()
is13 code =
  if code == 13 then Ok () else Err "not the right key code"