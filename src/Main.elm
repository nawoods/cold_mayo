module Main exposing (main)

import Browser
import Html exposing (Html)

import Array

import Svg as S
import Svg.Attributes as SA
import Element as E
import Element.Events as EE
import Element.Input as EI
import Element.Font as Font

import Dropdown exposing (Dropdown, OutMsg(..), Placement(..))

import PremierePoll as Poll
import PremierePoll.Data as PollData

-- MAIN

main : Program () Model Msg
main = 
  Browser.document { init = init, update = update, view = view, subscriptions = subscriptions }



-- MODEL

type alias Model =
  { selectedPlayer : Maybe String
  , selectedDiscipline : Maybe Poll.Discipline
  , selectedYearMonth : Maybe (Int, Int)
  , playerDropdown : Dropdown String
  , premierePoll : Maybe Poll.PremirePoll
  , mouseoverBar : Maybe Int
  }


init : () -> ( Model, Cmd Msg )
init _ = 
  (
    { selectedPlayer = Nothing
    , mouseoverBar = Nothing
    , selectedDiscipline = Nothing
    , selectedYearMonth = Nothing
    , premierePoll = Nothing
    , playerDropdown = Dropdown.init
        |> Dropdown.id "player-dropdown"
        |> Dropdown.inputType Dropdown.TextField
        |> Dropdown.filterType Dropdown.StartsWithThenContains
    }
    , Cmd.none
  )




-- UPDATE 

type Msg = 
  PlayerDropdownMsg (Dropdown.Msg String)
  | GraphBarMouseEnterMsg Int
  | GraphBarMouseLeaveMsg
  | DisciplineSelectMsg Poll.Discipline
  | YearMonthSelectMsg (Int, Int)
  | ClearSelections

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = 
  case msg of
    PlayerDropdownMsg subMsg ->
      let
        ( dropdown, cmd, outMsg ) =
          Dropdown.update subMsg model.playerDropdown
      in
      ( { model
           | playerDropdown = dropdown
           , selectedPlayer =
               case outMsg of
                   Selected (_, _, option) ->
                      Just option
                   TextChanged _ ->
                      Nothing
                   _ ->
                      model.selectedPlayer
        }
        , Cmd.map PlayerDropdownMsg cmd
      )
    GraphBarMouseEnterMsg bar ->
      ( { model | mouseoverBar = Just bar }, Cmd.none )
    GraphBarMouseLeaveMsg ->
      ( { model | mouseoverBar = Nothing }, Cmd.none )
    DisciplineSelectMsg discipline ->
      ( { model | selectedDiscipline = Just discipline }, Cmd.none )
    YearMonthSelectMsg ym ->
      ( case model.selectedDiscipline of
          Just discipline ->
            let
              premierePoll =
                PollData.polls
                |> List.filter 
                    (\poll -> poll.yearMonth == ym && poll.discipline == discipline)
                |> List.head
            in
            { model
              | selectedYearMonth = Just ym
              , premierePoll = premierePoll
              , playerDropdown =
                  case premierePoll of 
                    Just poll ->
                      model.playerDropdown
                        |> Dropdown.stringOptions 
                          (poll.ballots |> Poll.getPlayerNames)
                    Nothing ->
                      model.playerDropdown
            }
          Nothing ->
            { model | selectedYearMonth = Just ym }
      , Cmd.none 
      )
    ClearSelections ->
      ( { model
            | selectedDiscipline = Nothing
            , selectedPlayer = Nothing
            , selectedYearMonth = Nothing
            , premierePoll = Nothing
        }
      , Cmd.none
      )
                    
          
    






-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none







-- VIEW

view : Model -> Browser.Document Msg
view model = 
  { title = "Cold Mayo"
  , body = [ body model ]
  }

body : Model -> Html Msg
body model =
  E.layout
    []
    (E.column
      [ E.width E.fill, E.height E.fill ]
      [ E.column 
          [ E.centerX
          , E.spacing 40
          , E.padding 40 
          , Font.size 18
          ]
          [ title
          , content model
          ]
      , footer
      ]
    )

title : E.Element msg
title =
  E.column
  []
  [ E.el [Font.size 24] <| E.text "Cold Mayo"
  , E.el [Font.size 16] <| E.text "A delicious plant-based spread for Nestris enthusiasts"
  ]

footer : E.Element msg
footer =
  E.el 
  [ E.alignBottom
  , E.alignRight 
  , E.padding 10
  , Font.size 16
  ] 
  <| E.paragraph 
    [] 
    [ E.text "Put together by Nick \"arbaro\" Woods of NES Tetris fame. View the source code "
    , E.link [ cornflower ] { url = "https://github.com/nawoods/cold_mayo", label = E.text "here" }
    , E.text "."
    ]

content : Model -> E.Element Msg
content model =
  case (model.selectedDiscipline, model.selectedYearMonth) of
    (Nothing, _) ->
      disciplineButtonRow
    (Just _, Nothing) ->
      E.column
        [ E.spacing 20 ]
        [ disciplineButtonRow
        , yearMonthButtonRow model
        ]
    _ ->
      case model.premierePoll of
        Just _ ->
          E.column
            [ E.spacing 30 ]
            [ pollHeading model
            , playerDropdown model
            , voteChart model
            , case model.mouseoverBar of
                Nothing ->
                  EI.button
                  [ E.alignRight ]
                  { onPress = Just ClearSelections
                  , label = E.text "Go again ->"
                  }
                _ ->
                  E.none
          ]
        _ ->
          E.column
            [ E.spacing 20 ]
            [ disciplineButtonRow
            , yearMonthButtonRow model
            , E.text "No poll found"
            ]

pollHeading : Model -> E.Element msg
pollHeading model =
  case (model.selectedYearMonth, model.selectedDiscipline) of
    (Just yearMonth, Just discipline) ->
      let 
        yearMonthText = 
          Maybe.withDefault "?" (Poll.longYearMonthString yearMonth)
      in
      E.text
        (  yearMonthText
        ++ " "
        ++ Poll.disciplineString discipline
        ++ " Poll"
        )
    _ ->
      E.none


disciplineButtonRow : E.Element Msg
disciplineButtonRow =
  E.row
  [ E.spacing 20 ]
  ( [Poll.Open, Poll.Das, Poll.Tap]
      |> List.map (\d -> disciplineButton d (Poll.disciplineString d))
      |> List.intersperse (E.text "|")
  )

disciplineButton : Poll.Discipline -> String -> E.Element Msg
disciplineButton discipline label =
  EI.button
  []
  { onPress = Just (DisciplineSelectMsg discipline)
  , label = E.text label
  }


yearMonthButtonRow : Model -> E.Element Msg
yearMonthButtonRow model =
  E.wrappedRow
  [ E.spacing 20 ]
  ( PollData.polls
    |> List.filter (\poll -> Just poll.discipline == model.selectedDiscipline)
    |> List.map .yearMonth
    |> List.map yearMonthButton
    |> List.intersperse (E.text "|")
  )
  

yearMonthButton : (Int, Int) -> E.Element Msg
yearMonthButton ym =
  let
    monthText = Poll.longYearMonthString ym
    label =
      case monthText of
        Just a ->
          a
        Nothing ->
         "?"
  in
  EI.button
  []
  { onPress = Just (YearMonthSelectMsg ym)
  , label = E.text label
  }

cornflower : E.Attr decorative msg
cornflower = Font.color (E.rgb255 100 149 237)

elsvg : List (S.Attribute msg) -> List (S.Svg msg) -> E.Element msg
elsvg xs ys = S.svg xs ys |> E.html

svgRect : Bool -> Int -> E.Element msg
svgRect sel ht =
  elsvg
    [ SA.width "20"
    , SA.height <| String.fromInt <| 10 * ht
    ]
    [ S.rect
        [ SA.width "100%" 
        , SA.height "100%"
        , SA.fill ( if sel then "orange" else "cornflowerblue" )
        ]
        []
    ]

graphBar : Model -> Int -> Int -> E.Element Msg
graphBar model col ht = 
  let
    sel = Just col == model.mouseoverBar
    addInfoBox = (\xs -> if sel then (E.below (graphBarTooltip model)) :: xs else xs)
  in
  E.el 
    (addInfoBox
      [ E.alignBottom 
      , EE.onMouseEnter (GraphBarMouseEnterMsg col)
      , EE.onMouseLeave GraphBarMouseLeaveMsg
      ] 
    )
    (svgRect sel ht)

graphBarTooltip : Model -> E.Element msg
graphBarTooltip model =
  case (model.mouseoverBar, model.selectedPlayer, model.premierePoll) of
      (Just x, Just p, Just poll) ->
        let
          voters = poll.ballots
            |> Poll.collectPlayerVotes p
            |> Array.fromList
            |> Array.get (x - 1)
        in
        case voters of
            Just v ->
              E.column
                [ E.paddingXY 0 10 ]
                [ E.text ("Ranked " 
                    ++ ordinal x 
                    ++ " by " 
                    ++ (v |> List.length |> String.fromInt)
                    ++ " pollster"
                    ++ if List.length v == 1 then "" else "s"
                  ),
                  E.column
                  [ Font.size 14 ]
                  (List.map (\a -> E.text a) v)
                ]
            _ ->
              E.none
      _ ->
        E.none

lineVert : E.Element msg
lineVert = 
  elsvg
    [ SA.width "5" 
    , SA.height "250"
    ]
    [ S.rect
        [ SA.width "100%" 
        , SA.height "100%"
        , SA.fill "black"
      ]
      []
    ]

lineHoriz : E.Element msg
lineHoriz =
  elsvg
    [ SA.width "505" 
    , SA.height "5"
    ]
    [ S.rect
        [ SA.width "100%" 
        , SA.height "100%"
        , SA.fill "black"
      ]
      []
    ]



voteChart : Model -> E.Element Msg
voteChart model =
  let
    votes =
      case (model.selectedPlayer, model.premierePoll) of
          (Just player, Just poll) ->
            Poll.collectPlayerVotes player poll.ballots |> List.map List.length
          _ ->
            List.repeat 25 0
  in
  E.column
    []
    [ E.row
      []
      (lineVert :: List.map2 (graphBar model) (List.range 1 25) votes)
    , lineHoriz
    , case model.mouseoverBar of
        Nothing ->
          E.row
           [ E.paddingXY 5 5, E.width E.fill ]
           [ E.el [ E.alignLeft ] <| E.text "1"
           , E.el [ E.centerX ] <| E.text "Rank"
           , E.el [ E.alignRight ] <| E.text "25"
           ]
        _ ->
          E.none
    ]
      
ordinal : Int -> String
ordinal x =
  let 
    suffix =
      case x // 10 of
          1 ->
            "th"
          _ ->
            case modBy 10 x of
                1 ->
                  "st"
                2 ->
                  "nd"
                3 ->
                  "rd"
                _ ->
                  "th"
  in String.fromInt x ++ suffix

playerDropdown : Model -> E.Element Msg
playerDropdown model = 
  Dropdown.label (E.text "Player") model.playerDropdown
    |> Dropdown.inputType Dropdown.TextField
    |> Dropdown.labelPlacement Left
    |> Dropdown.view PlayerDropdownMsg
