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
  { appStatus: AppStatus
  }

type AppStatus =
  SelectPoll SelectPollModel
  | ViewGraph ViewGraphModel

type SelectPollModel =
  SelectDiscipline
  | SelectYear
      { selectedDiscipline : Poll.Discipline
      }
  | SelectMonth
      { selectedDiscipline : Poll.Discipline  
      , selectedYear : Int
      }

type alias ViewGraphModel =
      { premierePoll: Poll.PremierePoll
      , playerDropdown : Dropdown String
      , selectedPlayer : Maybe String
      , mouseoverBar : Maybe Int
      }

init : () -> ( Model, Cmd Msg )
init _ = 
  (
    { appStatus = SelectPoll SelectDiscipline
    }
    , Cmd.none
  )




-- UPDATE 

type Msg = 
  SelectPollMsg SelectPollSubmsg
  | ViewGraphMsg ViewGraphSubmsg
  | MakePollSelectionMsg
     { discipline : Poll.Discipline
     , year : Int
     , month : Int
     }
  | ClearSelectionsMsg

type SelectPollSubmsg =
  DisciplineSelectMsg Poll.Discipline
  | YearSelectMsg Int

type ViewGraphSubmsg =
  PlayerDropdownMsg (Dropdown.Msg String)
  | GraphBarMouseEnterMsg Int
  | GraphBarMouseLeaveMsg

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = 
  case msg of
    SelectPollMsg subMsg ->
      case model.appStatus of
        SelectPoll selectPollStatus ->
          let
            newSelectPollStatus = updateSelectPoll subMsg selectPollStatus
          in
          ( { model
               | appStatus = SelectPoll newSelectPollStatus
            }
          , Cmd.none
          )
        _ ->
          ( model, Cmd.none )
    ViewGraphMsg subMsg ->
      case model.appStatus of
        ViewGraph viewGraphStatus ->
          let
            ( newViewGraphStatus, cmd ) = updateViewGraph subMsg viewGraphStatus
          in
          ( { model
               | appStatus = ViewGraph newViewGraphStatus 
            }
          , Cmd.map ViewGraphMsg cmd )
        _ ->
          ( model, Cmd.none )
    ClearSelectionsMsg ->
      ( { model 
           | appStatus = SelectPoll SelectDiscipline
        }
      , Cmd.none
      )
    MakePollSelectionMsg pollInfo ->
      let
        poll =
          Poll.findPoll 
            pollInfo.discipline 
            pollInfo.year 
            pollInfo.month
            PollData.polls
      in
      case poll of
        Just p ->
          let
            newViewGraphStatus = 
              { premierePoll = p
              , mouseoverBar = Nothing
              , selectedPlayer = Nothing
              , playerDropdown = Dropdown.init
                 |> Dropdown.id "player-dropdown"
                 |> Dropdown.inputType Dropdown.TextField
                 |> Dropdown.filterType Dropdown.StartsWithThenContains
                 |> Dropdown.stringOptions
                     (p.ballots |> Poll.getPlayerNames)
              }
          in
            ( { model
                 | appStatus = ViewGraph newViewGraphStatus
              }
            , Cmd.none 
            )
        Nothing ->
          ( { model 
               | appStatus = SelectPoll SelectDiscipline
            }
          , Cmd.none
          )
          
    
updateSelectPoll : SelectPollSubmsg -> SelectPollModel -> SelectPollModel
updateSelectPoll subMsg selectPollStatus =
  case subMsg of
    DisciplineSelectMsg discipline ->
      SelectYear { selectedDiscipline = discipline }
    YearSelectMsg year ->
      case selectPollStatus of
        SelectYear status ->
          SelectMonth 
            { selectedDiscipline = status.selectedDiscipline
            , selectedYear = year
            }
        _ ->
          selectPollStatus
    
updateViewGraph : ViewGraphSubmsg -> ViewGraphModel -> ( ViewGraphModel, Cmd ViewGraphSubmsg )
updateViewGraph subMsg viewGraphModel =
  case subMsg of
    PlayerDropdownMsg dropdownSubMsg ->
      let
        ( dropdown, cmd, outMsg ) =
          Dropdown.update dropdownSubMsg viewGraphModel.playerDropdown
      in
      ( { viewGraphModel
           | playerDropdown = dropdown
           , selectedPlayer =
               case outMsg of
                   Selected (_, _, option) ->
                      Just option
                   TextChanged _ ->
                      Nothing
                   _ ->
                      viewGraphModel.selectedPlayer
        }
        , Cmd.map PlayerDropdownMsg cmd

      )
    GraphBarMouseEnterMsg bar ->
       ( { viewGraphModel | mouseoverBar = Just bar }, Cmd.none )
    GraphBarMouseLeaveMsg ->
       ( { viewGraphModel | mouseoverBar = Nothing }, Cmd.none )
    
          



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
          , E.width (E.px 585)
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
  case model.appStatus of
    SelectPoll selectPollModel ->
      selectPollContent selectPollModel
    ViewGraph viewGraphModel ->
      viewGraphContent viewGraphModel
      

selectPollContent : SelectPollModel -> E.Element Msg
selectPollContent model =
  case model of
    SelectDiscipline ->
      disciplineButtonRow
    SelectYear args ->
      E.column
        [ E.spacing 20 ]
        [ disciplineButtonRow
        , yearButtonRow args.selectedDiscipline
        ]
    SelectMonth args ->
      E.column
        [ E.spacing 20 ]
        [ disciplineButtonRow
        , yearButtonRow args.selectedDiscipline
        , monthButtonRow args.selectedDiscipline args.selectedYear
        ]

viewGraphContent : ViewGraphModel -> E.Element Msg
viewGraphContent model =
  E.column
    [ E.spacing 30 ]
    [ pollHeading model.premierePoll
    , playerDropdown model.playerDropdown
    , voteChart model
    , case model.mouseoverBar of
        Nothing ->
          EI.button
          [ E.alignRight ]
          { onPress = Just ClearSelectionsMsg
          , label = E.text "Go again ->"
          }
        _ ->
          E.none
    ]


pollHeading : Poll.PremierePoll -> E.Element msg
pollHeading poll =
  let
    monthText =
      Maybe.withDefault "?" (Poll.getLongMonthName poll.month)
  in
  E.text
    (  monthText
    ++ " "
    ++ String.fromInt poll.year
    ++ " "
    ++ Poll.disciplineString poll.discipline
    ++ " Poll"
    )



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
  { onPress = Just (SelectPollMsg (DisciplineSelectMsg discipline))
  , label = E.text label
  }


yearButtonRow : Poll.Discipline -> E.Element Msg
yearButtonRow discipline =
  E.wrappedRow
  [ E.spacingXY 20 0 ]
  ( PollData.polls
    |> Poll.getYearsWithDiscipline discipline
    |> List.map yearButton
    |> List.intersperse (E.text "|")
  )

yearButton : Int -> E.Element Msg
yearButton year =
  EI.button
  []
  { onPress = Just (SelectPollMsg (YearSelectMsg year))
  , label = E.text (String.fromInt year)
  }

-- monthButtonRow : SelectPollModel -> E.Element Msg
monthButtonRow : Poll.Discipline -> Int -> E.Element Msg
monthButtonRow discipline year =
  E.wrappedRow
  [ E.spacingXY 20 0 ]
  ( PollData.polls
    |> Poll.getMonthsWithDisciplineAndYear discipline year
    |> List.map (monthButton discipline year)
    |> List.intersperse (E.text "|")
  )

monthButton : Poll.Discipline -> Int -> Int -> E.Element Msg
monthButton discipline year month =
  let
    monthText = Poll.getLongMonthName month
    label =
      case monthText of
        Just a ->
          a
        Nothing ->
         "?"
  in
  EI.button
  []
  { onPress = 
      Just (MakePollSelectionMsg 
        { discipline = discipline
        , year = year
        , month = month
        }
      )
  , label = E.text label
  }

cornflower : E.Attr decorative msg
cornflower = Font.color (E.rgb255 100 149 237)

elsvg : List (S.Attribute msg) -> List (S.Svg msg) -> E.Element msg
elsvg xs ys = S.svg xs ys |> E.html

svgRect : Bool -> Int -> Int -> E.Element msg
svgRect sel ln ht =
  elsvg
    [ SA.width <| String.fromInt ln
    , SA.height <| String.fromInt <| 10 * ht
    ]
    [ S.rect
        [ SA.width "100%" 
        , SA.height "100%"
        , SA.fill ( if sel then "orange" else "cornflowerblue" )
        ]
        []
    ]

graphBar : ViewGraphModel -> Int -> Int -> E.Element Msg
graphBar model col votes = 
  let
    sel = Just col == model.mouseoverBar
    addInfoBox = (\xs -> if sel then (E.below (graphBarTooltip model)) :: xs else xs)
  in
  E.el 
    (addInfoBox
      [ E.alignBottom 
      , EE.onMouseEnter <| ViewGraphMsg <| GraphBarMouseEnterMsg col
      , EE.onMouseLeave <| ViewGraphMsg GraphBarMouseLeaveMsg
      ] 
    )
    (svgRect sel 20 votes)

graphBarTooltip : ViewGraphModel -> E.Element msg
graphBarTooltip model =
  case (model.mouseoverBar, model.selectedPlayer) of
      (Just x, Just p) ->
        let
          voters = model.premierePoll.ballots
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



voteChart : ViewGraphModel -> E.Element Msg
voteChart model =
  let
    votes =
      case model.selectedPlayer of
          Just player ->
            Poll.collectPlayerVotes player model.premierePoll.ballots 
              |> List.map List.length
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

playerDropdown : Dropdown String -> E.Element Msg
playerDropdown dropdown = 
  Dropdown.label (E.text "Player") dropdown
    |> Dropdown.inputType Dropdown.TextField
    |> Dropdown.labelPlacement Left
    |> Dropdown.view PlayerDropdownMsg
    |> (E.map ViewGraphMsg)
