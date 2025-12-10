module SelectionFlow exposing 
  ( Model(..)
  , SelectionFlowSubmsg
  , update
  , selectionPollContent
  )

import PremierePoll as Poll
import Element as E
import Element.Input as EI

import PremierePoll.Data as PollData


type Model =
  SelectDiscipline
  | SelectYear
      { selectedDiscipline : Poll.Discipline
      }
  | SelectMonth
      { selectedDiscipline : Poll.Discipline  
      , selectedYear : Int
      }

type SelectionFlowSubmsg =
  DisciplineSelectMsg Poll.Discipline
  | YearSelectMsg Int

update : SelectionFlowSubmsg -> Model -> Model
update subMsg selectPollStatus =
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

selectionPollContent : Model -> E.Element SelectionFlowSubmsg
selectionPollContent model =
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

disciplineButtonRow : E.Element SelectionFlowSubmsg
disciplineButtonRow =
  E.row
  [ E.spacing 20 ]
  ( [Poll.Open, Poll.Das, Poll.Tap]
      |> List.map (\d -> disciplineButton d (Poll.disciplineToString d))
      |> List.intersperse (E.text "|")
  )

disciplineButton : Poll.Discipline -> String -> E.Element SelectionFlowSubmsg
disciplineButton discipline label =
  EI.button
  []
  { onPress = Just (DisciplineSelectMsg discipline)
  , label = E.text label
  }


yearButtonRow : Poll.Discipline -> E.Element SelectionFlowSubmsg
yearButtonRow discipline =
  E.wrappedRow
  [ E.spacingXY 20 0 ]
  ( PollData.polls
    |> Poll.getYearsWithDiscipline discipline
    |> List.map yearButton
    |> List.intersperse (E.text "|")
  )

yearButton : Int -> E.Element SelectionFlowSubmsg
yearButton year =
  EI.button
  []
  { onPress = Just (YearSelectMsg year)
  , label = E.text (String.fromInt year)
  }

monthButtonRow : Poll.Discipline -> Int -> E.Element SelectionFlowSubmsg
monthButtonRow discipline year =
  E.wrappedRow
  [ E.spacingXY 20 0 ]
  ( PollData.polls
    |> Poll.getMonthsWithDisciplineAndYear discipline year
    |> List.map (monthButton discipline year)
    |> List.intersperse (E.text "|")
  )

monthButton : Poll.Discipline -> Int -> Int -> E.Element SelectionFlowSubmsg
monthButton discipline year month =
  let
    monthText = Poll.getLongMonthName month
    label =
      case monthText of
        Just a ->
          a
        Nothing ->
         "?"
    disciplineText = discipline |> Poll.disciplineToString |> String.toLower
  in
  E.link
  []
  { url = "/poll/" ++ disciplineText ++ "/" ++ String.fromInt year ++ "/" ++ String.fromInt month ++ "/"
  , label = E.text label
  }