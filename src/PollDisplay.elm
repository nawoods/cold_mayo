module PollDisplay exposing 
  ( Model
  , Msg
  , init
  , update
  , view
  )
import PremierePoll as Poll
import Graph

import Element as E
import Element.Events as EE
import Element.Input as EI
import Element.Font as Font

import Colors.Opaque exposing (cornflowerblue, black)

import PremierePoll
import Element.Border as Border

type Model
  = ViewingPoll ViewingPollModel
  | ViewingPlayer ViewingPlayerModel

type alias ViewingPollModel =
  { poll : Poll.PremierePoll
  , mouseoverPlayer : Maybe String
  }

type alias ViewingPlayerModel =
  { poll : Poll.PremierePoll
  , player : String
  , graph : Graph.Model
  }

init : Poll.PremierePoll -> Model
init p = 
  ViewingPoll
    { poll = p 
    , mouseoverPlayer = Nothing
    }

getPoll : Model -> Poll.PremierePoll
getPoll model =
  case model of
    ViewingPoll viewingPollModel ->
      viewingPollModel.poll
    ViewingPlayer viewingPlayerModel ->
      viewingPlayerModel.poll

getPlayer : ViewingPlayerModel -> String
getPlayer model = model.player

getGraph : ViewingPlayerModel -> Graph.Model
getGraph model = model.graph

setGraph : Graph.Model -> ViewingPlayerModel -> ViewingPlayerModel
setGraph graph model = { model | graph = graph }

getMouseOverBar : ViewingPlayerModel -> Maybe Int
getMouseOverBar model =
  model
    |> getGraph
    |> Graph.getMouseOverBar
          

type Msg
  = ChoosePlayerMsg String
  | ClearPlayerMsg
  | PlayerMouseEnterMsg String
  | PlayerMouseLeaveMsg
  | GraphMsg Graph.Msg

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  let
    poll = getPoll model
  in
  case msg of
    ChoosePlayerMsg player ->
      ( ViewingPlayer
          { poll = poll
          , player = player
          , graph = Graph.init poll player
          }
      , Cmd.none )
    ClearPlayerMsg ->
      case model of
        ViewingPlayer viewingPlayerModel ->
          ( init viewingPlayerModel.poll, Cmd.none )
        _ ->
          ( model, Cmd.none )
    PlayerMouseEnterMsg player ->
      case model of
        ViewingPoll viewingPollModel ->
          ( ViewingPoll
              { viewingPollModel
                  | mouseoverPlayer = Just player
              }
          , Cmd.none
          )
        _ ->
          ( model, Cmd.none )
    PlayerMouseLeaveMsg ->
      case model of
        ViewingPoll viewingPollModel ->
          ( ViewingPoll
              { viewingPollModel
                  | mouseoverPlayer = Nothing
              }
          , Cmd.none
          )
        _ ->
          ( model, Cmd.none )
    GraphMsg subMsg ->
      case model of
        ViewingPlayer viewingPlayerModel ->
          let
            ( graphModel, graphCmd ) = Graph.update subMsg (getGraph viewingPlayerModel)
          in
            ( ViewingPlayer (setGraph graphModel viewingPlayerModel)
            , Cmd.map GraphMsg graphCmd
            )
          
        _ ->
          ( model, Cmd.none )
      

view : Model -> E.Element Msg
view model =
  E.column
    [ E.spacing 30 
    , E.width E.fill
    ]
    [ pollHeading model
    , case model of
        ViewingPoll viewingPollModel ->
          rankingsDisplay viewingPollModel
        ViewingPlayer viewingPlayerModel ->
          playerDisplay viewingPlayerModel
    ]

rankingsDisplay : ViewingPollModel -> E.Element Msg
rankingsDisplay model =
  let
    rankedPlayers = PremierePoll.rankPlayers model.poll
  in
  E.column
    [ E.spacing 50 
    , E.width E.fill
    ]
    [ E.column
      [ E.width E.fill ]
      <|  E.row
            [ E.paddingEach { edges | bottom = 5 } ]
            [ E.el
              [ E.paddingEach { edges | left = 40 } 
              , E.width (E.px 220)
              ]
              (E.text "Player")
            , E.el
              [ Font.center
              , E.width (E.px 100)
              ]
              (E.text "Points")

            ]
          :: (List.map (rankingsDisplayRow model) rankedPlayers)
    , pollDisplayBottomButtons
        "Click player name to see vote distribution"
        [ chooseAnotherPoll ]
    ]

rankingsDisplayRow : ViewingPollModel -> PremierePoll.PlayerRanking -> E.Element Msg
rankingsDisplayRow model p =
  let
    cutoff = PremierePoll.numberOfPlayers model.poll
    bottomBorderWidth = if p.rank == cutoff then 1 else 0
    topPadding = if p.rank == cutoff + 1 then 3 else 0
    playerFontColor =
      if model.mouseoverPlayer == Just p.player
      then
        cornflowerblue
      else
        black
  in
  EI.button
    []
    { onPress = Just (ChoosePlayerMsg p.player)
    , label =
        E.row
          [ E.width E.fill
          , Border.dashed
          , Border.widthEach { edges | bottom = bottomBorderWidth }
          , E.paddingEach { edges | top = topPadding }
          , EE.onMouseEnter (PlayerMouseEnterMsg p.player)
          , EE.onMouseLeave PlayerMouseLeaveMsg
          ]
          [ E.el 
            [ E.width (E.px 20)
            , Font.alignRight 
            ] 
            <|  if 
                  p.rank <= cutoff 
                then 
                  E.text (String.fromInt p.rank) 
                else 
                  E.none
          , E.el 
            [ E.paddingEach { edges | left = 20 } 
            , E.width (E.px 200)
            , Font.color playerFontColor
            ] 
            <| E.text p.player
          , E.el 
            [ E.width (E.px 100)
            , Font.center 
            ] 
            <| E.text (String.fromInt p.points)
          ]
    }

playerDisplay : ViewingPlayerModel -> E.Element Msg
playerDisplay model =
  E.column
    [ E.spacing 30 ]
    [ model
        |> getGraph
        |> Graph.view
        |> E.map GraphMsg
    , case (getMouseOverBar model) of
        Nothing ->
          pollDisplayBottomButtons
            "Hover over graph to see votes from individual pollsters"
            [ chooseAnotherPlayer
            , chooseAnotherPoll
            ]
        _ ->
          E.none
    ]

chooseAnotherPlayer : E.Element Msg
chooseAnotherPlayer =
  EI.button
    [ E.alignRight ]
    { onPress = Just ClearPlayerMsg
    , label = E.text "Choose another player ->"
    }

chooseAnotherPoll : E.Element msg
chooseAnotherPoll =
  E.link
    [ E.alignRight ]
    { url = "/"
    , label = E.text "Choose another poll ->"
    }

pollDisplayBottomButtons : String -> List (E.Element Msg) -> E.Element Msg
pollDisplayBottomButtons instructions buttons =
  E.column
    [ E.alignRight ]
    <|  ( E.el
          [ Font.italic 
          , Font.size 14
          , E.paddingEach { edges | bottom = 5 }
          ]
          <| E.el
              [ Font.color cornflowerblue ]
              ( E.text instructions )
        )
          :: buttons


pollHeading : Model -> E.Element msg
pollHeading model =
  let
    poll = getPoll model
    month = poll |> .month |> Poll.getLongMonthName
  in
  E.column
    [ E.width E.fill ]
    [ E.el
        [ Font.size 30 ]
        <| case model of
            ViewingPlayer viewingPlayerModel ->
              E.text viewingPlayerModel.player
            _ ->
              E.none
    , E.text
        (  ( Maybe.withDefault "?" month )
        ++ " "
        ++ String.fromInt poll.year
        ++ " "
        ++ Poll.disciplineToString poll.discipline
        ++ " Poll"
        )
    ]




edges : { top : number, right : number, left : number, bottom : number }
edges = 
 { top = 0
 , right = 0
 , left = 0
 , bottom = 0
 }