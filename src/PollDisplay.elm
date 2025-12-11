module PollDisplay exposing 
  ( Model
  , Msg
  , init
  , update
  , pollDisplayContent
  )
import PremierePoll as Poll

import Array

import Element as E
import Element.Events as EE
import Element.Input as EI
import Element.Font as Font
import Svg as S
import Svg.Attributes as SA

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
  , mouseoverBar : Maybe Int
  }

init : Poll.PremierePoll -> Model
init p = 
  ViewingPoll
    { poll = p 
    , mouseoverPlayer = Nothing
    }
          

type Msg
  = ChoosePlayerMsg String
  | ClearPlayerMsg
  | GraphBarMouseEnterMsg Bool Int
  | GraphBarMouseLeaveMsg
  | PlayerMouseEnterMsg String
  | PlayerMouseLeaveMsg

update : Msg -> Model -> ( Model, Cmd Msg )
update subMsg model =
  case subMsg of
    GraphBarMouseEnterMsg bool bar ->
      case model of
        ViewingPlayer viewingPlayerModel ->
          ( ViewingPlayer
            { viewingPlayerModel 
                | mouseoverBar = if bool then Just bar else Nothing 
            }
          , Cmd.none 
          )
        _ ->
          ( model, Cmd.none )
    GraphBarMouseLeaveMsg ->
      case model of
        ViewingPlayer viewingPlayerModel ->
          ( ViewingPlayer
            { viewingPlayerModel 
                | mouseoverBar = Nothing
            }
          , Cmd.none 
          )
        _ ->
          ( model, Cmd.none )
    ChoosePlayerMsg player ->
      case model of
        ViewingPoll viewingPollModel ->
          ( ViewingPlayer
              { poll = viewingPollModel.poll
              , player = player
              , mouseoverBar = Nothing
              }
          , Cmd.none )
        ViewingPlayer viewingPlayerModel ->
          ( ViewingPlayer
              { viewingPlayerModel
                  | player = player
              }
          , Cmd.none
          )
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
      

pollDisplayContent : Model -> E.Element Msg
pollDisplayContent model =
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
    [ voteChart model
    , case model.mouseoverBar of
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
    poll =
      case model of
        ViewingPoll viewingPollModel ->
          viewingPollModel.poll
        ViewingPlayer viewingPlayerModel ->
          viewingPlayerModel.poll
          
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
        (  (Maybe.withDefault "?" (Poll.getLongMonthName poll.month) )
        ++ " "
        ++ String.fromInt poll.year
        ++ " "
        ++ Poll.disciplineToString poll.discipline
        ++ " Poll"
        )
    ]

voteChart : ViewingPlayerModel -> E.Element Msg
voteChart model =
  let
    numberofPlayers = Poll.numberOfPlayers model.poll
    votes =
      Poll.collectPlayerVotes model.player model.poll.ballots 
        |> List.map List.length
    barWidth = 500 // numberofPlayers
  in
  E.column
    [ E.inFront
       <| E.row
          [ E.paddingEach { edges | right = 5 } ]
          <| List.indexedMap (graphBarHitbox model barWidth 250) votes
    ]
    [ elsvg
      [ SA.width "505"
      , SA.height "255"
      ]
      <| List.append
          (List.indexedMap (graphBar model barWidth) votes)
          [ S.rect
            [ SA.width "5" 
            , SA.height "100%"
            , SA.fill "black"
            ]
            []
          , S.rect
            [ SA.x "0"
            , SA.y "250"
            , SA.width "100%"
            , SA.height "5"
            ]
            []
          ]
    , case model.mouseoverBar of
        Nothing ->
          E.row
           [ E.paddingXY 5 5, E.width E.fill ]
           [ E.el [ E.alignLeft ] <| E.text "1"
           , E.el [ E.centerX ] <| E.text "Rank"
           , E.el [ E.alignRight ] <| E.text (String.fromInt numberofPlayers)
           ]
        _ ->
          E.none
    ]

graphBar : ViewingPlayerModel -> Int -> Int -> Int -> S.Svg msg
graphBar model width index votes =
  S.rect
    [ SA.x <| String.fromInt (5 + (index * width))
    , SA.y <| String.fromInt (250 - (votes * 10))
    , SA.height <| String.fromInt (votes * 10)
    , SA.width <| String.fromInt width
    , SA.fill <| if Just (index + 1) == model.mouseoverBar then "orange" else "cornflowerblue"
    ]
    []

graphBarHitbox : ViewingPlayerModel -> Int -> Int -> Int -> Int -> E.Element Msg
graphBarHitbox model width height index votes =
  E.el
    [ E.width <| E.px width
    , E.height <| E.px height
    , EE.onMouseEnter <| GraphBarMouseEnterMsg (votes /= 0) (index + 1)
    , EE.onMouseLeave GraphBarMouseLeaveMsg
    , E.below <| 
        if
          model.mouseoverBar == Just (index + 1)
        then 
          graphBarTooltip model
        else
          E.none
    ]
    E.none

graphBarTooltip : ViewingPlayerModel -> E.Element msg
graphBarTooltip model =
  case model.mouseoverBar of
      Just x ->
        let
          voters = model.poll.ballots
            |> Poll.collectPlayerVotes model.player
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
                  ( v |> List.sort
                      |> List.map (\a -> E.text a) 
                  )
                ]
            _ ->
              E.none
      _ ->
        E.none

elsvg : List (S.Attribute msg) -> List (S.Svg msg) -> E.Element msg
elsvg xs ys = S.svg xs ys |> E.html
      
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

edges : { top : number, right : number, left : number, bottom : number }
edges = 
 { top = 0
 , right = 0
 , left = 0
 , bottom = 0
 }