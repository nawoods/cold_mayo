module Graph exposing
  ( Model
  , getMouseOverBar
  , init
  , Msg
  , update
  , view
  )
import PremierePoll as Poll
import Array

import Element as E
import Element.Events as EE
import Element.Font as Font
import Svg as S
import Svg.Attributes as SA
import PremierePoll exposing (PremierePoll)

-- MODEL

type Model = Model Poll.PremierePoll Player MouseOverBar


type alias MouseOverBar = Maybe Int

type alias Player = String

getPoll : Model -> PremierePoll
getPoll (Model poll _ _) = poll

getPlayer : Model -> Player
getPlayer (Model _ player _) = player

getMouseOverBar : Model -> MouseOverBar
getMouseOverBar (Model _ _ bar) = bar

setPoll : Poll.PremierePoll -> Model -> Model
setPoll poll model =
  Model poll (getPlayer model) (getMouseOverBar model)

setPlayer : Player -> Model -> Model
setPlayer player model =
  Model (getPoll model) player (getMouseOverBar model)

setMouseOverBar : MouseOverBar -> Model -> Model
setMouseOverBar bar model =
  Model (getPoll model) (getPlayer model) bar




init : Poll.PremierePoll -> String -> Model
init poll player = 
  Model poll player Nothing

  



-- UPDATE

type Msg =
  GraphBarMouseEnterMsg Bool Int
  | GraphBarMouseLeaveMsg

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    GraphBarMouseEnterMsg isNonzero bar ->
      if 
        isNonzero
      then
        ( setMouseOverBar (Just bar) model , Cmd.none )
      else
        ( setMouseOverBar Nothing model , Cmd.none )
    GraphBarMouseLeaveMsg ->
        ( setMouseOverBar Nothing model , Cmd.none )
    




-- VIEW

view : Model -> E.Element Msg
view model =
  E.column
    (graphBarHitboxes model)
    [ elsvg
      [ SA.width "505"
      , SA.height "255"
      ]
      <| List.append
          (graphBars model)
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
    , xAxisLabel model
    ]

xAxisLabel : Model -> E.Element Msg
xAxisLabel model =
  case (getMouseOverBar model) of
    Nothing ->
      let
        poll = getPoll model
        numberofPlayers = Poll.numberOfPlayers poll
      in
      E.row
       [ E.paddingXY 5 5, E.width E.fill ]
       [ E.el [ E.alignLeft ] <| E.text "1"
       , E.el [ E.centerX ] <| E.text "Rank"
       , E.el [ E.alignRight ] <| E.text (String.fromInt numberofPlayers)
       ]
    _ ->
      E.none

graphBars : Model -> List (S.Svg Msg)
graphBars model =
  let
    poll = getPoll model
    player = getPlayer model
    bar = getMouseOverBar model
    numberofPlayers = Poll.numberOfPlayers poll
    votes =
      Poll.collectPlayerVotes player poll.ballots 
        |> List.map List.length
    barWidth = 500 // numberofPlayers
  in
  (List.indexedMap (graphBar bar barWidth) votes)
  

graphBar : Maybe Int -> Int -> Int -> Int -> S.Svg msg
graphBar bar width index votes =
  S.rect
    [ SA.x <| String.fromInt (5 + (index * width))
    , SA.y <| String.fromInt (250 - (votes * 10))
    , SA.height <| String.fromInt (votes * 10)
    , SA.width <| String.fromInt width
    , SA.fill <| if Just (index + 1) == bar then "orange" else "cornflowerblue"
    ]
    []

graphBarHitboxes : Model -> List (E.Attribute Msg)
graphBarHitboxes model =
      let
        poll = getPoll model
        player = getPlayer model
        numberofPlayers = Poll.numberOfPlayers poll
        votes =
          Poll.collectPlayerVotes player poll.ballots 
            |> List.map List.length
        barWidth = 500 // numberofPlayers
      in
      List.indexedMap (graphBarHitbox model barWidth 250) votes
        |> E.row [ E.paddingEach { edges | right = 5 } ]
        |> E.inFront
        |> List.singleton


graphBarHitbox : Model -> Int -> Int -> Int -> Int -> E.Element Msg
graphBarHitbox model width height index votes =
  let
    bar = getMouseOverBar model
  in
    E.el
      [ E.width <| E.px width
      , E.height <| E.px height
      , EE.onMouseEnter <| GraphBarMouseEnterMsg (votes /= 0) (index + 1)
      , EE.onMouseLeave GraphBarMouseLeaveMsg
      , E.below <| 
          if
            bar == Just (index + 1)
          then 
            graphBarTooltip model
          else
            E.none
      ]
      E.none

graphBarTooltip : Model -> E.Element msg
graphBarTooltip model =
  let
    bar = getMouseOverBar model
    poll = getPoll model
    player = getPlayer model
  in
  case bar of
      Just x ->
        let
          voters = poll.ballots
            |> Poll.collectPlayerVotes player
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
  
edges : { top : number, right : number, left : number, bottom : number }
edges = 
 { top = 0
 , right = 0
 , left = 0
 , bottom = 0
 }

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