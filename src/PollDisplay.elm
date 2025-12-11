module PollDisplay exposing 
  ( Model
  , PollDisplaySubmsg
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

import Dropdown exposing (Dropdown, OutMsg(..), Placement(..))
import PremierePoll
import Element.Border as Border

type alias Model =
      { premierePoll: Poll.PremierePoll
      , playerDropdown : Dropdown String
      , selectedPlayer : Maybe String
      , mouseoverBar : Maybe Int
      }

init : Poll.PremierePoll -> Model
init p = 
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

type PollDisplaySubmsg
  = ChoosePlayerMsg String
  | ClearPlayerMsg
  | GraphBarMouseEnterMsg Bool Int
  | GraphBarMouseLeaveMsg

update : PollDisplaySubmsg -> Model -> ( Model, Cmd PollDisplaySubmsg )
update subMsg viewGraphModel =
  case subMsg of
    GraphBarMouseEnterMsg bool bar ->
      ( { viewGraphModel 
            | mouseoverBar = if bool then Just bar else Nothing 
        }
      , Cmd.none 
      )
    GraphBarMouseLeaveMsg ->
      ( { viewGraphModel | mouseoverBar = Nothing }, Cmd.none )
    ChoosePlayerMsg player ->
      ( { viewGraphModel | selectedPlayer = Just player }, Cmd.none )
    ClearPlayerMsg ->
      ( { viewGraphModel | selectedPlayer = Nothing }, Cmd.none )

pollDisplayContent : Model -> E.Element PollDisplaySubmsg
pollDisplayContent model =
  E.column
    [ E.spacing 30 
    , E.width E.fill
    ]
    [ pollHeading model
    , case model.selectedPlayer of
        Nothing ->
          rankingsDisplay model
        Just _ ->
          playerDisplay model
    ]

rankingsDisplay : Model -> E.Element PollDisplaySubmsg
rankingsDisplay model =
  let
    rankedPlayers = PremierePoll.rankPlayers model.premierePoll
    cutoff = PremierePoll.numberOfPlayers model.premierePoll
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
          :: (List.map (rankingsDisplayRow cutoff) rankedPlayers)

    , pollDisplayBottomButtons model
    ]

rankingsDisplayRow : Int -> PremierePoll.PlayerRanking -> E.Element PollDisplaySubmsg
rankingsDisplayRow cutoff p =
  let
    bottomBorderWidth = if p.rank == cutoff then 1 else 0
    topPadding = if p.rank == cutoff + 1 then 3 else 0
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
            ] 
            <| E.text p.player
          , E.el 
            [ E.width (E.px 100)
            , Font.center 
            ] 
            <| E.text (String.fromInt p.points)
          ]
    }

playerDisplay : Model -> E.Element PollDisplaySubmsg
playerDisplay model =
  E.column
    [ E.spacing 30 ]
    [ voteChart model
    , case model.mouseoverBar of
        Nothing ->
          pollDisplayBottomButtons model
        _ ->
          E.none
    ]

pollDisplayBottomButtons : Model -> E.Element PollDisplaySubmsg
pollDisplayBottomButtons model =
  let
    instructionsText = 
      case model.selectedPlayer of
        Just _ ->
          "Hover over graph to see votes from individual pollsters"
        Nothing ->
          "Click player name to see vote distribution"
  in
  E.column
    [ E.alignRight ]
    [ E.el
        [ Font.italic 
        , Font.size 14
        , E.paddingEach { edges | bottom = 5 }
        ]
        <| E.text instructionsText
    , case model.selectedPlayer of 
        Just _ ->
          EI.button
            [ E.alignRight ]
            { onPress = Just ClearPlayerMsg
            , label = E.text "Choose another player ->"
            }
        Nothing ->
          E.none
    , E.link
        [ E.alignRight ]
        { url = "/"
        , label = E.text "Choose another poll ->"
        }

    ]


pollHeading : Model -> E.Element msg
pollHeading model =
  let
    monthText =
      Maybe.withDefault "?" (Poll.getLongMonthName model.premierePoll.month)
  in
  E.column
    [ E.width E.fill ]
    [ E.el
        [ Font.size 30 ]
        <| case model.selectedPlayer of
            Just p ->
              E.text p
            Nothing ->
              E.none
    , E.text
        (  monthText
        ++ " "
        ++ String.fromInt model.premierePoll.year
        ++ " "
        ++ Poll.disciplineToString model.premierePoll.discipline
        ++ " Poll"
        )
    ]

voteChart : Model -> E.Element PollDisplaySubmsg
voteChart model =
  let
    numberofPlayers = Poll.numberOfPlayers model.premierePoll
    votes =
      case model.selectedPlayer of
          Just player ->
            Poll.collectPlayerVotes player model.premierePoll.ballots 
              |> List.map List.length
          _ ->
            List.repeat numberofPlayers 0
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

graphBar : Model -> Int -> Int -> Int -> S.Svg msg
graphBar model width index votes =
  S.rect
    [ SA.x <| String.fromInt (5 + (index * width))
    , SA.y <| String.fromInt (250 - (votes * 10))
    , SA.height <| String.fromInt (votes * 10)
    , SA.width <| String.fromInt width
    , SA.fill <| if Just (index + 1) == model.mouseoverBar then "orange" else "cornflowerblue"
    ]
    []

graphBarHitbox : Model -> Int -> Int -> Int -> Int -> E.Element PollDisplaySubmsg
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

graphBarTooltip : Model -> E.Element msg
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
