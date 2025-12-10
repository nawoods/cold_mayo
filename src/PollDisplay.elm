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
import Element.Font as Font
import Svg as S
import Svg.Attributes as SA

import Dropdown exposing (Dropdown, OutMsg(..), Placement(..))

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

type PollDisplaySubmsg =
  PlayerDropdownMsg (Dropdown.Msg String)
  | GraphBarMouseEnterMsg Int
  | GraphBarMouseLeaveMsg

update : PollDisplaySubmsg -> Model -> ( Model, Cmd PollDisplaySubmsg )
update subMsg viewGraphModel =
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

pollDisplayContent : Model -> E.Element PollDisplaySubmsg
pollDisplayContent model =
  E.column
    [ E.spacing 30 ]
    [ pollHeading model.premierePoll
    , playerDropdown model.playerDropdown
    , voteChart model
    , case model.mouseoverBar of
        Nothing ->
          E.link
            [ E.alignRight ]
            { url = "/"
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
    ++ Poll.disciplineToString poll.discipline
    ++ " Poll"
    )

voteChart : Model -> E.Element PollDisplaySubmsg
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

graphBar : Model -> Int -> Int -> E.Element PollDisplaySubmsg
graphBar model col votes = 
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
    (svgRect sel 20 votes)

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

playerDropdown : Dropdown String -> E.Element PollDisplaySubmsg
playerDropdown dropdown = 
  Dropdown.label (E.text "Player") dropdown
    |> Dropdown.inputType Dropdown.TextField
    |> Dropdown.labelPlacement Left
    |> Dropdown.view PlayerDropdownMsg

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
