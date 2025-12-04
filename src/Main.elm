module Main exposing (main)

import Browser
import Html exposing (Html)

import Array

import Svg as S
import Svg.Attributes as SA
import Element as E
import Element.Events as EE
import Element.Font as Font

import Dropdown exposing (Dropdown, OutMsg(..), Placement(..))

import PremierePoll
import PremierePoll.Data

-- MAIN

main : Program () Model Msg
main = 
  Browser.document { init = init, update = update, view = view, subscriptions = subscriptions }



-- MODEL

type alias Model =
  { selectedPlayer : Maybe String
  , playerDropdown : Dropdown String
  , ballots : List PremierePoll.Ballot
  , mouseoverBar : Maybe Int
  }


init : () -> ( Model, Cmd Msg )
init _ = 
  (
    { selectedPlayer = Nothing
    , mouseoverBar = Nothing
    , ballots = PremierePoll.Data.rawBallotData |> PremierePoll.parseBallotData
    , playerDropdown = Dropdown.init
        |> Dropdown.id "player-dropdown"
        |> Dropdown.inputType Dropdown.TextField
        |> Dropdown.stringOptions (PremierePoll.Data.rawBallotData 
          |> PremierePoll.parseBallotData 
          |> PremierePoll.getPlayerNames)
        |> Dropdown.filterType Dropdown.StartsWithThenContains
    }
    , Cmd.none
  )




-- UPDATE 

type Msg = 
  PlayerDropdownMsg (Dropdown.Msg String)
  | GraphBarMouseEnterMsg Int
  | GraphBarMouseLeaveMsg

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
          [ E.column
            []
            [ E.el [Font.size 24] <| E.text "Cold Mayo"
            , E.el [Font.size 16] <| E.text "A delicious plant-based spread for Nestris enthusiasts"

            ]
          , playerDropdown model
          , voteChart model
          ]
       , E.el 
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
      ]
    )

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
  case (model.mouseoverBar, model.selectedPlayer) of
      (Just x, Just p) ->
        let
          voters = model.ballots
            |> PremierePoll.collectPlayerVotes p
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
              E.text "whoops"
      _ ->
        E.text "whoops"

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
      case model.selectedPlayer of
          Nothing ->
            List.repeat 25 0
          Just p ->
            PremierePoll.collectPlayerVotes p model.ballots |> List.map List.length
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
