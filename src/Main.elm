module Main exposing (main)

import Browser
import Svg as S
import Svg.Attributes as SA
import Set

import Element as E
import Element.Events as EE
-- import Element.Background as Background
-- import Element.Border as Border
-- import Element.Font as Font
import Dropdown exposing (Dropdown, OutMsg(..), Placement(..))
import Html exposing (Html)

-- MAIN

main = 
  Browser.document { init = init, update = update, view = view, subscriptions = subscriptions }



-- MODEL

type alias Model =
  { selectedPlayer : Maybe String
  , playerDropdown : Dropdown String
  , ballots : List Ballot
  , mouseoverBar : Maybe Int
  }

type alias Ballot =
  {  voter : String
  ,  votes : List String
  }

init : () -> ( Model, Cmd Msg )
init _ = 
  (
    { selectedPlayer = Nothing
    , mouseoverBar = Nothing
    , ballots = rawBallotData |> parseBallotData
    , playerDropdown = Dropdown.init
        |> Dropdown.id "player-dropdown"
        |> Dropdown.inputType Dropdown.TextField
        |> Dropdown.stringOptions (rawBallotData |> parseBallotData |> getPlayerNames)
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
      ( { model | mouseoverBar = Nothing}, Cmd.none )
                    
          
    






-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
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
      [ E.centerX, E.spacing 40, E.padding 40 ]
      [ playerDropdown model
      , voteChart model
      ]
    )

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
        , SA.fill ( if sel then "darkorange" else "orange" )
        ]
        []
    ]

graphBar : Model -> Int -> Int -> E.Element Msg
graphBar model col ht = 
  let
    sel = Just col == model.mouseoverBar
  in
  E.el [ E.alignBottom 
       , EE.onMouseEnter (GraphBarMouseEnterMsg col)
       , EE.onMouseLeave GraphBarMouseLeaveMsg
       ] 
       (svgRect sel ht)

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
            collectPlayerVotes p model.ballots |> List.map List.length
  in
  E.column
    []
    [ E.row
      []
      (lineVert :: List.map2 (graphBar model) (List.range 1 25) votes)
    , lineHoriz
    ]
      

playerDropdown : Model -> E.Element Msg
playerDropdown model = 
  Dropdown.label (E.text "Player") model.playerDropdown
    |> Dropdown.inputType Dropdown.TextField
    |> Dropdown.labelPlacement Left
    |> Dropdown.view PlayerDropdownMsg

getPlayerNames : List Ballot -> List String
getPlayerNames = List.map (\l -> l.votes) >> List.concat >> Set.fromList >> Set.toList

collectPlayerVotes : String -> List Ballot -> List (List String)
collectPlayerVotes player ballots =
  let
    emptyPlayerVotes = List.repeat 25 []
  in
  List.foldl (\b -> addPlayerVote player b) emptyPlayerVotes ballots

addPlayerVote : String -> Ballot -> List (List String) -> List (List String)
addPlayerVote player ballot existingVotes =
  List.map2 (pushVoter ballot.voter player) ballot.votes existingVotes

pushVoter : String -> String -> String -> List String -> List String
pushVoter voter player vote existingVoters =
  if player == vote then voter :: existingVoters else existingVoters


parseBallotData : String -> List Ballot
parseBallotData = 
  String.lines 
    >> List.map (String.split ",") 
    >> List.filter (\l -> List.length l > 1)
    >> List.filterMap convertListToBallot

convertListToBallot : List String -> Maybe Ballot
convertListToBallot list =
  case list of
    [] ->
      Nothing
    x :: xs ->
      Just { voter = x
           , votes = xs
           }

rawBallotData : String
rawBallotData = """
3arrett,BLUE SCUTI,ALEX T,DOG,PIXELANDY,FRACTAL,MEME,TRISTOP,SIDNEV,IBALL,NIGHT,MYLES,COBRA,TETRISTIME,SV,NEK0,THEDENGLER,COAL,GERALD FREEMAN,HUFFULUFUGUS,DANV,AKATU,TUGI,GWAFEY,SODIUM,EVESY
AdamIrish,BLUE SCUTI,DOG,ALEX T,PIXELANDY,MEME,NIGHT,SV,TRISTOP,SIDNEV,IBALL,COBRA,NEK0,MYLES,HUFFULUFUGUS,COAL,AKATU,FRACTAL,TUGI,THEDENGLER,TETRISTIME,EVESY,DANV,SODIUM,O,ID_KIRBY
Andrew Meep,BLUE SCUTI,DOG,ALEX T,MEME,PIXELANDY,NIGHT,SIDNEV,TRISTOP,SV,IBALL,AKATU,MYLES,HUFFULUFUGUS,NEK0,COBRA,COAL,GWAFEY,TETRISTIME,EVESY,TUGI,DANV,CRYTICALAPIS,THEDENGLER,WINTERRA,ID_KIRBY
arbaro,BLUE SCUTI,DOG,ALEX T,MEME,PIXELANDY,SIDNEV,NIGHT,SV,TRISTOP,COBRA,NEK0,MYLES,IBALL,TETRISTIME,COAL,HUFFULUFUGUS,DANV,AKATU,SODIUM,TUGI,WALLBANT,O,EVESY,ID_KIRBY,THEDENGLER
Grand Designs,BLUE SCUTI,DOG,ALEX T,PIXELANDY,MEME,SIDNEV,NIGHT,TRISTOP,SV,AKATU,MYLES,IBALL,HUFFULUFUGUS,COAL,COBRA,EVESY,NEK0,TETRISTIME,TUGI,DANV,GWAFEY,THEDENGLER,WINTERRA,CRYTICALAPIS,WALLBANT
Gunter,BLUE SCUTI,DOG,ALEX T,PIXELANDY,MEME,NIGHT,NEK0,FRACTAL,IBALL,COBRA,SV,TRISTOP,MYLES,SIDNEV,TETRISTIME,DANV,TUGI,SODIUM,THEDENGLER,HUFFULUFUGUS,COAL,O,AKATU,EVESY,PHLOXAE
Hecate,BLUE SCUTI,DOG,ALEX T,PIXELANDY,MEME,NIGHT,SIDNEV,SV,AKATU,IBALL,COBRA,TRISTOP,MYLES,TUGI,NEK0,SODIUM,WALLBANT,COAL,HUFFULUFUGUS,TETRISTIME,THEDENGLER,WINTERRA,DANV,CRYTICALAPIS,O
Jakub,ALEX T,DOG,BLUE SCUTI,PIXELANDY,MEME,SIDNEV,TRISTOP,MYLES,IBALL,FRACTAL,NIGHT,COAL,COBRA,HUFFULUFUGUS,DANV,TETRISTIME,SV,THEDENGLER,GWAFEY,AKATU,ALLENBOT,SOMALIAN,EVESY,ID_KIRBY,SUNNY
Jay,ALEX T,BLUE SCUTI,DOG,MEME,TRISTOP,PIXELANDY,IBALL,NIGHT,NEK0,FRACTAL,COBRA,THEDENGLER,SV,MYLES,COAL,HUFFULUFUGUS,TETRISTIME,TUGI,SODIUM,AKATU,EVESY,WALLBANT,DANV,DMJ,MASY
Kingsman,DOG,ALEX T,BLUE SCUTI,PIXELANDY,MEME,IBALL,SV,FRACTAL,NEK0,SIDNEV,COBRA,TRISTOP,AKATU,THEDENGLER,NIGHT,MYLES,COAL,HUFFULUFUGUS,SODIUM,MASY,EVESY,DANV,WALLBANT,WINTERRA,HYDRANTDUDE
Marfram,ALEX T,BLUE SCUTI,DOG,MEME,PIXELANDY,NIGHT,SV,SIDNEV,AKATU,NEK0,MYLES,TRISTOP,IBALL,TETRISTIME,COBRA,TUGI,DANV,CRYTICALAPIS,SODIUM,WALLBANT,THEDENGLER,O,ID_KIRBY,WINTERRA,PEEKAYRIC
Mathwiz100,BLUE SCUTI,PIXELANDY,DOG,ALEX T,MEME,SV,COAL,COBRA,FRACTAL,NIGHT,AKATU,TUGI,SODIUM,MYLES,THEDENGLER,HUFFULUFUGUS,TRISTOP,IBALL,NEK0,TETRISTIME,CRYTICALAPIS,GWAFEY,MASY,EVESY,HYDRANTDUDE
pumpyheart,BLUE SCUTI,DOG,ALEX T,PIXELANDY,MEME,IBALL,NIGHT,TRISTOP,SIDNEV,HUFFULUFUGUS,SV,MYLES,NEK0,COAL,COBRA,AKATU,TETRISTIME,FRACTAL,TUGI,GWAFEY,THEDENGLER,EVESY,SODIUM,OPAUX,WALLBANT
Richard Wolf VI,ALEX T,DOG,PIXELANDY,BLUE SCUTI,MEME,MYLES,COBRA,SV,SIDNEV,NEK0,TRISTOP,IBALL,COAL,AKATU,THEDENGLER,HUFFULUFUGUS,NIGHT,DANV,GWAFEY,TETRISTIME,WALLBANT,TUGI,SODIUM,ID_KIRBY,HYDRANTDUDE
roncli,ALEX T,BLUE SCUTI,DOG,PIXELANDY,MEME,TRISTOP,IBALL,FRACTAL,NIGHT,MYLES,SV,SIDNEV,NEK0,THEDENGLER,DANV,HUFFULUFUGUS,SODIUM,COAL,COBRA,EVESY,TUGI,O,TETRISTIME,ID_KIRBY,HYDRANTDUDE
rxs,BLUE SCUTI,DOG,ALEX T,MEME,PIXELANDY,TRISTOP,SIDNEV,SV,NIGHT,IBALL,AKATU,COBRA,HUFFULUFUGUS,COAL,MYLES,NEK0,TETRISTIME,GWAFEY,TUGI,DANV,SODIUM,EVESY,WALLBANT,ID_KIRBY,CRYTICALAPIS
Ryan Amburgy,BLUE SCUTI,DOG,ALEX T,PIXELANDY,MEME,NIGHT,SIDNEV,TRISTOP,SV,AKATU,IBALL,MYLES,HUFFULUFUGUS,COAL,COBRA,NEK0,TETRISTIME,GWAFEY,TUGI,DANV,EVESY,CRYTICALAPIS,WINTERRA,ID_KIRBY,MASY
stolenshortsword,BLUE SCUTI,DOG,ALEX T,PIXELANDY,MEME,SIDNEV,NIGHT,TRISTOP,SV,COBRA,NEK0,IBALL,AKATU,HUFFULUFUGUS,MYLES,GWAFEY,COAL,TETRISTIME,TUGI,DANV,THEDENGLER,WINTERRA,SODIUM,WALLBANT,MASY
Thisinthat,BLUE SCUTI,DOG,ALEX T,PIXELANDY,MEME,TRISTOP,SV,IBALL,FRACTAL,NIGHT,NEK0,AKATU,COAL,COBRA,MYLES,TUGI,HUFFULUFUGUS,SODIUM,DANV,SIDNEV,GWAFEY,THEDENGLER,TETRISTIME,WALLBANT,CRYTICALAPIS
vandweller,DOG,BLUE SCUTI,ALEX T,PIXELANDY,MEME,TRISTOP,NIGHT,IBALL,MYLES,THEDENGLER,NEK0,COBRA,SIDNEV,COAL,SV,FRACTAL,TUGI,HUFFULUFUGUS,TETRISTIME,AKATU,DANV,EVESY,WALLBANT,SODIUM,ID_KIRBY
Wingfryer,DOG,BLUE SCUTI,MEME,ALEX T,TRISTOP,IBALL,FRACTAL,SV,MYLES,PIXELANDY,THEDENGLER,COBRA,NIGHT,TETRISTIME,COAL,TUGI,GWAFEY,WALLBANT,HUFFULUFUGUS,DANV,SODIUM,AKATU,NEK0,NOWI,EVESY
"""
