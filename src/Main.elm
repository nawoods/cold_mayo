module Main exposing (main)

import Browser
import Css as C
import Html.Styled as H
import Html.Styled.Attributes as HA
import Html.Styled.Events as HE
import Svg.Styled as S
import Svg.Styled.Attributes as SA
import Set

main = 
  Browser.sandbox { init = init, update = update, view = view >> H.toUnstyled }

type Msg = SelectPlayer String

update : Msg -> Model -> Model
update msg model = 
  case msg of
    SelectPlayer player ->
      { model | selectedPlayer = Just player }



type alias Model =
  { selectedPlayer : Maybe String
  , ballots : List Ballot
  }

type alias Ballot =
  {  voter : String
  ,  votes : List String
  }

init : Model
init = 
  { selectedPlayer = Nothing
  , ballots = rawBallotData |> parseBallotData
  }



svgRect : Int -> S.Svg msg
svgRect ht =
  S.svg
    [ SA.width "20"
    , SA.height <| String.fromInt <| 10 * ht
    ]
    [ S.rect
        [ SA.width "100%" 
        , SA.height "100%"
        , SA.fill "orange"
        ]
        []
    ]

lineVert : S.Svg msg
lineVert = 
  S.svg
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

lineHoriz : S.Svg msg
lineHoriz =
  S.svg
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


-- votes : List Int
-- votes =
--   [ 0, 0, 0, 0, 0
--   , 1, 4, 5, 4, 0
--   , 3, 0, 1, 1, 1
--   , 0, 1, 0, 0, 0
--   , 0, 0, 0, 0, 0
--   ]

view: Model -> H.Html Msg
view model =
  H.div 
    []
    [ voteChart model
    , playerDropdown model.ballots
    ]

voteChart : Model -> H.Html Msg
voteChart model =
  let
    votes =
      case model.selectedPlayer of
          Nothing ->
            List.repeat 25 0
          Just p ->
            collectPlayerVotes p model.ballots |> List.map List.length
  in
  H.div 
    [ HA.css 
      [ C.displayFlex 
      , C.flexDirection C.column
      , C.alignItems C.center
      ] 
    ]
    [ H.div 
      [ HA.css
        [ C.displayFlex
        , C.alignItems C.flexEnd
        ]
      ]
      <| lineVert :: List.map svgRect votes
    , lineHoriz
    ]

playerDropdown : List Ballot -> H.Html Msg
playerDropdown ballots =
  let 
    players = getPlayerNames ballots
  in
  H.select
    [ HE.onInput SelectPlayer ]
    <| List.map (\p -> H.option [] [ H.text p ]) players

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
