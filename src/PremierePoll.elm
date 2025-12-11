module PremierePoll exposing
  ( PremierePoll
  , Discipline (..)
  , Ballot
  , PlayerRanking
  , findPoll
  , disciplineToString
  , disciplineFromString
  , collectPlayerVotes
  , computePlayerPoints
  , parseBallotData
  , getPlayerNames
  , getLongMonthName
  , numberOfPlayers
  , rankPlayers
  , getYearsWithDiscipline
  , getMonthsWithDisciplineAndYear
  , toUrlString
  )

import Set
import Array


type alias PremierePoll =
    { year : Int
    , month : Int
    , discipline : Discipline
    , ballots : List Ballot
    }

type Discipline = 
  Open 
  | Das 
  | Tap

type alias Ballot =
  {  voter : String
  ,  votes : List String
  }

type alias PlayerRanking =
  { player: String
  , rank: Int
  , points: Int
  }

findPoll : Discipline -> Int -> Int -> List PremierePoll -> Maybe PremierePoll
findPoll discipline year month polls =
  polls
    |> List.filter 
      (\p -> p.discipline == discipline
      && p.year == year
      && p.month == month
      )
    |> List.head

  

numberOfPlayers : PremierePoll -> Int
numberOfPlayers =
  .ballots
  >> maxPlayersInBallots

maxPlayersInBallots : List Ballot -> Int
maxPlayersInBallots =
    List.map (\b -> List.length b.votes)
    >> List.maximum
    >> Maybe.withDefault 25

disciplineToString : Discipline -> String
disciplineToString discipline =
  case discipline of
    Open -> 
      "Open"
    Das ->
      "DAS"
    Tap ->
      "Tap"

disciplineFromString : String -> Maybe Discipline
disciplineFromString str =
  [Open, Das, Tap]
    |> List.filter 
        (\d -> String.toLower (disciplineToString d) == String.toLower str)
    |> List.head

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

computePlayerPoints : String -> PremierePoll -> Int
computePlayerPoints player poll = 
  (collectPlayerVotes player poll.ballots)
    |> List.indexedMap (\i v -> (numberOfPlayers poll - i, List.length v))
    |> List.foldl (\(pts, votes) acc -> acc + (pts * votes)) 0

rankPlayers : PremierePoll -> List PlayerRanking
rankPlayers poll =
  let
    names = getPlayerNames poll.ballots
  in
  List.map (\n -> computePlayerPoints n poll) names
    |> List.map2 Tuple.pair names 
    |> List.sortBy Tuple.second
    |> List.reverse
    |> List.map2 
         (\rank (player, points) -> { rank = rank, player = player, points = points})
         (List.range 1 (List.length names))

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

getYearsWithDiscipline : Discipline -> List PremierePoll -> List Int
getYearsWithDiscipline discipline polls =
  polls
    |> List.filter (\p -> p.discipline == discipline)
    |> List.map .year
    |> Set.fromList
    |> Set.toList

getMonthsWithDisciplineAndYear : Discipline -> Int -> List PremierePoll -> List Int
getMonthsWithDisciplineAndYear discipline year polls =
  polls
    |> List.filter (\p -> p.discipline == discipline && p.year == year)
    |> List.map .month
    |> Set.fromList
    |> Set.toList


longMonthNames : Array.Array String
longMonthNames =
  Array.fromList
    [ "January"
    , "February"
    , "March"
    , "April"
    , "May"
    , "June"
    , "July"
    , "August"
    , "September"
    , "October"
    , "November"
    , "December"
    ]

getLongMonthName : Int -> Maybe String
getLongMonthName monthNumber =
  if 
    monthNumber > 0 && monthNumber <= 12
  then
    Array.get (monthNumber - 1) longMonthNames
  else
    Nothing
      
toUrlString : PremierePoll -> String
toUrlString poll =
  "/" ++ (disciplineToString poll.discipline) 
      ++ "/" 
      ++ (String.fromInt poll.year)
      ++ "/"
      ++ (String.fromInt poll.month)
      ++ "/"