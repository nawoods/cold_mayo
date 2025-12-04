module PremierePoll exposing (PremirePoll, Discipline, Ballot, collectPlayerVotes, parseBallotData, getPlayerNames)

import Set


type alias PremirePoll =
    { yearMonth : ( Int, Int )
    , discipline : Discipline
    , ballots : List Ballot
    }

type Discipline = 
  Open 
  | DAS 
  | Tap

type alias Ballot =
  {  voter : String
  ,  votes : List String
  }

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
