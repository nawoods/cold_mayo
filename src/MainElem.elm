module MainElem exposing (..)
import Browser
import PollDisplay
import Html
import PremierePoll.Data
import Element as E

main : Program String Model Msg
main = Browser.element 
  { init = init
  , update = update
  , subscriptions = subscriptions
  , view = view
  }


type Model
  = Success PollDisplay.Model
  | Failure

init : a -> (Model, Cmd Msg)
init _ = 
  ( Success (PollDisplay.init PremierePoll.Data.samplePoll)
  , Cmd.none
  )




type Msg
  = FailureMsg
  | PollDisplayMsg PollDisplay.Msg

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    FailureMsg ->
      ( Failure, Cmd.none )
    PollDisplayMsg subMsg ->
      case model of
        Failure ->
          ( Failure, Cmd.none )
        Success prevPollDisplayModel ->
          let
            ( pollDisplayModel, pollDisplayCmd ) = PollDisplay.update subMsg prevPollDisplayModel
          in
            ( Success pollDisplayModel
            , Cmd.map PollDisplayMsg pollDisplayCmd
            )





subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none



view : Model -> Html.Html Msg
view model =
  case model of
    Failure ->
      Html.div [] [Html.text "whoops"]
    Success pollDisplayModel ->
      pollDisplayModel
      |> PollDisplay.view 
      |> (E.map PollDisplayMsg)
      |> (E.layout [])
