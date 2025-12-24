module Main exposing (main)

import Browser
import Browser.Navigation exposing (Key)
import Html exposing (Html)
import Url
import Url.Parser as P exposing ((</>))

import Element as E
import Element.Font as Font
import Element.Background as Background
import Colors.Opaque exposing (orange, cornflowerblue, white)

import PremierePoll as Poll
import PremierePoll.Data as PollData
import PollDisplay
import SelectionFlow

-- MAIN

main : Program () Model Msg
main = 
  Browser.application 
    { init = init
    , update = update
    , view = view
    , subscriptions = subscriptions 
    , onUrlChange = UrlChanged
    , onUrlRequest = LinkClicked
    }



-- MODEL

type alias Model =
  { appStatus: AppStatus
  , key: Key
  , url: Url.Url
  }

type AppStatus =
  SelectionFlow SelectionFlow.Model
  | PollDisplay PollDisplay.Model
  | Error String



init : () -> Url.Url -> Key -> ( Model, Cmd Msg )
init _ url key = 
  (
    { appStatus = appStatusFromUrl url
    , url = url
    , key = key
    }
    , Cmd.none
  )




-- UPDATE 

type Msg = 
  SelectionFlowMsg SelectionFlow.SelectionFlowSubmsg
  | PollDisplayMsg PollDisplay.Msg
  | ErrorMsg String
  | LinkClicked Browser.UrlRequest
  | UrlChanged Url.Url


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = 
  case msg of
    SelectionFlowMsg subMsg ->
      case model.appStatus of
        SelectionFlow selectionFlowStatus ->
          let
            newSelectPollStatus = SelectionFlow.update subMsg selectionFlowStatus
          in
          ( { model
               | appStatus = SelectionFlow newSelectPollStatus
            }
          , Cmd.none
          )
        _ ->
          ( model, Cmd.none )
    PollDisplayMsg subMsg ->
      case model.appStatus of
        PollDisplay pollDisplayStatus ->
          let
            ( newViewGraphStatus, cmd ) = PollDisplay.update subMsg pollDisplayStatus
          in
          ( { model
               | appStatus = PollDisplay newViewGraphStatus 
            }
          , Cmd.map PollDisplayMsg cmd )
        _ ->
          ( model, Cmd.none )
    LinkClicked urlRequest ->
      case urlRequest of
        Browser.Internal url ->
          ( model, Browser.Navigation.pushUrl model.key (Url.toString url) )
        Browser.External href ->
          ( model, Browser.Navigation.load href )
    UrlChanged url ->
      ( { model
           | appStatus = appStatusFromUrl url
           , url = url
        }
      , Cmd.none -- Browser.Navigation.replaceUrl model.key (Poll.toUrlString poll)
      )
    ErrorMsg errorText ->
      ( { model
           | appStatus = Error errorText
        }
      , Cmd.none
      )
      
          
    
    

type Route
  = Home
  | Poll String Int Int

parser: P.Parser (Route -> a) a
parser =
  P.oneOf
    [ P.map Home P.top
    , P.map Poll (P.s "poll" </> P.string </> P.int </> P.int)
    ]

appStatusFromUrl : Url.Url -> AppStatus
appStatusFromUrl url =
  let
    route = P.parse parser url
  in
  case route of
    Just Home ->
      SelectionFlow SelectionFlow.SelectDiscipline
    Just (Poll discString year month) ->
      let 
        maybePoll 
          = discString
          |> Poll.disciplineFromString
          |> Maybe.andThen (\d -> Poll.findPoll d year month PollData.polls)
      in
      case maybePoll of
        Just poll ->
          PollDisplay (PollDisplay.init poll)
        Nothing ->
          Error "Poll not found"
    Nothing ->
      Error "File not found"


    
          



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
        [ E.width E.fill ]
        [ title
        , E.el
            [ E.centerX
            , E.width (E.px 585)
            , E.padding 40 
            , Font.size 18
            ]
            (content model)
        ]
      , footer
      ]
    )

title : E.Element msg
title =
  E.el
    [ Font.color white
    , Background.color cornflowerblue
    , E.width E.fill
    ]
    <| E.column
        [ E.paddingXY 40 20 ]
        [ E.el [Font.size 24] <| E.text "Cold Mayo"
        , E.el [Font.size 16] <| E.text "A delicious plant-based spread for Nestris enthusiasts"
        ]

footer : E.Element msg
footer =
  E.el 
  [ E.alignBottom
  , E.alignRight 
  , E.width E.fill
  , E.padding 10
  , Font.size 16
  , Background.color cornflowerblue
  , Font.color white
  ] 
  <| E.paragraph 
    [ Font.alignRight
    ] 
    [ E.text "Put together by Nick \"arbaro\" Woods of NES Tetris fame. View the source code "
    , E.link 
        -- [ Font.color cornflowerblue ] 
        [ Font.underline ]
        { url = "https://github.com/nawoods/cold_mayo", label = E.text "here" }
    , E.text "."
    ]

content : Model -> E.Element Msg
content model =
  case model.appStatus of
    SelectionFlow selectionFlowModel ->
      SelectionFlow.selectionPollContent selectionFlowModel
        |> E.map SelectionFlowMsg
    PollDisplay viewGraphModel ->
      PollDisplay.view viewGraphModel
        |> E.map PollDisplayMsg
    Error errorText ->
      viewErrorContent errorText
      



viewErrorContent : String -> E.Element msg
viewErrorContent errorText =
  E.column
    [ E.spacing 30 
    , E.width E.fill
    ]
    [ E.el
       [ Font.color white 
       , Background.color orange
       , E.padding 5
       ]
       (E.text errorText)
    , E.link
       [ E.alignRight 
       ]
       { url = "/"
       , label = E.text "Go again ->"
       }
    ]