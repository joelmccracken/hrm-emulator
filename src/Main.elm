import Html exposing (Html, div, text)
import Html.App as Html
import Time exposing (Time, second)
import Array exposing (Array)
import Maybe exposing (andThen)
import Result exposing (fromMaybe, andThen)

main : Program Never
main =
  Html.program { init = init, view = view, update = update, subscriptions = subscriptions }


subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every second Tick


init : (Model, Cmd Msg)
init =
  (initialModel, Cmd.none)


type alias Value = Int


type Instruction = Inbox
                 | Outbox


type Status = Running | Complete | Error String


-- MODEL

type alias MachineState =
    { status   : Status
    , held     : Maybe Value
    , input    : List Value
    , output   : List Value
    , pc       : Int
    }


type alias Model =
    { program : Array Instruction
    , state   : MachineState
    }


initialModel : Model
initialModel =
  { program  = Array.fromList program
  , state = { status = Running
            , held   = Nothing
            , input  = input
            , output = []
            , pc     = 0
            }
  }


program : List Instruction
program =
    [ Inbox
    , Outbox
    , Inbox
    , Outbox
    , Inbox
    , Outbox
    ]


input : List Value
input = [ 2
        , 7
        , 8
        ]


updateState : Model -> (MachineState -> MachineState) -> Model
updateState model updater =
    let newState = updater model.state
    in { model | state = newState }


stepPC : Model -> Model
stepPC model =
    updateState model (\s-> {s | pc = s.pc + 1 })


completeIfFinished : Model -> Model
completeIfFinished model =
  let programLength = Array.length model.program
      pc = model.state.pc
  in if pc < programLength then
         model
     else
         updateState model complete


shiftInboxToHands : Model -> Model
shiftInboxToHands model =
  let first = List.head model.state.input
      rest  = case List.tail model.state.input of
                Just r  -> r
                Nothing -> []
  in case first of
       Just val ->
           updateState model (\s-> { s | held = Just val, input = rest})
       Nothing -> updateState model (\s-> {s | status = Error "tried to pick up an item from the inbox, but inbox was empty" })


-- state manipulators

complete : MachineState -> MachineState
complete state = { state | status = Complete }


shiftHandsToOutbox : Model -> Model
shiftHandsToOutbox model =
  let currentState = model.state
  in case model.state.held of
    Nothing  -> updateState model complete
    Just val -> { model | state =
                     { currentState
                         | held   = Nothing,
                           output = (val :: currentState.output)
                     }}


currentInstruction : Array Instruction -> Int -> Result String Instruction
currentInstruction instructions pc =
    (Array.get pc instructions)
      |> fromMaybe ("No Instrution at" ++ (toString pc))


stepModel : Model -> Model
stepModel model =
  currentInstruction model.program model.state.pc
    `Result.andThen` (\instruction -> Ok (processInstruction instruction model))
    |> resultToState model


resultToState : Model -> Result String Model -> Model
resultToState originalModel result =
  case result of
    Err string -> updateState originalModel (\s-> { s | status = Error string })
    Ok  model ->  model


processInstruction : Instruction -> Model -> Model
processInstruction instruction model =
  case instruction of
    Inbox  ->
      shiftInboxToHands model |> stepPC |> completeIfFinished
    Outbox ->
      shiftHandsToOutbox model |> stepPC |> completeIfFinished


-- UPDATE

type Msg
  = Tick Time


update : Msg -> Model -> (Model, Cmd Msg)
update action model =
  case action of
    Tick newTime ->
      let
        isComplete = model.state.status
      in
        case isComplete of
          Running -> (stepModel model, Cmd.none)
          _       -> (model, Cmd.none)


-- VIEW

renderState : MachineState -> Html a
renderState state =
    div [] [text (toString state)]


view : Model -> Html Msg
view model =
  div []
    [ div [] [text (toString model.program)]
    , renderState model.state
    ]
