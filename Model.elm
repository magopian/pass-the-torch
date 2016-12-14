module Model exposing (..)

-- Model


type Msg
    = UpdateStatus Int
    | NewTitle String
    | NewContent String
    | NewBearer String
    | AddStep
    | DeleteStep Int


type Status
    = Todo
    | Done


type alias Torch =
    { id : Int
    , title : String
    , content : String
    , bearer : String
    , status : Status
    }


type Step
    = Single Torch
    | Any (List Torch)
    | All (List Torch)


type alias Workflow =
    List Step


type alias Model =
    { workflow : Workflow
    , currentId : Int
    , title : String
    , content : String
    , bearer : String
    }


init : ( Model, Cmd Msg )
init =
    (Model
        [ Single (Torch 0 "foo" "bar" "foobar" Done)
        , Single (Torch 1 "bar" "baz" "cruux" Todo)
        , Any
            [ Torch 2 "signoff" "foo" "foobar" Todo
            , Torch 3 "signoff" "bar" "cruux" Todo
            ]
        , All
            [ Torch 4 "build foo" "http://example.com" "John" Todo
            , Torch 5 "build bar" "http://example.com" "John" Todo
            ]
        , Single (Torch 6 "finished" "The workflow ended" "John Doe" Todo)
        ]
        7
        ""
        ""
        ""
    )
        ! []



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        UpdateStatus id ->
            let
                updatedWorkflow =
                    updateTorch
                        id
                        (\torch ->
                            { torch
                                | status =
                                    if torch.status == Done then
                                        Todo
                                    else
                                        Done
                            }
                        )
                        model.workflow
            in
                { model | workflow = updatedWorkflow } ! []

        NewTitle title ->
            { model | title = title } ! []

        NewContent content ->
            { model | content = content } ! []

        NewBearer bearer ->
            { model | bearer = bearer } ! []

        AddStep ->
            let
                newTorch =
                    Torch
                        model.currentId
                        model.title
                        model.content
                        model.bearer
                        Todo
            in
                { model
                    | title = ""
                    , content = ""
                    , bearer = ""
                    , currentId = model.currentId + 1
                    , workflow = model.workflow ++ [ Single newTorch ]
                }
                    ! []

        DeleteStep index ->
            let
                newWorkflow =
                    (List.take index model.workflow)
                        ++ (List.drop (index + 1) model.workflow)
            in
                { model | workflow = newWorkflow } ! []


updateTorch : Int -> (Torch -> Torch) -> Workflow -> Workflow
updateTorch id updateFunction workflow =
    let
        changeTorch : Torch -> Torch
        changeTorch torch =
            if torch.id == id then
                updateFunction torch
            else
                torch

        changeStep : Step -> Step
        changeStep step =
            case step of
                Single torch ->
                    Single (changeTorch torch)

                Any torchList ->
                    Any (List.map changeTorch torchList)

                All torchList ->
                    All (List.map changeTorch torchList)
    in
        List.map changeStep workflow



-- Main
