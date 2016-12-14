module Model exposing (..)

-- Model


type Msg
    = UpdateStatus Int
    | NewTitle String
    | NewContent String
    | NewBearer String
    | AddStep
    | DeleteStep Int
    | NewTorch Int
    | CancelNewTorch
    | AddTorch Int
    | ChangeAllMandatory Int Bool


type Status
    = Todo
    | Done


type alias Torch =
    { -- Only used to know which torch we're editing/doing. Not ordered.
      id : Int
    , title : String
    , content : String
    , bearer : String
    , status : Status
    }


type alias Step =
    { torchList : List Torch
    , allMandatory : Bool
    }


type alias Workflow =
    List Step


type alias Model =
    { workflow : Workflow
    , currentId : Int
    , title : String
    , content : String
    , bearer : String
    , newTorch : Maybe Int
    }


init : ( Model, Cmd Msg )
init =
    (Model
        [ Step [ (Torch 0 "foo" "bar" "foobar" Done) ] True
        , Step [ (Torch 1 "bar" "baz" "cruux" Todo) ] True
        , Step
            [ Torch 2 "signoff" "foo" "foobar" Todo
            , Torch 3 "signoff" "bar" "cruux" Todo
            ]
            False
        , Step
            [ Torch 4 "build foo" "http://example.com" "John" Todo
            , Torch 5 "build bar" "http://example.com" "John" Todo
            ]
            True
        , Step [ (Torch 6 "finished" "The workflow ended" "John Doe" Todo) ]
            True
        ]
        7
        ""
        ""
        ""
        Nothing
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
                    , workflow = model.workflow ++ [ Step [ newTorch ] True ]
                }
                    ! []

        DeleteStep index ->
            let
                newWorkflow =
                    (List.take index model.workflow)
                        ++ (List.drop (index + 1) model.workflow)
            in
                { model | workflow = newWorkflow } ! []

        NewTorch index ->
            { model | newTorch = Just index } ! []

        CancelNewTorch ->
            { model | newTorch = Nothing } ! []

        AddTorch index ->
            let
                newTorch =
                    Torch
                        model.currentId
                        model.title
                        model.content
                        model.bearer
                        Todo

                updatedWorkflow =
                    updateStep
                        index
                        (\step ->
                            { step | torchList = step.torchList ++ [ newTorch ] }
                        )
                        model.workflow
            in
                { model
                    | title = ""
                    , content = ""
                    , bearer = ""
                    , currentId = model.currentId + 1
                    , newTorch = Nothing
                    , workflow = updatedWorkflow
                }
                    ! []

        ChangeAllMandatory index allMandatory ->
            let
                updatedWorkflow =
                    updateStep
                        index
                        (\step ->
                            { step | allMandatory = allMandatory }
                        )
                        model.workflow
            in
                { model | workflow = updatedWorkflow } ! []


updateStep : Int -> (Step -> Step) -> Workflow -> Workflow
updateStep id updateFunction workflow =
    let
        changeStep : Int -> Step -> Step
        changeStep stepIndex step =
            if stepIndex == id then
                updateFunction step
            else
                step
    in
        List.indexedMap changeStep workflow


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
            { step | torchList = List.map changeTorch step.torchList }
    in
        List.map changeStep workflow
