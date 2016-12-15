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
        [ Step
            [ (Torch
                0
                "Build a workflow tool"
                "It should be a very simple tool, very basic, used as a proof of concept"
                "magopian@mozilla.com"
                Done
              )
            ]
            True
        , Step
            [ (Torch
                1
                "Edit the workflow"
                "Add steps, add torches to steps, change step to be 'all mandatory' or not"
                "magopian@mozilla.com"
                Todo
              )
            ]
            True
        , Step
            [ Torch
                2
                "Review"
                "Have a review from natim"
                "rhubscher@mozilla.com"
                Todo
            , Torch
                3
                "Review"
                "Have a review from n1k0"
                "nperriault@mozilla.com"
                Todo
            , Torch
                4
                "Review"
                "Have a review from glasserc"
                "eglassercamp@mozilla.com"
                Todo
            ]
            False
        , Step
            [ Torch
                5
                "Build the proof of concept"
                "`npm run build`"
                "magopian@mozilla.com"
                Todo
            , Torch
                6
                "Publish to github pages"
                "`npm run publish-to-gh-pages` then visit https://magopian.github.io/pass-the-torch/index.html"
                "magopian@mozilla.com"
                Todo
            ]
            True
        , Step
            [ (Torch
                7
                "Profit"
                "The workflow ended, time to celebrate!"
                "@all"
                Todo
              )
            ]
            True
        ]
        8
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
