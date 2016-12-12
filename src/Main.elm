module Main exposing (..)

import Html
import Html.Attributes
import Html.Events


-- Model


type Msg
    = UpdateStatus Int


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



-- View


view : Model -> Html.Html Msg
view model =
    Html.div [] (List.map viewStep model.workflow)


viewTorch : Torch -> Html.Html Msg
viewTorch torch =
    Html.div []
        [ Html.input
            [ Html.Attributes.type_ "checkbox"
            , case torch.status of
                Todo ->
                    Html.Attributes.checked False

                Done ->
                    Html.Attributes.checked True
            , Html.Events.onClick (UpdateStatus torch.id)
            ]
            []
        , Html.text <|
            "Title: "
                ++ torch.title
                ++ " - Content: "
                ++ torch.content
                ++ " - Bearer: "
                ++ torch.bearer
                ++ " - Status: "
                ++ (toString torch.status)
        ]


viewStep : Step -> Html.Html Msg
viewStep step =
    case step of
        Single torch ->
            viewTorch torch

        Any torchList ->
            Html.ul []
                (torchList
                    |> List.map viewTorch
                    |> List.intersperse (Html.text " or ")
                )

        All torchList ->
            Html.ul []
                (torchList
                    |> List.map viewTorch
                    |> List.intersperse (Html.text " and ")
                )



-- Main


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }
