module View exposing (view)

import Html
import Html.Attributes
import Html.Events
import Model exposing (..)


view : Model -> Html.Html Msg
view model =
    Html.div
        [ Html.Attributes.class "container" ]
        [ Html.h1
            []
            [ Html.text "Who's holding the torch?" ]
        , Html.div
            []
            (List.indexedMap viewStep model.workflow)
        , Html.h2
            []
            [ Html.text "Add a new step" ]
        , Html.form
            [ Html.Attributes.class "well"
            , Html.Events.onSubmit AddStep
            ]
            [ formGroup
                "Title"
                (input model.title "Frobnicate the bizbaz" NewTitle)
            , formGroup
                "Content"
                (Html.textarea
                    [ Html.Attributes.class "form-control"
                    , Html.Attributes.placeholder "Lengthy description"
                    , Html.Attributes.value model.content
                    , Html.Events.onInput NewContent
                    ]
                    []
                )
            , formGroup
                "Bearer"
                (input model.bearer "person@in.charge.com" NewBearer)
            , Html.input
                [ Html.Attributes.class "btn btn-primary"
                , Html.Attributes.type_ "submit"
                , Html.Attributes.value "Add this new step"
                ]
                []
            ]
        ]


formGroup : String -> Html.Html Msg -> Html.Html Msg
formGroup label input =
    Html.div
        [ Html.Attributes.class "form-group" ]
        [ Html.label [] [ Html.text label ]
        , input
        ]


input : String -> String -> (String -> Msg) -> Html.Html Msg
input value placeholder onInput =
    Html.input
        [ Html.Attributes.class "form-control"
        , Html.Attributes.placeholder placeholder
        , Html.Attributes.value value
        , Html.Events.onInput onInput
        ]
        []


viewTorch : Torch -> Html.Html Msg
viewTorch torch =
    Html.div [ Html.Attributes.class <| panelClass (isTorchDone torch) ]
        [ Html.div
            [ Html.Attributes.class "panel-heading" ]
            [ Html.text torch.title ]
        , Html.div
            [ Html.Attributes.class "panel-body" ]
            [ Html.text torch.content ]
        , Html.div
            [ Html.Attributes.class "panel-footer" ]
            [ Html.text <| "In charge: " ++ torch.bearer
            , Html.div
                [ Html.Attributes.class "pull-right" ]
                [ Html.button
                    [ Html.Attributes.type_ "button"
                    , Html.Attributes.class "btn btn-primary btn-xs"
                    , Html.Attributes.disabled (isTorchDone torch)
                    , Html.Events.onClick (UpdateStatus torch.id)
                    ]
                    [ Html.text "I'm Done" ]
                ]
            ]
        ]


viewStep : Int -> Step -> Html.Html Msg
viewStep index step =
    Html.div
        [ Html.Attributes.class <| panelClass (isStepDone step) ]
        [ Html.div
            [ Html.Attributes.class "panel-heading" ]
            [ Html.text <| "Step: " ++ (toString index)
            , Html.a
                [ Html.Attributes.class "pull-right"
                , Html.Attributes.href "#"
                ]
                [ Html.i
                    [ Html.Attributes.class "glyphicon glyphicon-trash"
                    , Html.Events.onClick (DeleteStep index)
                    ]
                    []
                ]
            ]
        , Html.div
            [ Html.Attributes.class "panel-body" ]
            [ case step of
                Single torch ->
                    viewTorch torch

                Any torchList ->
                    Html.ul []
                        (torchList
                            |> List.map viewTorch
                            |> List.intersperse (Html.h4 [] [ Html.text "or" ])
                        )

                All torchList ->
                    Html.ul []
                        (torchList
                            |> List.map viewTorch
                            |> List.intersperse (Html.h4 [] [ Html.text "and" ])
                        )
            ]
        ]


panelClass : Bool -> String
panelClass isDone =
    if isDone then
        "panel panel-success"
    else
        "panel panel-default"


isTorchDone : Torch -> Bool
isTorchDone torch =
    torch.status == Done


isStepDone : Step -> Bool
isStepDone step =
    case step of
        Single torch ->
            isTorchDone torch

        Any torchList ->
            List.any isTorchDone torchList

        All torchList ->
            List.all isTorchDone torchList
