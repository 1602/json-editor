port module JsonEditorApp exposing (main)

import Navigation exposing (Location, programWithFlags)
import Html exposing (Html)
import JsonInput exposing (Msg(SetEditableValue), ExternalMsg(Select))
import StyleSheet
    exposing
        ( Styles
            ( None
            , Main
            , SourceCode
            )
        , Variations
        , stylesheet
        )
import Element.Attributes as Attributes exposing (center, vary, inlineStyle, spacing, padding, height, minWidth, width, yScrollbar, fill, px, percent)
import Element exposing (Element, el, row, text, column, paragraph, empty)
import Json.Decode as Decode exposing (Decoder, decodeString, decodeValue, Value)
import Json.Schema.Examples exposing (coreSchemaDraft6, bookingSchema)
import Json.Schema.Definitions as Schema
    exposing
        ( JsonValue(ObjectValue)
        )


type alias View =
    Element Styles Variations Msg


type alias Model =
    { dragOver : Bool
    , jsonInput : JsonInput.Model
    }


type Msg
    = UrlChange Location
    | EditJson Value
    | DragOver Bool
    | JsonInputMsg JsonInput.Msg


main : Program Value Model Msg
main =
    programWithFlags UrlChange
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : Value -> Location -> ( Model, Cmd Msg )
init val location =
    Model
        -- dragOver
        False
        -- jsonInput
        (JsonInput.init coreSchemaDraft6 val)
        ! []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlChange l ->
            model ! []

        EditJson s ->
            { model | jsonInput = JsonInput.init coreSchemaDraft6 s } ! []

        DragOver isOver ->
            { model | dragOver = isOver } ! []

        JsonInputMsg m ->
            let
                ( jsonInput, externalMsg ) =
                    JsonInput.update m model.jsonInput

                cmd =
                    case externalMsg of
                        Select id ->
                            select id

                        _ ->
                            Cmd.none
            in
                { model | jsonInput = jsonInput } ! [ cmd ]


port editJson : (Value -> msg) -> Sub msg


port dragOver : (Bool -> msg) -> Sub msg


port download : Value -> Cmd msg


port select : String -> Cmd msg


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ editJson EditJson
        , dragOver DragOver
        ]


view : Model -> Html Msg
view model =
    Element.viewport stylesheet <|
        row Main
            [ height <| fill 1
            , width <| fill 1
            ]
            [ if model.dragOver then
                row None [ center, Attributes.verticalCenter, width <| fill 1, height <| fill 1 ] [ text "Drop it!" ]
              else
                case model.jsonInput.jsonValue of
                    ObjectValue [] ->
                        row None [ center, Attributes.verticalCenter, width <| fill 1, height <| fill 1 ] [ text "Drop schema file (or link to a schema) here." ]

                    _ ->
                        model.jsonInput
                            |> JsonInput.view "id"
                            |> Element.html
                            |> Element.map JsonInputMsg
                            |> el None
                                [ width <| px 500
                                , height <| fill 1
                                , yScrollbar
                                ]
              --|> Element.textLayout SourceCode []
            ]
