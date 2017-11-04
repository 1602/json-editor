port module JsonEditorApp exposing (main)

import Navigation exposing (Location, programWithFlags)
import Html exposing (Html)
import JsonInput exposing (Msg(SetEditableValue), ExternalMsg(Select, OnInput))
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
import SchemaExamples exposing (coreSchemaDraft6, bookingSchema)
import Json.Encode as Encode
import Json.Schema
import Json.Schema.Random
import Json.Schema.Definitions
import Task
import Time
import Random
import JsonValue exposing (JsonValue(ObjectValue))


type alias View =
    Element Styles Variations Msg


type alias Model =
    { dragOver : Bool
    , jsonInput : JsonInput.Model
    , seed : Random.Seed
    , schema : Json.Schema.Definitions.Schema
    , randomValue : Value
    }


type Msg
    = UrlChange Location
    | EditJson Value
    | DragOver Bool
    | JsonInputMsg JsonInput.Msg
    | Randomize Float


main : Program Value Model Msg
main =
    programWithFlags UrlChange
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


settings =
    Json.Schema.Random.defaultSettings


init : Value -> Location -> ( Model, Cmd Msg )
init v location =
    let
        schema =
            coreSchemaDraft6

        --settings =
        --Json.Schema.Random.defaultSettings
        --( val, _ ) =
        --Random.step (Json.Schema.Random.value { settings | defaultListLengthLimit = 10 } schema) (Random.initialSeed 3)
    in
        Model
            -- dragOver
            False
            -- jsonInput
            (JsonInput.init schema (bookingSchema |> Json.Schema.Definitions.encode))
            -- seed
            (Random.initialSeed 0)
            -- schema
            bookingSchema
            -- randomValue
            Encode.null
            ! [ Time.now |> Task.perform Randomize ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlChange l ->
            model ! []

        EditJson s ->
            { model | jsonInput = JsonInput.init coreSchemaDraft6 s } ! []

        DragOver isOver ->
            { model | dragOver = isOver } ! []

        Randomize x ->
            let
                ( val, seed ) =
                    Random.step (Json.Schema.Random.value { settings | defaultListLengthLimit = 10 } model.schema) (Random.initialSeed <| round x)
            in
                { model
                    | seed = seed
                    , randomValue = val
                }
                    ! []

        JsonInputMsg m ->
            let
                ( ji, externalMsg ) =
                    JsonInput.update m model.jsonInput

                ( um, cmd ) =
                    case externalMsg of
                        Select id ->
                            { model | jsonInput = ji } ! [ select id ]

                        OnInput ->
                            case ji.jsonValue |> JsonValue.encode |> Json.Schema.fromValue of
                                Ok schema ->
                                    let
                                        ( val, _ ) =
                                            Random.step (Json.Schema.Random.value { settings | defaultListLengthLimit = 10 } schema) model.seed
                                    in
                                        { model
                                            | jsonInput = ji
                                            , randomValue = val
                                        }
                                            ! []

                                _ ->
                                    { model | jsonInput = ji } ! []

                        _ ->
                            { model | jsonInput = ji } ! []
            in
                um ! [ cmd ]


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
            , spacing 100
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
            , model.randomValue
                |> Encode.encode 4
                |> text
                |> el None
                    [ inlineStyle
                        [ ( "white-space", "pre" )
                        , ( "font-size", "10px" )
                        , ( "font-family", "menlo, monospace" )
                        ]
                    , padding 100
                    ]
            ]
