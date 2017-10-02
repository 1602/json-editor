port module JsonEditor exposing (main)

import Navigation exposing (Location, programWithFlags)
import Html exposing (Html)
import Dom
import Task
import Dict exposing (Dict)
import StyleSheet
    exposing
        ( Styles
            ( None
            , Main
            , InlineError
            , SchemaHeader
            , JsonEditor
            , MenuItem
            , NoOutline
            , SourceCode
            , PropertyName
            , ItemIndex
            , PropertyValue
            , PropertySeparator
            )
        , Variations(Active)
        , stylesheet
        )
import Element.Events exposing (onClick, onMouseDown, onInput, onBlur, onFocus, onDoubleClick)
import Element.Attributes as Attributes exposing (center, vary, inlineStyle, spacing, padding, height, minWidth, width, yScrollbar, fill, px, percent)
import Element exposing (Element, el, row, text, column, paragraph, empty)
import Markdown
import Json.Decode as Decode exposing (Decoder, decodeString, decodeValue, Value)
import Json.Encode as Encode
import Json.Schema.Helpers
    exposing
        ( implyType
        , typeToString
        , setJsonValue
        , getJsonValue
        , deleteIn
        , for
        , whenObjectSchema
        , parseJsonPointer
        , makeJsonPointer
        , resolve
        , calcSubSchemaType
        , setPropertyNameInJsonValue
        )
import Validation
import Json.Schema.Examples exposing (coreSchemaDraft6, bookingSchema)
import Json.Schema.Definitions as Schema
    exposing
        ( Schema(BooleanSchema, ObjectSchema)
        , SubSchema
        , Schemata(Schemata)
        , Type(AnyType, SingleType, NullableType, UnionType)
        , SingleType(IntegerType, NumberType, StringType, BooleanType)
        , JsonValue(ObjectValue, ArrayValue, BooleanValue, NullValue, NumericValue, StringValue, EmptyValue)
        , jsonValueDecoder
        , encodeJsonValue
        , blankSchema
        )


type alias View =
    Element Styles Variations Msg


type alias Model =
    { schema : Schema
    , jsonValue : JsonValue
    , error : Maybe String
    , valueUpdateErrors : Dict String String
    , editPath : String
    , editValue : String
    , editPropertyName : ( String, Int )
    , dragOver : Bool
    }


type Msg
    = NoOp
    | UrlChange Location
      --| StringChange String String
      --| NumberChange String String
      --| BooleanChange String Bool
    | ValueChange String String
    | InsertValue Bool (List String) Int String
    | DeleteMe String
    | EditJson Value
    | DragOver Bool
    | SetEditPath String String String
    | SetEditPropertyName String (List String) Int
    | StopEditing
    | SetPropertyName String


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
        coreSchemaDraft6
        -- jsonValue
        (val
            |> Decode.decodeValue jsonValueDecoder
            |> Result.withDefault (ObjectValue [])
        )
        -- error
        Nothing
        -- valueUpdateErrors
        Dict.empty
        -- editPath
        ""
        -- editValue
        "null"
        -- editPropertyName
        ( "", 0 )
        -- dragOver
        False
        ! []


focus : String -> Cmd Msg
focus id =
    if id /= "" then
        id
            |> Dom.focus
            |> Task.attempt (\_ -> NoOp)
    else
        Cmd.none


makeValidSchema : JsonValue -> Schema -> Result String Schema
makeValidSchema jsonValue schema =
    let
        val =
            jsonValue
                |> encodeJsonValue
    in
        schema
            |> Validation.validate val
            |> Result.map (\_ -> val)
            |> Result.andThen (Decode.decodeValue Schema.decoder)


updateValue : Model -> String -> Result String JsonValue -> Model
updateValue model path newStuff =
    let
        addError message =
            { model | valueUpdateErrors = model.valueUpdateErrors |> Dict.insert path message }
    in
        newStuff
            |> Result.andThen
                (\val ->
                    setJsonValue model.jsonValue path val
                )
            |> \res ->
                case res of
                    Ok v ->
                        case makeValidSchema v model.schema of
                            Ok x ->
                                { model
                                    | jsonValue = v
                                    , valueUpdateErrors = model.valueUpdateErrors |> Dict.remove path
                                }

                            Err message ->
                                { model
                                    | jsonValue = v
                                    , valueUpdateErrors = model.valueUpdateErrors |> Dict.insert path message
                                }

                    Err s ->
                        addError s


deletePath : Model -> String -> Model
deletePath model pointer =
    case deleteIn model.jsonValue pointer of
        Ok val ->
            { model | jsonValue = val }

        Err s ->
            let
                a =
                    Debug.log "err" s
            in
                model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

        SetEditPath id jsonPointer value ->
            { model
                | editPath = jsonPointer
                , editPropertyName = ( "", 0 )
                , editValue = value
            }
                ! [ select id ]

        {-
           StringChange path str ->
               updateValue model path (str |> Encode.string |> OtherValue |> Ok) ! []

           NumberChange path str ->
               updateValue model path (str |> String.toFloat |> Result.map (Encode.float >> OtherValue)) ! []

           BooleanChange path bool ->
               updateValue model path (bool |> Encode.bool |> OtherValue |> Ok) ! []
        -}
        ValueChange path str ->
            updateValue { model | editValue = str, editPath = path } path (decodeString jsonValueDecoder str) ! []

        StopEditing ->
            { model
                | editPath = ""
                , editPropertyName = ( "", 0 )
                , jsonValue =
                    if model.editValue == "" && model.editPath /= "" then
                        deleteIn model.jsonValue model.editPath
                            |> Result.withDefault model.jsonValue
                    else
                        model.jsonValue
            }
                ! []

        InsertValue hasKey path index formId ->
            let
                newJsonPointer =
                    makeJsonPointer path

                ( editProp, editPath, id ) =
                    if hasKey then
                        ( newJsonPointer, "", formId ++ "/prop/" ++ newJsonPointer ++ "/" )
                    else
                        ( "", newJsonPointer ++ "/" ++ (toString index), formId ++ "/value/" ++ newJsonPointer ++ "/" ++ (toString index) )
            in
                updateValue
                    { model
                        | editPath = editPath
                        , editPropertyName = ( editProp, index )
                        , editValue = ""
                    }
                    (newJsonPointer ++ "/")
                    (Ok <| EmptyValue)
                    ! [ select id ]

        UrlChange l ->
            model ! []

        EditJson s ->
            { model
                | jsonValue =
                    s
                        |> Decode.decodeValue jsonValueDecoder
                        |> Result.withDefault (ObjectValue [])
            }
                ! []

        DragOver isOver ->
            { model | dragOver = isOver } ! []

        DeleteMe pointer ->
            deletePath model pointer ! []

        SetEditPropertyName id path index ->
            { model | editPropertyName = ( makeJsonPointer path, index ), editPath = "" } ! [ select id ]

        SetPropertyName str ->
            --let
            {-
               newJsonPointer =
                   model.editPropertyName
                       |> parseJsonPointer
                       |> List.reverse
                       |> (::) str
                       |> List.reverse
                       |> makeJsonPointer
            -}
            --in
            { model
                | jsonValue =
                    model.jsonValue
                        |> setPropertyNameInJsonValue model.editPropertyName str
                        |> Result.withDefault model.jsonValue
                    {-
                       , activeSection =
                           if model.activeSection == model.editPropertyName then
                               newJsonPointer
                           else
                               model.activeSection
                    -}
            }
                ! []


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
        <|
            if model.dragOver then
                [ row None [ center, Attributes.verticalCenter, width <| fill 1, height <| fill 1 ] [ text "Drop it!" ]
                ]
            else
                case model.jsonValue of
                    ObjectValue [] ->
                        [ row None [ center, Attributes.verticalCenter, width <| fill 1, height <| fill 1 ] [ text "Drop schema file (or link to a schema) here." ]
                        ]

                    _ ->
                        [ form
                            "id"
                            model.valueUpdateErrors
                            model.editPropertyName
                            model.editPath
                            model.editValue
                            model.jsonValue
                            []
                            |> Element.textLayout SourceCode
                                []
                        ]


form : String -> Dict String String -> ( String, Int ) -> String -> String -> JsonValue -> List String -> List View
form id valueUpdateErrors editPropertyName editPath editValue val path =
    let
        offset level n =
            el None
                [ inlineStyle
                    [ ( "display", "inline-block" )
                    , ( "margin-left", (level + n |> (*) 2 |> toString) ++ "ch" )
                    , ( "padding-left", "1ch" )
                    ]
                , Attributes.class "deletable"
                ]

        deleteMe path =
            Element.onLeft
                [ el None
                    [ Attributes.class "delete-me"
                    , Attributes.moveRight 5
                    ]
                  <|
                    el None [ onClick <| DeleteMe <| makeJsonPointer path ] <|
                        text "-"
                ]

        itemRow isEditableProp level path ( index, key, prop ) =
            let
                pp =
                    path
                        ++ [ if isEditableProp then
                                key
                             else
                                index |> toString
                           ]

                hostPointer =
                    makeJsonPointer path

                ( editPropPath, editIndex ) =
                    editPropertyName

                newPointer =
                    makeJsonPointer pp

                propId =
                    id ++ "/prop/" ++ newPointer

                propName =
                    if isEditableProp then
                        if hostPointer == editPropPath && index == editIndex then
                            key
                                |> Element.inputText JsonEditor
                                    [ onInput <| SetPropertyName
                                    , Attributes.size <| String.length key + 1
                                    , onBlur <| SetEditPropertyName "" [] 0
                                    , Attributes.tabindex 0
                                    , Attributes.id propId
                                    ]
                                |> el None []
                        else
                            key
                                |> text
                                |> el PropertyName
                                    [ Attributes.tabindex 0
                                    , onFocus <| SetEditPropertyName propId path index
                                    ]
                    else
                        index
                            |> toString
                            |> text
                            |> el ItemIndex []
            in
                (propName
                    |> offset level 1
                    |> deleteMe pp
                )
                    :: (el PropertySeparator [ inlineStyle [ ( "display", "inline-block" ), ( "padding-right", "1ch" ) ] ] <| text ":")
                    :: controls (level + 1) prop pp

        joinWithCommaAndWrapWith jsp valId vvv open close isEditableProp level path list =
            list
                |> List.map (itemRow isEditableProp level path)
                |> List.intersperse [ text ",", Element.break ]
                |> List.concat
                |> (\x ->
                        (el PropertySeparator
                            [ inlineStyle
                                [ ( "padding-left"
                                  , if level == 0 then
                                        "1ch"
                                    else
                                        "0"
                                  )
                                , ( "display", "inline-block" )
                                ]
                            , onClick <| SetEditPath valId jsp <| Encode.encode 4 <| encodeJsonValue vvv
                            ]
                         <|
                            text open
                        )
                            :: Element.break
                            :: (x
                                    ++ [ Element.break
                                       , offset level 0 <|
                                            el PropertySeparator
                                                (if editPath == "" || editValue /= "" then
                                                    [ onFocus <| InsertValue isEditableProp path (List.length list) id
                                                    , Attributes.tabindex 0
                                                    ]
                                                 else
                                                    []
                                                )
                                            <|
                                                text close
                                       ]
                               )
                   )

        controls level val path =
            let
                jsp =
                    makeJsonPointer path

                valId =
                    id ++ "/value/" ++ jsp

                isEditing =
                    jsp == editPath

                editForm editValue =
                    editValue
                        |> Element.inputText JsonEditor
                            [ onInput <| ValueChange jsp
                            , onBlur StopEditing
                            , Attributes.size <| String.length editValue + 1
                            , inlineStyle [ ( "display", "inline-block" ) ]
                            , Attributes.tabindex 0
                            , Attributes.id valId
                            ]
                        |> Element.el None
                            [ inlineStyle [ ( "display", "inline-block" ) ] ]
                        |> Element.below
                            [ valueUpdateErrors
                                |> Dict.get jsp
                                |> Maybe.map (text >> (el InlineError []))
                                |> Maybe.withDefault empty
                            ]

                edit val =
                    if isEditing then
                        editValue |> editForm
                    else
                        val
                            |> (\v ->
                                    if v == "" then
                                        "∅"
                                    else
                                        v
                               )
                            |> text
                            |> Element.el PropertyValue
                                [ inlineStyle [ ( "display", "inline-block" ) ]
                                  --, Attributes.contenteditable False
                                , onFocus <| SetEditPath valId jsp val
                                , Attributes.tabindex 0
                                ]
            in
                case val of
                    ArrayValue list ->
                        list
                            |> List.indexedMap (\index item -> ( index, "", item ))
                            |> joinWithCommaAndWrapWith jsp valId val "[" "]" False level path

                    ObjectValue obj ->
                        if isEditing then
                            [ val
                                |> encodeJsonValue
                                |> Encode.encode 4
                                |> editForm
                                ]
                        else
                            obj
                                |> List.indexedMap (\index ( key, val ) -> ( index, key, val ))
                                |> joinWithCommaAndWrapWith jsp valId val "{" "}" True level path

                    EmptyValue ->
                        [ edit "" ]

                    StringValue str ->
                        [ edit <| toString str ]

                    NumericValue n ->
                        [ edit <| toString n ]

                    BooleanValue n ->
                        [ edit <|
                            (if n then
                                "true"
                             else
                                "false"
                            ) ]

                    NullValue ->
                        [ edit "null" ]
    in
        controls 0 val path



{- -}


getFields : Value -> String -> List ( String, Value )
getFields val subpath =
    let
        path =
            subpath
                |> String.split "/"
                |> List.drop 1
                |> List.filter ((/=) "")
    in
        val
            |> Decode.decodeValue (Decode.at path <| Decode.keyValuePairs Decode.value)
            |> Result.withDefault []
            |> List.reverse


col10 : List View -> View
col10 =
    column None [ spacing 10, padding 10 ]


source : String -> Model -> Schema -> String -> View
source id model s subpath =
    let
        isEditMode =
            True

        val =
            model.jsonValue
                |> getJsonValue (parseJsonPointer subpath)
                |> Result.withDefault (ObjectValue [])

        editForm val =
            Element.textLayout None
                []
                (subpath
                    |> parseJsonPointer
                    |> form
                        id
                        model.valueUpdateErrors
                        model.editPropertyName
                        model.editPath
                        model.editValue
                        val
                )

        displaySchemaNode =
            editForm
    in
        displaySchemaNode val


takeHalfWidth : View -> View
takeHalfWidth =
    el None [ width <| percent 50 ]


when : Maybe a -> (a -> View) -> View
when a fn =
    case a of
        Just s ->
            fn s

        Nothing ->
            empty


metaDoc : SubSchema -> View
metaDoc s =
    column None
        []
        [ when s.title (\title -> row None [ inlineStyle [ ( "font-size", "18px" ) ] ] [ title |> Element.bold ])
        , when s.description (\description -> paragraph None [ inlineStyle [ ( "font-size", "16px" ) ] ] [ description |> Markdown.toHtml [] |> Element.html ])
        ]
