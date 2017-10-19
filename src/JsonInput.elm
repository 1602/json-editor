module JsonInput exposing (Model, update, view, init, getValue, ExternalMsg(Select, OnInput), Msg(SetEditableValue))

import Html exposing (Html)
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
            , DataRowHint
            )
        , Variations(Active)
        , stylesheet
        )
import Element.Events exposing (onClick, onMouseDown, onInput, onBlur, onFocus, onDoubleClick)
import Element.Attributes as Attributes
    exposing
        ( center
        , vary
        , inlineStyle
        , spacing
        , padding
        , height
        , minWidth
        , width
        , yScrollbar
        , fill
        , px
        , percent
        , alignRight
        , moveLeft
        )
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
import Json.Schema.Definitions as Schema
    exposing
        ( Schema(BooleanSchema, ObjectSchema)
        , SubSchema
        , Schemata(Schemata)
        , Type(AnyType, SingleType, NullableType, UnionType)
        , SingleType(IntegerType, NumberType, StringType, BooleanType)
        , JsonValue(ObjectValue, ArrayValue, BooleanValue, NullValue, NumericValue, StringValue)
        , jsonValueDecoder
        , encodeJsonValue
        , blankSchema
        )


type alias View =
    Element Styles Variations Msg


type alias Model =
    { schema : Schema
    , jsonValue : JsonValue
    , editableJsonValue : EditableJsonValue
    , error : Maybe String
    , valueUpdateErrors : Dict String String
    , editPath : String
    , editValue : String
    , editPropertyName : ( String, Int )
    }


type EditableJsonValue
    = EmptyValue
    | DeletedValue JsonValue
    | StringEValue String
    | NumericEValue Float
    | BoolEValue Bool
    | ObjectEValue (List ( String, EditableJsonValue ))
    | ArrayEValue (List EditableJsonValue)
    | NullEValue


type ExternalMsg
    = NoOp
    | Select String
    | OnInput


type
    Msg
    --| StringChange String String
    --| NumberChange String String
    --| BooleanChange String Bool
    = ValueChange String String
    | InsertValue Bool (List String) Int String
    | SetEditableValue Value
    | DeleteMe String
    | SetEditPath String String String
    | SetEditPropertyName String (List String) Int
    | StopEditing
    | SetPropertyName String


makeEditableJsonValue : JsonValue -> EditableJsonValue
makeEditableJsonValue jsonValue =
    case jsonValue of
        StringValue s ->
            StringEValue s

        NumericValue n ->
            NumericEValue n

        BooleanValue b ->
            BoolEValue b

        NullValue ->
            NullEValue

        ObjectValue obj ->
            obj
                |> List.map (\( key, val ) -> ( key, val |> makeEditableJsonValue ))
                |> ObjectEValue

        ArrayValue list ->
            list
                |> List.map makeEditableJsonValue
                |> ArrayEValue


makeJsonValue : EditableJsonValue -> JsonValue
makeJsonValue editableJsonValue =
    case editableJsonValue of
        StringEValue s ->
            StringValue s

        NumericEValue n ->
            NumericValue n

        BoolEValue b ->
            BooleanValue b

        NullEValue ->
            NullValue

        EmptyValue ->
            NullValue

        DeletedValue _ ->
            NullValue

        ObjectEValue obj ->
            obj
                |> List.filter
                    (\( key, val ) ->
                        case val of
                            EmptyValue ->
                                False

                            DeletedValue _ ->
                                False

                            _ ->
                                True
                    )
                |> List.map (\( key, val ) -> ( key, val |> makeJsonValue ))
                |> ObjectValue

        ArrayEValue list ->
            list
                |> List.filter
                    (\val ->
                        case val of
                            EmptyValue ->
                                False

                            DeletedValue _ ->
                                False

                            _ ->
                                True
                    )
                |> List.map makeJsonValue
                |> ArrayValue


init : Schema -> Value -> Model
init schema val =
    let
        jsonValue =
            val
                |> Decode.decodeValue jsonValueDecoder
                |> Result.withDefault (ObjectValue [])

        editableJsonValue =
            jsonValue
                |> makeEditableJsonValue
    in
        Model
            schema
            -- jsonValue
            jsonValue
            -- editableJsonValue
            editableJsonValue
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


getValue : Model -> Value
getValue model =
    model.jsonValue
        |> encodeJsonValue


updateValue : Model -> String -> Result String EditableJsonValue -> ( Model, ExternalMsg )
updateValue model path newStuff =
    let
        addError message =
            { model | valueUpdateErrors = model.valueUpdateErrors |> Dict.insert path message }
    in
        newStuff
            |> Result.andThen
                (\val ->
                    val
                        |> makeJsonValue
                        |> setJsonValue model.jsonValue path
                )
            |> \res ->
                case res of
                    Ok v ->
                        case makeValidSchema v model.schema of
                            Ok x ->
                                ( { model
                                    | jsonValue = v
                                    , valueUpdateErrors = model.valueUpdateErrors |> Dict.remove path
                                  }
                                , OnInput
                                )

                            Err message ->
                                ( { model
                                    | jsonValue = v
                                    , valueUpdateErrors = model.valueUpdateErrors |> Dict.insert path message
                                  }
                                , OnInput
                                )

                    Err s ->
                        ( addError s, NoOp )


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


update : Msg -> Model -> ( Model, ExternalMsg )
update msg model =
    case msg of
        SetEditPath id jsonPointer value ->
            ( { model
                | editPath = jsonPointer
                , editPropertyName = ( "", 0 )
                , editValue = value
              }
            , Select id
            )

        {-
           StringChange path str ->
               updateValue model path (str |> Encode.string |> OtherValue |> Ok) ! []

           NumberChange path str ->
               updateValue model path (str |> String.toFloat |> Result.map (Encode.float >> OtherValue)) ! []

           BooleanChange path bool ->
               updateValue model path (bool |> Encode.bool |> OtherValue |> Ok) ! []
        -}
        ValueChange path str ->
            str
                |> decodeString jsonValueDecoder
                |> Result.map makeEditableJsonValue
                |> updateValue { model | editValue = str, editPath = path } path

        StopEditing ->
            ( { model
                | editPath = ""
                , editPropertyName = ( "", 0 )
                , jsonValue =
                    if model.editValue == "" && model.editPath /= "" then
                        deleteIn model.jsonValue model.editPath
                            |> Result.withDefault model.jsonValue
                    else
                        model.jsonValue
              }
            , NoOp
            )

        InsertValue hasKey path index formId ->
            let
                newJsonPointer =
                    makeJsonPointer path

                ( editProp, editPath, id ) =
                    if hasKey then
                        ( newJsonPointer
                        , ""
                        , formId ++ "/prop/" ++ newJsonPointer ++ "/"
                        )
                    else
                        ( ""
                        , newJsonPointer ++ "/" ++ (toString index)
                        , formId ++ "/value/" ++ newJsonPointer ++ "/" ++ (toString index)
                        )

                ( updatedModel, _ ) =
                    updateValue
                        { model
                            | editPath = editPath
                            , editPropertyName = ( editProp, index )
                            , editValue = ""
                        }
                        (newJsonPointer ++ "/")
                        (Ok <| EmptyValue)
            in
                ( updatedModel
                , Select id
                )

        SetEditableValue s ->
            ( { model
                | jsonValue =
                    s
                        |> Decode.decodeValue jsonValueDecoder
                        |> Result.withDefault (ObjectValue [])
              }
            , NoOp
            )

        DeleteMe pointer ->
            ( deletePath model pointer, NoOp )

        SetEditPropertyName id path index ->
            ( { model
                | editPropertyName = ( makeJsonPointer path, index )
                , editPath = ""
              }
            , Select id
            )

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
            ( { model
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
            , NoOp
            )


view : String -> Model -> Html Msg
view id model =
    Element.layout stylesheet <|
        el SourceCode
            [ height <| fill 1
            , width <| fill 1
            ]
        <|
            form
                id
                model.valueUpdateErrors
                model.editPropertyName
                model.editPath
                model.editValue
                model.editableJsonValue
                []


form : String -> Dict String String -> ( String, Int ) -> String -> String -> EditableJsonValue -> List String -> View
form id valueUpdateErrors editPropertyName editPath editValue val path =
    let
        ( editPropPath, editIndex ) =
            editPropertyName

        propName name index path =
            let
                hostPointer =
                    makeJsonPointer path

                isEditableProp =
                    True

                pp =
                    path
                        ++ [ if isEditableProp then
                                name
                             else
                                index |> toString
                           ]

                ( editPropPath, editIndex ) =
                    editPropertyName

                newPointer =
                    makeJsonPointer pp

                propId =
                    id ++ "/prop/" ++ newPointer
            in
                if hostPointer == editPropPath && index == editIndex then
                    name
                        |> Element.inputText JsonEditor
                            [ onInput <| SetPropertyName
                            , Attributes.size <| String.length name + 1
                            , onBlur <| StopEditing
                            , Attributes.tabindex 0
                            , Attributes.id propId
                            ]
                        |> el None []
                else
                    name
                        |> text
                        |> el PropertyName
                            [ Attributes.tabindex 0
                            , onFocus <| SetEditPropertyName propId path index
                            ]

        itemRow isEditableProp index name prop =
            [ [ Element.checkbox True
                    None
                    [ moveLeft 2
                    , inlineStyle [ ( "width", "4ch" ), ( "padding-left", "2ch" ) ]
                    , Attributes.class "delete-property"
                    ]
                <|
                    text ""
              , propName name index path
              , ": "
                    |> text
                    |> el PropertySeparator []
              , case prop of
                    StringEValue s ->
                        s |> toString |> flip (++) "," |> text

                    NumericEValue s ->
                        s |> toString |> flip (++) "," |> text

                    BoolEValue s ->
                        s |> toString |> flip (++) "," |> text

                    ObjectEValue _ ->
                        "{" |> text

                    -- walkValue prop
                    ArrayEValue _ ->
                        "[" |> text

                    _ ->
                        empty
              , el DataRowHint
                    [ width <| fill 1
                    , onClick <| InsertValue isEditableProp path index id
                    ]
                    (text "")
              ]
                |> row None []
            , case prop of
                ObjectEValue _ ->
                    walkValue prop (path ++ [ name ])

                ArrayEValue _ ->
                    walkValue prop (path ++ [ name ])

                _ ->
                    empty
            , case prop of
                ObjectEValue _ ->
                    "},"
                        |> text
                        |> el DataRowHint [ width <| fill 1, inlineStyle [ ( "padding-left", "4ch" ) ] ]

                ArrayEValue _ ->
                    "],"
                        |> text
                        |> el DataRowHint [ width <| fill 1, inlineStyle [ ( "padding-left", "4ch" ) ] ]

                _ ->
                    empty
            ]

        walkValue v path =
            case v of
                ObjectEValue props ->
                    props
                        |> List.indexedMap (\index ( name, prop ) -> itemRow True index name prop)
                        |> List.concat
                        |> column None [ inlineStyle [ ( "padding-left", "4ch" ) ], Attributes.class "properties-block" ]

                ArrayEValue list ->
                    list
                        |> List.indexedMap (\index prop -> itemRow True index (index |> toString) prop)
                        |> List.concat
                        |> column None [ inlineStyle [ ( "padding-left", "4ch" ) ], Attributes.class "properties-block" ]

                _ ->
                    empty
    in
        walkValue val []


form_ : String -> Dict String String -> ( String, Int ) -> String -> String -> JsonValue -> List String -> List View
form_ id valueUpdateErrors editPropertyName editPath editValue val path =
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

                multilineEdit editValue =
                    editValue
                        |> Element.textArea JsonEditor
                            [ onInput <| ValueChange jsp
                            , onBlur StopEditing
                            , Attributes.width <| fill 1
                            , Attributes.rows <| (+) 1 <| List.length <| String.split "\n" editValue
                            , inlineStyle [ ( "display", "inline-block" ) ]
                            , Attributes.tabindex 0
                            , Attributes.id valId
                            ]
                        |> Element.el None
                            [ inlineStyle
                                [ ( "display", "block" )
                                , ( "margin-left", (level + 1 |> (*) 2 |> toString) ++ "ch" )
                                ]
                            ]
                        |> Element.below
                            [ valueUpdateErrors
                                |> Dict.get jsp
                                |> Maybe.map (text >> (el InlineError []))
                                |> Maybe.withDefault empty
                            ]

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
                        if isEditing then
                            [ editValue |> multilineEdit ]
                        else
                            list
                                |> List.indexedMap (\index item -> ( index, "", item ))
                                |> joinWithCommaAndWrapWith jsp valId val "[" "]" False level path

                    ObjectValue obj ->
                        if isEditing then
                            [ editValue |> multilineEdit ]
                        else
                            obj
                                |> List.indexedMap (\index ( key, val ) -> ( index, key, val ))
                                |> joinWithCommaAndWrapWith jsp valId val "{" "}" True level path

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
                            )
                        ]

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
