module JsonInput exposing (Model, update, view, init, getValue, ExternalMsg(Select, OnInput), Msg(SetEditableValue))

import Html exposing (Html)
import List.Extra
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
            , PropertiesBlock
            , ItemIndex
            , PropertyValue
            , PropertySeparator
            , DataRowHint
            )
        , Variations(Active)
        , stylesheet
        )
import Element.Events exposing (onCheck, onClick, onMouseDown, onInput, onBlur, onFocus, onDoubleClick)
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


-- import Markdown

import Json.Decode as Decode exposing (Decoder, decodeString, decodeValue, Value)
import Json.Encode as Encode
import Helpers
    exposing
        ( deleteIn
        , getJsonValue
        , setJsonValue
        , setPropertyNameInJsonValue
        , parseJsonPointer
        , makeJsonPointer
        )
import JsonValue exposing (JsonValue(ObjectValue, ArrayValue))
import Json.Schema as JS
import Json.Schema.Definitions as Schema
    exposing
        ( Schema(BooleanSchema, ObjectSchema)
        , SubSchema
        , Schemata(Schemata)
        , Type(AnyType, SingleType, NullableType, UnionType)
        , SingleType(IntegerType, NumberType, StringType, BooleanType)
        )
import EditableJsonValue
    exposing
        ( EditableJsonValue
            ( ObjectEValue
            , ArrayEValue
            , BoolEValue
            , NullEValue
            , NumericEValue
            , StringEValue
            , EmptyValue
            , DeletedValue
            , PermanentlyDeletedValue
            )
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
    | Delete (List String) Bool
    | SetEditPath String String String
    | SetEditValuePath String String
    | SetEditPropertyName String (List String) Int
    | StopEditing String
    | SetPropertyName String


init : Schema -> Value -> Model
init schema val =
    let
        jsonValue =
            val
                |> Decode.decodeValue JsonValue.decoder
                |> Result.withDefault (ObjectValue [])

        editableJsonValue =
            jsonValue
                |> EditableJsonValue.makeEditableJsonValue
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
                |> JsonValue.encode
    in
        schema
            |> JS.validateValue val
            |> Result.map (\_ -> val)
            |> Result.andThen (Decode.decodeValue Schema.decoder)


getValue : Model -> Value
getValue model =
    model.jsonValue
        |> JsonValue.encode


updateValue : Model -> String -> Result String EditableJsonValue -> ( Model, ExternalMsg )
updateValue model path newStuff =
    let
        addError message =
            { model | valueUpdateErrors = model.valueUpdateErrors |> Dict.insert path message }
    in
        newStuff
            |> Result.andThen (setJsonValue model.editableJsonValue path)
            |> \res ->
                case res of
                    Ok v ->
                        let
                            vv =
                                EditableJsonValue.makeJsonValue v
                        in
                            case makeValidSchema vv model.schema of
                                Ok _ ->
                                    ( { model
                                        | editableJsonValue = v
                                        , jsonValue = vv
                                        , valueUpdateErrors = model.valueUpdateErrors |> Dict.remove path
                                      }
                                    , OnInput
                                    )

                                Err message ->
                                    ( { model
                                        | editableJsonValue = v
                                        , jsonValue = vv
                                        , valueUpdateErrors = model.valueUpdateErrors |> Dict.insert path message
                                      }
                                    , OnInput
                                    )

                    Err s ->
                        ( addError s, NoOp )


deletePath : Model -> Bool -> List String -> Model
deletePath model isChecked path =
    case deleteIn (not isChecked) True model.editableJsonValue path of
        Ok val ->
            { model | editableJsonValue = val, jsonValue = val |> EditableJsonValue.makeJsonValue }

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

        SetEditValuePath id jsonPointer ->
            ( { model
                | editPath = jsonPointer
                , editPropertyName = ( "", 0 )
                , editValue =
                    model.editableJsonValue
                        |> getJsonValue (parseJsonPointer jsonPointer)
                        |> Result.withDefault model.editableJsonValue
                        |> EditableJsonValue.makeJsonValue
                        |> JsonValue.encode
                        |> Encode.encode 4
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
                |> decodeString JsonValue.decoder
                |> Result.map EditableJsonValue.makeEditableJsonValue
                |> updateValue { model | editValue = str, editPath = path } path

        StopEditing propName ->
            let
                ( editPropPath, _ ) =
                    model.editPropertyName
            in
                ( { model
                    | editPath = ""
                    , editPropertyName = ( "", 0 )
                    , editableJsonValue =
                        if (model.editValue == "" || model.editValue == "∅") && model.editPath /= "" then
                            deleteIn True False model.editableJsonValue (parseJsonPointer model.editPath)
                                |> Result.withDefault model.editableJsonValue
                        else if editPropPath /= "" && propName == "" then
                            deleteIn True False model.editableJsonValue (parseJsonPointer (editPropPath ++ "/"))
                                |> Result.withDefault model.editableJsonValue
                        else
                            model.editableJsonValue
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
                        , formId ++ "/val/" ++ newJsonPointer ++ "/" ++ (toString index)
                        )

                obj =
                    model.editableJsonValue
                        |> getJsonValue path
                        |> Result.map
                            (\x ->
                                case x of
                                    ObjectEValue props ->
                                        (List.take index props)
                                            ++ [ ( "", EmptyValue ) ]
                                            ++ (List.drop index props)
                                            |> ObjectEValue

                                    ArrayEValue items ->
                                        (List.take index items)
                                            ++ [ EmptyValue ]
                                            ++ (List.drop index items)
                                            |> ArrayEValue

                                    x ->
                                        x
                            )

                ( updatedModel, _ ) =
                    updateValue
                        { model
                            | editPath = editPath
                            , editPropertyName = ( editProp, index )
                            , editValue = ""
                        }
                        newJsonPointer
                        obj
            in
                ( updatedModel
                , Select id
                )

        SetEditableValue s ->
            ( { model
                | jsonValue =
                    s
                        |> Decode.decodeValue JsonValue.decoder
                        |> Result.withDefault (ObjectValue [])
              }
            , NoOp
            )

        Delete path isDelete ->
            ( deletePath model isDelete path, NoOp )

        SetEditPropertyName id path index ->
            ( { model
                | editPropertyName = ( makeJsonPointer path, index )
                , editPath = ""
              }
            , Select id
            )

        SetPropertyName str ->
            ( { model
                | editableJsonValue =
                    model.editableJsonValue
                        |> setPropertyNameInJsonValue model.editPropertyName str
                        |> Result.withDefault model.editableJsonValue
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

        propName isEditableProp name index path val =
            let
                hostPointer =
                    makeJsonPointer path

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
                case val of
                    DeletedValue _ ->
                        name |> text |> el PropertyName []

                    _ ->
                        if not isEditableProp then
                            name
                                |> text
                                |> el ItemIndex []
                        else if hostPointer == editPropPath && index == editIndex then
                            name
                                |> Element.inputText JsonEditor
                                    [ onInput <| SetPropertyName
                                    , Attributes.size <| String.length name + 1
                                    , onBlur <| StopEditing name
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

        itemRow isEditableProp index name prop isLast path =
            let
                newPath =
                    case name of
                        Just n ->
                            path ++ [ n ]

                        Nothing ->
                            path

                newPointer =
                    makeJsonPointer newPath

                isDeleted =
                    case prop of
                        DeletedValue _ ->
                            True

                        _ ->
                            False

                valId =
                    id ++ "/val/" ++ newPointer

                isEditing =
                    newPointer == editPath

                multilineEdit _ =
                    editValue
                        |> Element.textArea JsonEditor
                            [ onInput <| ValueChange newPointer
                            , onBlur <| StopEditing ""
                            , Attributes.width <| fill 1
                            , editValue
                                |> String.split "\n"
                                |> List.length
                                |> (+) 1
                                |> Attributes.rows
                            , inlineStyle [ ( "margin-left", "4ch" ) ]
                            , Attributes.tabindex 0
                            , Attributes.id valId
                            ]
                        |> Element.el None []
                        |> Element.below
                            [ valueUpdateErrors
                                |> Dict.get newPointer
                                |> Maybe.map (text >> (el InlineError []))
                                |> Maybe.withDefault empty
                            ]

                editForm _ =
                    editValue
                        |> Element.inputText JsonEditor
                            [ onInput <| ValueChange newPointer
                            , onBlur <| StopEditing ""
                            , Attributes.size <| String.length editValue + 1
                            , inlineStyle [ ( "display", "inline-block" ) ]
                            , Attributes.tabindex 0
                            , Attributes.id valId
                            ]
                        |> Element.el None
                            [ inlineStyle [ ( "display", "inline-block" ) ] ]
                        |> Element.below
                            [ valueUpdateErrors
                                |> Dict.get newPointer
                                |> Maybe.map (text >> (el InlineError []))
                                |> Maybe.withDefault empty
                            ]

                editable s =
                    if isEditing then
                        editForm 1
                    else
                        s
                            |> flip (++) ","
                            |> text
                            |> el PropertyValue
                                (if name /= Just "" then
                                    [ onFocus <| SetEditPath valId newPointer s
                                    , Attributes.tabindex 0
                                    ]
                                 else
                                    []
                                )
            in
                [ [ Element.checkbox (not isDeleted)
                        None
                        [ moveLeft 1
                        , inlineStyle [ ( "width", "4ch" ), ( "padding-left", "2ch" ) ]
                        , Attributes.class
                            (if isDeleted then
                                "restore-property"
                             else
                                "delete-property"
                            )
                        , onCheck <| Delete newPath
                        ]
                    <|
                        text ""
                  , if isEditableProp then
                        propName isEditableProp (name |> Maybe.withDefault "") index path prop
                    else
                        empty
                  , if isEditableProp then
                        ": "
                            |> text
                            |> el PropertySeparator []
                    else
                        empty
                  , case prop of
                        DeletedValue (StringEValue s) ->
                            s |> toString |> flip (++) "," |> text

                        StringEValue s ->
                            s
                                |> toString
                                |> editable

                        DeletedValue (NumericEValue s) ->
                            s |> toString |> flip (++) "," |> text

                        NumericEValue s ->
                            s |> toString |> editable

                        DeletedValue (BoolEValue s) ->
                            (if s then
                                "true"
                             else
                                "false"
                            )
                                |> flip (++) ","
                                |> text

                        BoolEValue s ->
                            (if s then
                                "true"
                             else
                                "false"
                            )
                                |> editable

                        NullEValue ->
                            "null"
                                |> editable

                        DeletedValue (ObjectEValue _) ->
                            "{...}" |> text

                        ObjectEValue _ ->
                            if isEditing then
                                empty
                            else
                                "{"
                                    |> text
                                    |> el PropertyValue [ onClick <| SetEditValuePath valId newPointer ]

                        DeletedValue (ArrayEValue _) ->
                            "[...]" |> text

                        ArrayEValue _ ->
                            if isEditing then
                                empty
                            else
                                "["
                                    |> text
                                    |> el PropertyValue [ onClick <| SetEditValuePath valId newPointer ]

                        EmptyValue ->
                            ""
                                |> editable

                        _ ->
                            empty
                  , if editPath == "" && editPropPath == "" || isLast && editValue /= "" then
                        case prop of
                            ArrayEValue x ->
                                el DataRowHint
                                    ((if List.length x == 0 then
                                        [ Attributes.tabindex 0 ]
                                      else
                                        []
                                     )
                                        ++ [ width <| fill 1
                                           , InsertValue False newPath 0 id
                                                |> \e ->
                                                    if List.length x == 0 && editValue /= "" then
                                                        onFocus e
                                                    else
                                                        onClick e
                                           ]
                                    )
                                    (text "")

                            ObjectEValue x ->
                                el DataRowHint
                                    ((if List.length x == 0 then
                                        [ Attributes.tabindex 0 ]
                                      else
                                        []
                                     )
                                        ++ [ width <| fill 1
                                           , InsertValue True newPath 0 id
                                                |> \e ->
                                                    if List.length x == 0 && editValue /= "" then
                                                        onFocus e
                                                    else
                                                        onClick e
                                           ]
                                    )
                                    (text "")

                            _ ->
                                el DataRowHint
                                    ((if isLast then
                                        [ Attributes.tabindex 0 ]
                                      else
                                        []
                                     )
                                        ++ [ width <| fill 1
                                           , InsertValue isEditableProp path (index + 1) id
                                                |> \x ->
                                                    if isLast && editValue /= "" then
                                                        onFocus x
                                                    else
                                                        onClick x
                                           ]
                                    )
                                    (text "")
                    else
                        empty
                  ]
                    |> row None
                        [ Attributes.class "item-row"
                        , inlineStyle
                            [ ( "text-decoration"
                              , if isDeleted then
                                    "line-through"
                                else
                                    "none"
                              )
                            , ( "color", "grey" )
                            ]
                        ]
                , case prop of
                    ObjectEValue _ ->
                        if isEditing then
                            multilineEdit 1
                        else
                            walkValue prop newPath

                    ArrayEValue _ ->
                        if isEditing then
                            multilineEdit 1
                        else
                            walkValue prop newPath

                    _ ->
                        empty
                , case prop of
                    ObjectEValue _ ->
                        if isEditing then
                            empty
                        else if editPath == "" && editPropPath == "" then
                            "},"
                                |> text
                                |> el DataRowHint
                                    [ width <| fill 1
                                    , inlineStyle [ ( "padding-left", "4ch" ) ]
                                    , onClick <| InsertValue isEditableProp path (index + 1) id
                                    ]
                        else
                            "},"
                                |> text
                                |> el None [ inlineStyle [ ( "padding-left", "4ch" ) ] ]

                    ArrayEValue _ ->
                        if isEditing then
                            empty
                        else if editPath == "" && editPropPath == "" then
                            "],"
                                |> text
                                |> el DataRowHint
                                    [ width <| fill 1
                                    , inlineStyle [ ( "padding-left", "4ch" ) ]
                                    , onClick <| InsertValue isEditableProp path (index + 1) id
                                    ]
                        else
                            "],"
                                |> text
                                |> el None [ inlineStyle [ ( "padding-left", "4ch" ) ] ]

                    _ ->
                        empty
                ]

        walkValue v path =
            case v of
                ObjectEValue props ->
                    let
                        lastIndex =
                            props
                                |> List.Extra.takeWhileRight
                                    (\( _, v ) ->
                                        v == PermanentlyDeletedValue
                                    )
                                |> List.length
                                |> (-) (List.length props - 1)
                    in
                        props
                            |> List.indexedMap
                                (\index ( name, prop ) ->
                                    if prop == PermanentlyDeletedValue then
                                        []
                                    else
                                        itemRow True index (Just name) prop (index == lastIndex) path
                                )
                            |> List.concat
                            |> column PropertiesBlock [ Attributes.class "properties-block" ]

                ArrayEValue list ->
                    let
                        lastIndex =
                            list
                                |> List.Extra.takeWhileRight
                                    ((==) PermanentlyDeletedValue)
                                |> List.length
                                |> (-) (List.length list - 1)
                    in
                        list
                            |> List.indexedMap
                                (\index prop ->
                                    if prop == PermanentlyDeletedValue then
                                        []
                                    else
                                        itemRow False index (index |> toString |> Just) prop (index == lastIndex) path
                                )
                            |> List.concat
                            |> column PropertiesBlock [ Attributes.class "properties-block" ]

                _ ->
                    empty
    in
        itemRow False 0 Nothing val True []
            |> column None []
