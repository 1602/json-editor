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
import Markdown
import Json.Decode as Decode exposing (Decoder, decodeString, decodeValue, Value)
import Json.Encode as Encode
import Json.Schema.Helpers
    exposing
        ( implyType
        , typeToString
        , for
        , whenObjectSchema
        , parseJsonPointer
        , makeJsonPointer
        , resolve
        , calcSubSchemaType
        )
import Helpers
    exposing
        ( deleteIn
        , getJsonValue
        , setJsonValue
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
    | SetEditPropertyName String (List String) Int
    | StopEditing
    | SetPropertyName String


init : Schema -> Value -> Model
init schema val =
    let
        jsonValue =
            val
                |> Decode.decodeValue jsonValueDecoder
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
                |> Result.map EditableJsonValue.makeEditableJsonValue
                |> Debug.log "haha"
                |> updateValue { model | editValue = str, editPath = path } path

        StopEditing ->
            ( { model
                | editPath = ""
                , editPropertyName = ( "", 0 )
                , editableJsonValue =
                    if (model.editValue == "" || model.editValue == "∅") && model.editPath /= "" then
                        deleteIn True False model.editableJsonValue (parseJsonPointer model.editPath)
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
                        |> Decode.decodeValue jsonValueDecoder
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
                | editableJsonValue =
                    model.editableJsonValue
                        |> setPropertyNameInJsonValue model.editPropertyName str
                        |> Result.withDefault model.editableJsonValue
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

        itemRow isEditableProp index name prop isLast path =
            let
                newPath =
                    path ++ [ name ]

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

                editForm _ =
                    editValue
                        |> Element.inputText JsonEditor
                            [ onInput <| ValueChange newPointer
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
                                [ onFocus <| SetEditPath valId newPointer s
                                , Attributes.tabindex 0
                                ]
            in
                [ [ Element.checkbox (not isDeleted)
                        None
                        [ moveLeft 2
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
                        propName isEditableProp name index path prop
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

                        DeletedValue (ObjectEValue _) ->
                            "{...}" |> text

                        ObjectEValue _ ->
                            "{"
                                |> text
                                |> el PropertyValue []

                        DeletedValue (ArrayEValue _) ->
                            "[...]" |> text

                        ArrayEValue _ ->
                            "["
                                |> text
                                |> el PropertyValue []

                        EmptyValue ->
                            "∅"
                                |> editable

                        _ ->
                            empty
                  , case prop of
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
                            if editPath == "" && editPropPath == "" || isLast && editValue /= "∅" then
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
                        walkValue prop newPath

                    ArrayEValue _ ->
                        walkValue prop newPath

                    _ ->
                        empty
                , case prop of
                    ObjectEValue _ ->
                        if editPath == "" && editPropPath == "" then
                            "},"
                                |> text
                                |> el DataRowHint
                                    [ width <| fill 1
                                    , inlineStyle [ ( "padding-left", "4ch" ) ]
                                    , onClick <| InsertValue isEditableProp path (index + 1) id
                                    , Attributes.tabindex 0
                                    ]
                        else
                            "},"
                                |> text
                                |> el None [ inlineStyle [ ( "padding-left", "4ch" ) ] ]

                    ArrayEValue _ ->
                        if editPath == "" && editPropPath == "" then
                            "],"
                                |> text
                                |> el DataRowHint
                                    [ width <| fill 1
                                    , inlineStyle [ ( "padding-left", "4ch" ) ]
                                    , onClick <| InsertValue isEditableProp path (index + 1) id
                                    , Attributes.tabindex 0
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
                    props
                        |> List.indexedMap (\index ( name, prop ) -> itemRow True index name prop (index == (List.length props) - 1) path)
                        |> List.concat
                        |> column PropertiesBlock [ Attributes.class "properties-block" ]

                ArrayEValue list ->
                    list
                        |> List.indexedMap (\index prop -> itemRow False index (index |> toString) prop (index == (List.length list) - 1) path)
                        |> List.concat
                        |> column PropertiesBlock [ Attributes.class "properties-block" ]

                _ ->
                    empty
    in
        [ "{"
            |> text
            |> el None [ width <| fill 1, inlineStyle [ ( "padding-left", "4ch" ) ] ]
        , walkValue val []
        , "}"
            |> text
            |> el None [ width <| fill 1, inlineStyle [ ( "padding-left", "4ch" ) ] ]
        ]
            |> column None []


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
                    el None [] <|
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
