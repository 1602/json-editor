module EditableJsonValue exposing (EditableJsonValue(..), makeEditableJsonValue, makeJsonValue)

import Json.Schema.Definitions as Schema
    exposing
        ( JsonValue
            ( ObjectValue
            , ArrayValue
            , BooleanValue
            , NullValue
            , NumericValue
            , StringValue
            )
        )


type EditableJsonValue
    = EmptyValue
    | DeletedValue EditableJsonValue
    | StringEValue String
    | NumericEValue Float
    | BoolEValue Bool
    | ObjectEValue (List ( String, EditableJsonValue ))
    | ArrayEValue (List EditableJsonValue)
    | NullEValue


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
