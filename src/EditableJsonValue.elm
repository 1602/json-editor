module EditableJsonValue exposing (EditableJsonValue(..), makeEditableJsonValue, makeJsonValue)

import JsonValue
    exposing
        ( JsonValue
            ( ObjectValue
            , ArrayValue
            , BoolValue
            , NullValue
            , NumericValue
            , StringValue
            )
        )


type EditableJsonValue
    = EmptyValue
    | DeletedValue EditableJsonValue
    | PermanentlyDeletedValue
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

        BoolValue b ->
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
            BoolValue b

        NullEValue ->
            NullValue

        EmptyValue ->
            NullValue

        DeletedValue _ ->
            NullValue

        PermanentlyDeletedValue ->
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

                            PermanentlyDeletedValue ->
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

                            PermanentlyDeletedValue ->
                                False

                            _ ->
                                True
                    )
                |> List.map makeJsonValue
                |> ArrayValue
