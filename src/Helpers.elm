module Helpers exposing (deleteIn, getJsonValue, setJsonValue, setPropertyNameInJsonValue)

import Json.Decode as Decode exposing (Value, decodeValue, decodeString)
import EditableJsonValue exposing (EditableJsonValue(..))


deleteIn : Bool -> Bool -> EditableJsonValue -> List String -> Result String EditableJsonValue
deleteIn isDelete isRecoverable hostValue p =
    let
        ( key, path ) =
            p
                |> List.reverse
                |> \x ->
                    case x of
                        k :: revPath ->
                            ( Just k, List.reverse revPath )

                        [] ->
                            ( Nothing, [] )

        newJsonPointer =
            makeJsonPointer path

        applyAction val =
            if isRecoverable then
                case val of
                    DeletedValue x ->
                        if isDelete then
                            DeletedValue x
                        else
                            x

                    x ->
                        DeletedValue x
            else
                PermanentlyDeletedValue

        rejectKey key val =
            case val of
                ObjectEValue res ->
                    res
                        |> List.map
                            (\( k, v ) ->
                                if k == key then
                                    ( k, applyAction v )
                                else
                                    ( k, v )
                            )
                        |> (ObjectEValue >> Ok)

                ArrayEValue res ->
                    res
                        |> List.indexedMap
                            (\ind v ->
                                if toString ind == key then
                                    applyAction v
                                else
                                    v
                            )
                        |> (ArrayEValue >> Ok)

                _ ->
                    Err "It is not possible to delete key when host value is not object or array"

        targetValue =
            case key of
                Just k ->
                    hostValue
                        |> getJsonValue path
                        |> Result.andThen (rejectKey k)
                        |> Result.withDefault hostValue

                Nothing ->
                    hostValue
    in
        setJsonValue hostValue newJsonPointer targetValue


getJsonValue : List String -> EditableJsonValue -> Result String EditableJsonValue
getJsonValue path value =
    case path of
        [] ->
            Ok value

        head :: tail ->
            case value of
                ObjectEValue v ->
                    v
                        |> List.foldl
                            (\( key, v ) res ->
                                if res /= Nothing then
                                    res
                                else if key == head then
                                    Just v
                                else
                                    Nothing
                            )
                            Nothing
                        |> Result.fromMaybe ("Key not found: " ++ (toString path))
                        |> Result.andThen (getJsonValue tail)

                ArrayEValue v ->
                    head
                        |> String.toInt
                        |> Result.andThen
                            (\index ->
                                v
                                    |> List.drop index
                                    |> List.head
                                    |> Result.fromMaybe "Index is too big"
                            )
                        |> Result.andThen (getJsonValue tail)

                _ ->
                    Err "You are trying to access property of something that is not object or array"


setJsonValue : EditableJsonValue -> String -> EditableJsonValue -> Result String EditableJsonValue
setJsonValue hostValue jsonPath valueToSet =
    let
        path =
            jsonPath
                |> parseJsonPointer
                |> List.reverse

        newValue =
            case path of
                [] ->
                    Ok valueToSet

                _ :: subpath ->
                    path
                        |> List.foldl
                            (\key ( path, value ) ->
                                let
                                    p =
                                        List.reverse path

                                    v =
                                        value
                                            |> Result.andThen
                                                (\vv ->
                                                    hostValue
                                                        |> getJsonValue p
                                                        |> Result.andThen (setPropertyInJsonValue key vv)
                                                )
                                in
                                    case path of
                                        [] ->
                                            ( [], v )

                                        _ :: tail ->
                                            ( tail, v )
                            )
                            ( subpath, Ok valueToSet )
                        |> \( _, v ) -> v
    in
        newValue


parseJsonPointer : String -> List String
parseJsonPointer subpath =
    subpath
        |> String.split "/"
        |> List.drop 1


makeJsonPointer : List String -> String
makeJsonPointer path =
    ("#" :: path)
        |> String.join "/"


setPropertyInJsonValue : String -> EditableJsonValue -> EditableJsonValue -> Result String EditableJsonValue
setPropertyInJsonValue key value object =
    let
        updateOrAppend list =
            if List.any (\( k, _ ) -> k == key) list then
                list
                    |> List.map
                        (\( k, v ) ->
                            if k == key then
                                ( key, value )
                            else
                                ( k, v )
                        )
            else
                list ++ [ ( key, value ) ]
    in
        case object of
            ObjectEValue o ->
                o
                    |> updateOrAppend
                    |> ObjectEValue
                    |> Ok

            ArrayEValue list ->
                let
                    index =
                        key
                            |> decodeString Decode.int
                            |> Result.withDefault (List.length list)
                in
                    if List.length list > index then
                        list
                            |> List.indexedMap
                                (\i v ->
                                    if i == index then
                                        value
                                    else
                                        v
                                )
                            |> ArrayEValue
                            |> Ok
                    else
                        list
                            ++ [ value ]
                            |> ArrayEValue
                            |> Ok

            _ ->
                if key == "0" then
                    [ value ]
                        |> ArrayEValue
                        |> Ok
                else
                    [ ( key, value ) ]
                        |> ObjectEValue
                        |> Ok


setPropertyNameInJsonValue : ( String, Int ) -> String -> EditableJsonValue -> Result String EditableJsonValue
setPropertyNameInJsonValue ( jsonPointer, index ) newName hostValue =
    let
        renameKey val =
            case val of
                ObjectEValue v ->
                    v
                        |> List.indexedMap
                            (\i ( k, v ) ->
                                ( if index == i then
                                    newName
                                  else
                                    k
                                , v
                                )
                            )
                        |> ObjectEValue
                        >> Ok

                _ ->
                    Err "Can not rename property of this json value"

        targetValue =
            hostValue
                |> getJsonValue (parseJsonPointer jsonPointer)
                |> Result.andThen renameKey
                |> Result.withDefault hostValue
    in
        setJsonValue hostValue jsonPointer targetValue
