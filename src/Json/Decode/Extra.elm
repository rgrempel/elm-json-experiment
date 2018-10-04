module Json.Decode.Extra exposing (default, defaultAt, with)

{-|


# Json.Decode.Extra

Experimental API for building JSON decoders.


## Decoding fields

@docs default, defaultAt, with

-}

import Json.Decode as Decode exposing (Decoder)


{-| Given a decoder, apply it and then continue on to decode other things.

    type alias User =
        { id : Int
        , followers : Int
        , email : String
        }

    userDecoder : Decoder User
    userDecoder =
        with (field int "id") <| \id ->
        with (field int "followers") <| \followers ->
        with (field string "email") <| \email ->
        succeed { id = id, followers = followers, email = email }

    result : Result String User
    result =
        Decode.decodeString
            userDecoder
            """
            {"id": 123, "email": "sam@example.com", "followers": 42 }
            """


    --> Ok { id = 123, followers = 42, email = "sam@example.com" }

-}
with : Decoder a -> (a -> Decoder b) -> Decoder b
with decoder callback =
    Decode.andThen callback decoder


{-| Decode a field that may be missing or have a null value. If the field is
missing, then it decodes as the `fallback` value. If the field is present,
then `valDecoder` is used to decode its value. If `valDecoder` fails on a
`null` value, then the `fallback` is used as if the field were missing
entirely.

    type alias User =
        { id : Int
        , followers : Int
        , email : String
        }

    userDecoder : Decoder User
    userDecoder =
        with (field int "id") <| \id ->
        with (default 0 int "followers") <| \followers ->
        with (field string "email") <| \email ->
        succeed { id = id, followers = followers, email = email }

    result : Result String User
    result =
        Decode.decodeString
            userDecoder
            """
            {"id": 123, "email": "sam@example.com" }
            """


    --> Ok { id = 123, followers = 0, email = "sam@example.com" }

Because `valDecoder` is given an opportunity to decode `null` values before
resorting to the `fallback`, you can distinguish between missing and `null`
values if you need to:

    userDecoder : Decoder User
    userDecoder =
        with (field int "id") <| \id ->
        with (default 0 (oneOf [ int, null 0 ]) "followers") <| \followers ->
        with (field string "email") <| \email ->
        succeed { id = id, followers = followers, email = email }

-}
default : String -> Decoder a -> a -> Decoder a
default fieldName valDecoder defaultVal =
    optionalDecoder (Decode.field fieldName Decode.value) valDecoder defaultVal


{-| Decode an optional nested field.

This is the same as `default` except it uses `Json.Decode.at` in place of
`Json.Decode.field`.

-}
defaultAt : List String -> Decoder a -> a -> Decoder a
defaultAt path valDecoder defaultVal =
    optionalDecoder (Decode.at path Decode.value) valDecoder defaultVal


optionalDecoder : Decoder Decode.Value -> Decoder a -> a -> Decoder a
optionalDecoder pathDecoder valDecoder fallback =
    let
        nullOr decoder =
            Decode.oneOf [ decoder, Decode.null fallback ]

        handleResult input =
            case Decode.decodeValue pathDecoder input of
                Ok rawValue ->
                    -- The field was present, so now let's try to decode that value.
                    -- (If it was present but fails to decode, this should and will fail!)
                    case Decode.decodeValue (nullOr valDecoder) rawValue of
                        Ok finalResult ->
                            Decode.succeed finalResult

                        Err finalErr ->
                            -- TODO is there some way to preserve the structure
                            -- of the original error instead of using toString here?
                            Decode.fail (Decode.errorToString finalErr)

                Err _ ->
                    -- The field was not present, so use the fallback.
                    Decode.succeed fallback
    in
    Decode.value
        |> Decode.andThen handleResult
