module Tests exposing (all, expectErr, isError, runWith)

import Expect exposing (Expectation)
import Json.Decode as Decode exposing (Decoder, at, field, null, string, succeed)
import Json.Decode.Extra
    exposing
        ( default
        , defaultAt
        , with
        )
import Test exposing (..)


{-| Run some JSON through a Decoder and return the result.
-}
runWith : String -> Decoder a -> Result String a
runWith str decoder =
    Decode.decodeString decoder str
        |> Result.mapError Decode.errorToString


isError : Result err ok -> Bool
isError result =
    case result of
        Err _ ->
            True

        Ok _ ->
            False


expectErr : Result err ok -> Expectation
expectErr result =
    isError result
        |> Expect.true ("Expected an Err but got " ++ Debug.toString result)


all : Test
all =
    describe
        "Json.Decode.Pipeline"
        [ test "should decode basic example" <|
            \() ->
                (with (field "a" string) <| \a ->
                 with (field "b" string) <| \b ->
                 succeed ( a, b )
                )
                    |> runWith """{"a":"foo","b":"bar"}"""
                    |> Expect.equal (Ok ( "foo", "bar" ))
        , test "should decode requireAt fields" <|
            \() ->
                (with (at [ "a" ] string) <| \a ->
                 with (at [ "b", "c" ] string) <| \b ->
                 succeed ( a, b )
                )
                    |> runWith """{"a":"foo","b":{"c":"bar"}}"""
                    |> Expect.equal (Ok ( "foo", "bar" ))
        , test "should decode defaultAt fields" <|
            \() ->
                (with (defaultAt [ "a", "b" ] string "--") <| \a ->
                 with (defaultAt [ "x", "y" ] string "--") <| \b ->
                 succeed ( a, b )
                )
                    |> runWith """{"a":{},"x":{"y":"bar"}}"""
                    |> Expect.equal (Ok ( "--", "bar" ))
        , test "default succeeds if the field is not present" <|
            \() ->
                (with (default "a" string "--") <| \a ->
                 with (default "x" string "--") <| \b ->
                 succeed ( a, b )
                )
                    |> runWith """{"x":"five"}"""
                    |> Expect.equal (Ok ( "--", "five" ))
        , test "default succeeds with fallback if the field is present but null" <|
            \() ->
                (with (default "a" string "--") <| \a ->
                 with (default "x" string "--") <| \b ->
                 succeed ( a, b )
                )
                    |> runWith """{"a":null,"x":"five"}"""
                    |> Expect.equal (Ok ( "--", "five" ))
        , test "default succeeds with result of the given decoder if the field is null and the decoder decodes nulls" <|
            \() ->
                (with (default "a" (null "null") "--") <| \a ->
                 with (default "x" string "--") <| \b ->
                 succeed ( a, b )
                )
                    |> runWith """{"a":null,"x":"five"}"""
                    |> Expect.equal (Ok ( "null", "five" ))
        , test "default fails if the field is present but doesn't decode" <|
            \() ->
                (with (default "a" string "--") <| \a ->
                 with (default "x" string "--") <| \b ->
                 succeed ( a, b )
                )
                    |> runWith """{"x":5}"""
                    |> expectErr
        , test "defaultAt fails if the field is present but doesn't decode" <|
            \() ->
                (with (defaultAt [ "a", "b" ] string "--") <| \a ->
                 with (defaultAt [ "x", "y" ] string "--") <| \b ->
                 succeed ( a, b )
                )
                    |> runWith """{"a":{},"x":{"y":5}}"""
                    |> expectErr
        ]
