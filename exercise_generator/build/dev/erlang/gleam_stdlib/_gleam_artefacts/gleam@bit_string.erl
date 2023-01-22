-module(gleam@bit_string).
-compile(no_auto_import).

-export([from_string/1, byte_size/1, append/2, slice/3, is_utf8/1, to_string/1, concat/1]).

-spec from_string(binary()) -> bitstring().
from_string(X) ->
    gleam_stdlib:identity(X).

-spec byte_size(bitstring()) -> integer().
byte_size(X) ->
    erlang:byte_size(X).

-spec append(bitstring(), bitstring()) -> bitstring().
append(First, Second) ->
    concat([First, Second]).

-spec slice(bitstring(), integer(), integer()) -> {ok, bitstring()} |
    {error, nil}.
slice(String, Position, Length) ->
    gleam_stdlib:bit_string_slice(String, Position, Length).

-spec is_utf8(bitstring()) -> boolean().
is_utf8(Bits) ->
    do_is_utf8(Bits).

-spec do_is_utf8(bitstring()) -> boolean().
do_is_utf8(Bits) ->
    case Bits of
        <<>> ->
            true;

        <<_@1/utf8, Rest/binary>> ->
            do_is_utf8(Rest);

        _@2 ->
            false
    end.

-spec to_string(bitstring()) -> {ok, binary()} | {error, nil}.
to_string(Bits) ->
    do_to_string(Bits).

-spec do_to_string(bitstring()) -> {ok, binary()} | {error, nil}.
do_to_string(Bits) ->
    case is_utf8(Bits) of
        true ->
            {ok, gleam_stdlib:identity(Bits)};

        false ->
            {error, nil}
    end.

-spec concat(list(bitstring())) -> bitstring().
concat(Bit_strings) ->
    gleam_stdlib:bit_string_concat(Bit_strings).
