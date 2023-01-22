-module(gleam@json).
-compile(no_auto_import).

-export([decode/2, decode_bits/2, to_string/1, to_string_builder/1, string/1, bool/1, int/1, float/1, null/0, nullable/2, object/1, array/2, preprocessed_array/1]).
-export_type([json/0, decode_error/0]).

-type json() :: any().

-type decode_error() :: unexpected_end_of_input |
    {unexpected_byte, binary(), integer()} |
    {unexpected_sequence, binary(), integer()} |
    {unexpected_format, list(gleam@dynamic:decode_error())}.

-spec decode(
    binary(),
    fun((gleam@dynamic:dynamic()) -> {ok, EDC} |
        {error, list(gleam@dynamic:decode_error())})
) -> {ok, EDC} | {error, decode_error()}.
decode(Json, Decoder) ->
    do_decode(Json, Decoder).

-spec do_decode(
    binary(),
    fun((gleam@dynamic:dynamic()) -> {ok, EDG} |
        {error, list(gleam@dynamic:decode_error())})
) -> {ok, EDG} | {error, decode_error()}.
do_decode(Json, Decoder) ->
    Bits = gleam@bit_string:from_string(Json),
    decode_bits(Bits, Decoder).

-spec decode_bits(
    bitstring(),
    fun((gleam@dynamic:dynamic()) -> {ok, EDK} |
        {error, list(gleam@dynamic:decode_error())})
) -> {ok, EDK} | {error, decode_error()}.
decode_bits(Json, Decoder) ->
    case gleam_json_ffi:decode(Json) of
        {error, _try} -> {error, _try};
        {ok, Dynamic_value} ->
            _pipe = Decoder(Dynamic_value),
            gleam@result:map_error(_pipe, fun(A) -> {unexpected_format, A} end)
    end.

-spec to_string(json()) -> binary().
to_string(Json) ->
    gleam_json_ffi:json_to_string(Json).

-spec to_string_builder(json()) -> gleam@string_builder:string_builder().
to_string_builder(Json) ->
    gleam_json_ffi:json_to_iodata(Json).

-spec string(binary()) -> json().
string(Input) ->
    gleam_json_ffi:string(Input).

-spec bool(boolean()) -> json().
bool(Input) ->
    gleam_json_ffi:bool(Input).

-spec int(integer()) -> json().
int(Input) ->
    gleam_json_ffi:int(Input).

-spec float(float()) -> json().
float(Input) ->
    gleam_json_ffi:float(Input).

-spec null() -> json().
null() ->
    gleam_json_ffi:null().

-spec nullable(gleam@option:option(EDQ), fun((EDQ) -> json())) -> json().
nullable(Input, Inner_type) ->
    case Input of
        {some, Value} ->
            Inner_type(Value);

        none ->
            null()
    end.

-spec object(list({binary(), json()})) -> json().
object(Entries) ->
    gleam_json_ffi:object(Entries).

-spec array(list(EDU), fun((EDU) -> json())) -> json().
array(Entries, Inner_type) ->
    _pipe = Entries,
    _pipe@1 = gleam@list:map(_pipe, Inner_type),
    preprocessed_array(_pipe@1).

-spec preprocessed_array(list(json())) -> json().
preprocessed_array(From) ->
    gleam_json_ffi:array(From).
