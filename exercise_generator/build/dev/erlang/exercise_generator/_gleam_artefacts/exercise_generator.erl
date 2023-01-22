-module(exercise_generator).
-compile(no_auto_import).

-export([main/0]).
-export_type([canonical_data/0, test_case/0, test_data/0, argument/0, function_/0, json_data/0]).

-type canonical_data() :: {canonical_data, list(binary()), list(test_case())}.

-type test_case() :: {single_test, test_data()} |
    {test_group, list(binary()), binary(), list(test_case())}.

-type test_data() :: {test_data,
        list(binary()),
        {ok, binary()} | {error, nil},
        binary(),
        binary(),
        gleam@map:map_(binary(), json_data()),
        json_data()}.

-type argument() :: {argument, binary(), json_data()}.

-type function_() :: {function,
        list(argument()),
        json_data(),
        boolean(),
        integer()}.

-type json_data() :: json_null |
    {json_bool, boolean()} |
    {json_int, integer()} |
    {json_float, float()} |
    {json_string, binary()} |
    {json_list, list(json_data())} |
    {json_object, gleam@map:map_(binary(), json_data())}.

-spec json_data_to_gleam_type(json_data()) -> binary().
json_data_to_gleam_type(Data) ->
    case Data of
        json_null ->
            <<"Result(unknown, Nil)"/utf8>>;

        {json_bool, _@1} ->
            <<"Bool"/utf8>>;

        {json_int, _@2} ->
            <<"Int"/utf8>>;

        {json_float, _@3} ->
            <<"Float"/utf8>>;

        {json_string, _@4} ->
            <<"String"/utf8>>;

        {json_list, []} ->
            <<"List(unknown)"/utf8>>;

        {json_list, [Head | _@5]} ->
            <<<<"List ("/utf8, (json_data_to_gleam_type(Head))/binary>>/binary,
                ")"/utf8>>;

        {json_object, _@6} ->
            <<"CustomRecordType"/utf8>>
    end.

-spec json_data_to_gleam_value(json_data()) -> binary().
json_data_to_gleam_value(Data) ->
    case Data of
        json_null ->
            <<"Nil"/utf8>>;

        {json_bool, true} ->
            <<"True"/utf8>>;

        {json_bool, false} ->
            <<"False"/utf8>>;

        {json_int, Int} ->
            gleam@int:to_string(Int);

        {json_float, Float} ->
            gleam@float:to_string(Float);

        {json_string, String} ->
            gleam@string:inspect(String);

        {json_list, List} ->
            <<<<"["/utf8,
                    (gleam@string:join(
                        gleam@list:map(List, fun json_data_to_gleam_value/1),
                        <<", "/utf8>>
                    ))/binary>>/binary,
                "]"/utf8>>;

        {json_object, Map} ->
            Args = begin
                _pipe = Map,
                _pipe@1 = gleam@map:to_list(_pipe),
                _pipe@2 = gleam@list:map(
                    _pipe@1,
                    fun(Arg) ->
                        <<<<(clean_variable(erlang:element(1, Arg)))/binary,
                                ": "/utf8>>/binary,
                            (json_data_to_gleam_value(erlang:element(2, Arg)))/binary>>
                    end
                ),
                gleam@string:join(_pipe@2, <<", "/utf8>>)
            end,
            <<<<"CustomRecordType("/utf8, Args/binary>>/binary, ")"/utf8>>
    end.

-spec main() -> nil.
main() ->
    [Slug@1, Canonical_data@1] = case gleam@erlang:start_arguments() of
        [Slug, Canonical_data] -> [Slug, Canonical_data];
        _try ->
            erlang:error(#{gleam_error => assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _try,
                        module => <<"exercise_generator"/utf8>>,
                        function => <<"main"/utf8>>,
                        line => 98})
    end,
    case gleam@json:decode(Canonical_data@1, canonical_data_decoder()) of
        {ok, Data} ->
            _@1 = write_solution_files(Slug@1, Data),
            _@2 = write_test_file(Slug@1, Data),
            gleam@io:println(<<"Files written"/utf8>>);

        {error, Errors} ->
            gleam@io:println(
                <<"Error decoding canonical data:\n"/utf8,
                    (gleam@string:inspect(Errors))/binary>>
            )
    end.

-spec kebab_to_snake(binary()) -> binary().
kebab_to_snake(Slug) ->
    gleam@string:replace(Slug, <<"-"/utf8>>, <<"_"/utf8>>).

-spec camel_to_snake(binary()) -> binary().
camel_to_snake(Variable) ->
    _pipe = Variable,
    _pipe@1 = gleam@string:to_graphemes(_pipe),
    _pipe@2 = gleam@list:map(
        _pipe@1,
        fun(Char) -> case gleam@string:lowercase(Char) /= Char of
                true ->
                    <<"_"/utf8, (gleam@string:lowercase(Char))/binary>>;

                false ->
                    Char
            end end
    ),
    gleam@string:concat(_pipe@2).

-spec clean_variable(binary()) -> binary().
clean_variable(Variable) ->
    Reserved_words = [<<"as"/utf8>>,
        <<"assert"/utf8>>,
        <<"case"/utf8>>,
        <<"const"/utf8>>,
        <<"external"/utf8>>,
        <<"fn"/utf8>>,
        <<"if"/utf8>>,
        <<"import"/utf8>>,
        <<"let"/utf8>>,
        <<"opaque"/utf8>>,
        <<"pub"/utf8>>,
        <<"todo"/utf8>>,
        <<"try"/utf8>>,
        <<"type"/utf8>>,
        <<"use"/utf8>>],
    Variable@1 = camel_to_snake(Variable),
    case gleam@list:contains(Reserved_words, Variable@1) of
        true ->
            <<Variable@1/binary, "_value"/utf8>>;

        false ->
            Variable@1
    end.

-spec write_solution_files(binary(), canonical_data()) -> {ok, nil} |
    {error, gleam@erlang@file:reason()}.
write_solution_files(Slug, Data) ->
    Functions = functions_to_implement(erlang:element(3, Data)),
    Content = begin
        _pipe = Functions,
        _pipe@1 = gleam@map:to_list(_pipe),
        _pipe@2 = gleam@list:sort(
            _pipe@1,
            fun(A, B) ->
                gleam@int:compare(
                    erlang:element(5, erlang:element(2, A)),
                    erlang:element(5, erlang:element(2, B))
                )
            end
        ),
        _pipe@5 = gleam@list:map(
            _pipe@2,
            fun(Item) ->
                {Name, {function, Arguments, Return_type, Can_error, _@1}} = Item,
                Return_type@1 = json_data_to_gleam_type(Return_type),
                Return = case Can_error of
                    true ->
                        <<<<"Result("/utf8, Return_type@1/binary>>/binary,
                            ", String)"/utf8>>;

                    false ->
                        Return_type@1
                end,
                Args = begin
                    _pipe@3 = Arguments,
                    _pipe@4 = gleam@list:map(
                        _pipe@3,
                        fun(Argument) ->
                            gleam@string:concat(
                                [clean_variable(erlang:element(2, Argument)),
                                    <<" "/utf8>>,
                                    clean_variable(erlang:element(2, Argument)),
                                    <<": "/utf8>>,
                                    json_data_to_gleam_type(
                                        erlang:element(3, Argument)
                                    )]
                            )
                        end
                    ),
                    gleam@string:join(_pipe@4, <<", "/utf8>>)
                end,
                <<<<<<<<<<<<"pub fn "/utf8, (clean_variable(Name))/binary>>/binary,
                                    "("/utf8>>/binary,
                                Args/binary>>/binary,
                            ") -> "/utf8>>/binary,
                        Return/binary>>/binary,
                    " {\n todo \n}"/utf8>>
            end
        ),
        gleam@string:join(_pipe@5, <<"\n"/utf8>>)
    end,
    Exercise = kebab_to_snake(Slug),
    Solution_path = gleam@string:join(
        [<<".."/utf8>>,
            <<"exercises"/utf8>>,
            <<"practice"/utf8>>,
            Slug,
            <<"src"/utf8>>,
            <<Exercise/binary, ".gleam"/utf8>>],
        <<"/"/utf8>>
    ),
    {ok, nil} = case gleam@erlang@file:write(Content, Solution_path) of
        {ok, nil} -> {ok, nil};
        _try ->
            erlang:error(#{gleam_error => assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _try,
                        module => <<"exercise_generator"/utf8>>,
                        function => <<"write_solution_files"/utf8>>,
                        line => 188})
    end,
    Example_path = gleam@string:join(
        [<<".."/utf8>>,
            <<"exercises"/utf8>>,
            <<"practice"/utf8>>,
            Slug,
            <<".meta"/utf8>>,
            <<"example.gleam"/utf8>>],
        <<"/"/utf8>>
    ),
    {ok, nil} = case gleam@erlang@file:write(Content, Example_path) of
        {ok, nil} -> {ok, nil};
        _try@1 ->
            erlang:error(#{gleam_error => assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _try@1,
                        module => <<"exercise_generator"/utf8>>,
                        function => <<"write_solution_files"/utf8>>,
                        line => 196})
    end.

-spec functions_to_implement(list(test_case())) -> gleam@map:map_(binary(), function_()).
functions_to_implement(Test_cases) ->
    gleam@list:fold(Test_cases, gleam@map:new(), fun check_test_case/2).

-spec check_test_case(gleam@map:map_(binary(), function_()), test_case()) -> gleam@map:map_(binary(), function_()).
check_test_case(Functions, Test_case) ->
    case Test_case of
        {test_group, _@1, _@2, Cases} ->
            gleam@list:fold(Cases, Functions, fun check_test_case/2);

        {single_test, {test_data, _@3, _@4, _@5, Function, Input, Expected}} ->
            Can_error = case Expected of
                {json_object, Object} ->
                    gleam@map:has_key(Object, <<"error"/utf8>>);

                _@6 ->
                    false
            end,
            Args = begin
                _pipe = Input,
                _pipe@1 = gleam@map:to_list(_pipe),
                gleam@list:map(
                    _pipe@1,
                    fun(Arg) ->
                        {argument,
                            erlang:element(1, Arg),
                            erlang:element(2, Arg)}
                    end
                )
            end,
            Current_function = case gleam@map:get(Functions, Function) of
                {ok, Func} ->
                    case Can_error of
                        true ->
                            erlang:setelement(4, Func, true);

                        false ->
                            erlang:setelement(3, Func, Expected)
                    end;

                {error, nil} ->
                    {function,
                        Args,
                        Expected,
                        Can_error,
                        gleam@map:size(Functions)}
            end,
            gleam@map:insert(Functions, Function, Current_function)
    end.

-spec write_test_file(binary(), canonical_data()) -> {ok, nil} |
    {error, gleam@erlang@file:reason()}.
write_test_file(Slug, Data) ->
    Exercise = kebab_to_snake(Slug),
    Functions = functions_to_implement(erlang:element(3, Data)),
    Comments = print_comments(erlang:element(2, Data)),
    Test_cases = print_tests(
        Slug,
        <<""/utf8>>,
        Functions,
        erlang:element(3, Data)
    ),
    Content = begin
        _pipe = <<"
  import gleeunit
  import gleeunit/should
  import <exercise>

  <comments>

  pub fn main() {
    gleeunit.main()
  }

  <test_cases>
  "/utf8>>,
        _pipe@1 = gleam@string:replace(_pipe, <<"<exercise>"/utf8>>, Exercise),
        _pipe@2 = gleam@string:replace(_pipe@1, <<"<comments>"/utf8>>, Comments),
        gleam@string:replace(_pipe@2, <<"<test_cases>"/utf8>>, Test_cases)
    end,
    Path = gleam@string:join(
        [<<".."/utf8>>,
            <<"exercises"/utf8>>,
            <<"practice"/utf8>>,
            Slug,
            <<"test"/utf8>>,
            <<Exercise/binary, "_test.gleam"/utf8>>],
        <<"/"/utf8>>
    ),
    {ok, nil} = case gleam@erlang@file:write(Content, Path) of
        {ok, nil} -> {ok, nil};
        _try ->
            erlang:error(#{gleam_error => assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _try,
                        module => <<"exercise_generator"/utf8>>,
                        function => <<"write_test_file"/utf8>>,
                        line => 271})
    end.

-spec print_comments(list(binary())) -> binary().
print_comments(Comments) ->
    _pipe = Comments,
    _pipe@1 = gleam@list:map(
        _pipe,
        fun(Comment) -> <<"// "/utf8, Comment/binary>> end
    ),
    gleam@string:join(_pipe@1, <<"\n"/utf8>>).

-spec print_tests(
    binary(),
    binary(),
    gleam@map:map_(binary(), function_()),
    list(test_case())
) -> binary().
print_tests(Slug, Prefix, Functions, Tests) ->
    _pipe = Tests,
    _pipe@1 = gleam@list:map(
        _pipe,
        fun(_capture) -> print_test(Slug, Prefix, Functions, _capture) end
    ),
    gleam@string:join(_pipe@1, <<"\n"/utf8>>).

-spec print_test(
    binary(),
    binary(),
    gleam@map:map_(binary(), function_()),
    test_case()
) -> binary().
print_test(Slug, Prefix, Functions, Test) ->
    case Test of
        {test_group, Comments, Description, Cases} ->
            Prefix@1 = <<<<<<Prefix/binary, ""/utf8>>/binary,
                    Description/binary>>/binary,
                "_"/utf8>>,
            Tests = print_tests(Slug, Prefix@1, Functions, Cases),
            <<<<(print_comments(Comments))/binary, "\n"/utf8>>/binary,
                Tests/binary>>;

        {single_test,
            {test_data,
                Comments@1,
                Reimplements,
                Description@1,
                Function,
                Input,
                Expected}} ->
            Exercise = kebab_to_snake(Slug),
            Comments@2 = case Reimplements of
                {ok, Uuid} ->
                    _pipe = [<<"This test reimplements the test with uuid "/utf8,
                            Uuid/binary>>,
                        <<"Please identify that test and remove it. Link:"/utf8>>,
                        <<<<"https://github.com/exercism/problem-specifications/blob/main/exercises/"/utf8,
                                Slug/binary>>/binary,
                            "/canonical-data.json"/utf8>>],
                    _pipe@1 = gleam@list:append(_pipe, Comments@1),
                    print_comments(_pipe@1);

                _@1 ->
                    print_comments(Comments@1)
            end,
            Test_name = flatten_description(
                <<Prefix/binary, Description@1/binary>>
            ),
            Input@1 = begin
                _pipe@2 = Input,
                _pipe@3 = gleam@map:to_list(_pipe@2),
                _pipe@4 = gleam@list:map(
                    _pipe@3,
                    fun(Item) ->
                        <<<<(clean_variable(erlang:element(1, Item)))/binary,
                                ": "/utf8>>/binary,
                            (json_data_to_gleam_value(erlang:element(2, Item)))/binary>>
                    end
                ),
                gleam@string:join(_pipe@4, <<", "/utf8>>)
            end,
            Expected@1 = get_expected_value(Function, Functions, Expected),
            _pipe@5 = <<"
      <comments>
      pub fn <test_name>_test(){
        <exercise>.<function>(<input>)
        |> should.equal(<expected>)
      }
      "/utf8>>,
            _pipe@6 = gleam@string:replace(
                _pipe@5,
                <<"<comments>"/utf8>>,
                Comments@2
            ),
            _pipe@7 = gleam@string:replace(
                _pipe@6,
                <<"<test_name>"/utf8>>,
                Test_name
            ),
            _pipe@8 = gleam@string:replace(
                _pipe@7,
                <<"<exercise>"/utf8>>,
                Exercise
            ),
            _pipe@9 = gleam@string:replace(
                _pipe@8,
                <<"<function>"/utf8>>,
                clean_variable(Function)
            ),
            _pipe@10 = gleam@string:replace(
                _pipe@9,
                <<"<input>"/utf8>>,
                Input@1
            ),
            gleam@string:replace(_pipe@10, <<"<expected>"/utf8>>, Expected@1)
    end.

-spec flatten_description(binary()) -> binary().
flatten_description(Description) ->
    _pipe = Description,
    _pipe@1 = gleam@string:lowercase(_pipe),
    _pipe@2 = gleam@string:to_graphemes(_pipe@1),
    _pipe@3 = gleam@list:map(
        _pipe@2,
        fun(Char) ->
            Allowed_chars = <<"0123456789abcdefghijklmnopqrstuvwxyz_"/utf8>>,
            case gleam@string:contains(Allowed_chars, Char) of
                true ->
                    Char;

                false ->
                    <<"_"/utf8>>
            end
        end
    ),
    gleam@string:concat(_pipe@3).

-spec get_expected_value(
    binary(),
    gleam@map:map_(binary(), function_()),
    json_data()
) -> binary().
get_expected_value(Function, Functions, Expected) ->
    case {gleam@map:get(Functions, Function), Expected} of
        {{ok, {function, _@1, _@2, true, _@3}}, {json_object, Map}} ->
            case gleam@map:get(Map, <<"error"/utf8>>) of
                {ok, Value} ->
                    <<<<"Error("/utf8,
                            (json_data_to_gleam_value(Value))/binary>>/binary,
                        ")"/utf8>>;

                {error, nil} ->
                    <<<<"Ok("/utf8,
                            (json_data_to_gleam_value(Expected))/binary>>/binary,
                        ")"/utf8>>
            end;

        _@4 ->
            json_data_to_gleam_value(Expected)
    end.

-spec canonical_data_decoder() -> fun((gleam@dynamic:dynamic()) -> {ok,
        canonical_data()} |
    {error, list(gleam@dynamic:decode_error())}).
canonical_data_decoder() ->
    gleam@dynamic:decode2(
        fun(Field@0, Field@1) -> {canonical_data, Field@0, Field@1} end,
        comments_decoder(),
        gleam@dynamic:field(
            <<"cases"/utf8>>,
            gleam@dynamic:list(case_decoder())
        )
    ).

-spec comments_decoder() -> fun((gleam@dynamic:dynamic()) -> {ok,
        list(binary())} |
    {error, list(gleam@dynamic:decode_error())}).
comments_decoder() ->
    gleam@dynamic:any(
        [gleam@dynamic:field(
                <<"comments"/utf8>>,
                gleam@dynamic:list(fun gleam@dynamic:string/1)
            ),
            gleam@function:constant({ok, []})]
    ).

-spec case_decoder() -> fun((gleam@dynamic:dynamic()) -> {ok, test_case()} |
    {error, list(gleam@dynamic:decode_error())}).
case_decoder() ->
    gleam@dynamic:any(
        [gleam@dynamic:decode1(
                fun(Field@0) -> {single_test, Field@0} end,
                test_decoder()
            ),
            gleam@dynamic:decode3(
                fun(Field@0, Field@1, Field@2) -> {test_group, Field@0, Field@1, Field@2} end,
                comments_decoder(),
                gleam@dynamic:field(
                    <<"description"/utf8>>,
                    fun gleam@dynamic:string/1
                ),
                gleam@dynamic:field(
                    <<"cases"/utf8>>,
                    gleam@dynamic:list(lazy(fun() -> case_decoder() end))
                )
            )]
    ).

-spec test_decoder() -> fun((gleam@dynamic:dynamic()) -> {ok, test_data()} |
    {error, list(gleam@dynamic:decode_error())}).
test_decoder() ->
    gleam@dynamic:decode6(
        fun(Field@0, Field@1, Field@2, Field@3, Field@4, Field@5) -> {test_data, Field@0, Field@1, Field@2, Field@3, Field@4, Field@5} end,
        comments_decoder(),
        optional(
            gleam@dynamic:field(
                <<"reimplements"/utf8>>,
                fun gleam@dynamic:string/1
            )
        ),
        gleam@dynamic:field(<<"description"/utf8>>, fun gleam@dynamic:string/1),
        gleam@dynamic:field(<<"property"/utf8>>, fun gleam@dynamic:string/1),
        gleam@dynamic:field(
            <<"input"/utf8>>,
            gleam@dynamic:map(fun gleam@dynamic:string/1, json_data_decoder())
        ),
        gleam@dynamic:field(<<"expected"/utf8>>, json_data_decoder())
    ).

-spec json_data_decoder() -> fun((gleam@dynamic:dynamic()) -> {ok, json_data()} |
    {error, list(gleam@dynamic:decode_error())}).
json_data_decoder() ->
    gleam@dynamic:any(
        [gleam@dynamic:decode1(
                fun(Field@0) -> {json_bool, Field@0} end,
                fun gleam@dynamic:bool/1
            ),
            gleam@dynamic:decode1(
                fun(Field@0) -> {json_int, Field@0} end,
                fun gleam@dynamic:int/1
            ),
            gleam@dynamic:decode1(
                fun(Field@0) -> {json_float, Field@0} end,
                fun gleam@dynamic:float/1
            ),
            gleam@dynamic:decode1(
                fun(Field@0) -> {json_string, Field@0} end,
                fun gleam@dynamic:string/1
            ),
            gleam@dynamic:decode1(
                fun(Field@0) -> {json_list, Field@0} end,
                gleam@dynamic:list(lazy(fun() -> json_data_decoder() end))
            ),
            gleam@dynamic:decode1(
                fun(Field@0) -> {json_object, Field@0} end,
                gleam@dynamic:map(
                    fun gleam@dynamic:string/1,
                    lazy(fun() -> json_data_decoder() end)
                )
            ),
            gleam@function:constant({ok, json_null})]
    ).

-spec optional(
    fun((gleam@dynamic:dynamic()) -> {ok, SV} |
        {error, list(gleam@dynamic:decode_error())})
) -> fun((gleam@dynamic:dynamic()) -> {ok, {ok, SV} | {error, nil}} |
    {error, list(gleam@dynamic:decode_error())}).
optional(Decoder) ->
    gleam@dynamic:any(
        [gleam@dynamic:decode1(fun(Field@0) -> {ok, Field@0} end, Decoder),
            gleam@function:constant({ok, {error, nil}})]
    ).

-spec lazy(
    fun(() -> fun((gleam@dynamic:dynamic()) -> {ok, TA} |
        {error, list(gleam@dynamic:decode_error())}))
) -> fun((gleam@dynamic:dynamic()) -> {ok, TA} |
    {error, list(gleam@dynamic:decode_error())}).
lazy(Wrapped_decoder) ->
    fun(Data) ->
        Decoder = Wrapped_decoder(),
        Decoder(Data)
    end.
