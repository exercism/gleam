-module(gleam_erlang_ffi).
-export([
    atom_from_dynamic/1, rescue/1, atom_from_string/1, get_line/1,
    ensure_all_started/1, sleep/1, os_family/0, sleep_forever/0, read_file/1,
    append_file/2, write_file/2, delete_file/1, get_all_env/0, get_env/1,
    set_env/2, unset_env/1, delete_directory/1, recursive_delete/1,
    list_directory/1, demonitor/1, make_directory/1, new_selector/0, link/1,
    insert_selector_handler/3, select/1, select/2, trap_exits/1, map_selector/2,
    merge_selector/2, flush_messages/0
]).

-define(is_posix_error(Error),
    Error =:= eacces orelse Error =:= eagain orelse Error =:= ebadf orelse
    Error =:= ebadmsg orelse Error =:= ebusy orelse Error =:= edeadlk orelse
    Error =:= edeadlock orelse Error =:= edquot orelse Error =:= eexist orelse
    Error =:= efault orelse Error =:= efbig orelse Error =:= eftype orelse
    Error =:= eintr orelse Error =:= einval orelse Error =:= eio orelse
    Error =:= eisdir orelse Error =:= eloop orelse Error =:= emfile orelse
    Error =:= emlink orelse Error =:= emultihop orelse Error =:= enametoolong orelse
    Error =:= enfile orelse Error =:= enobufs orelse Error =:= enodev orelse
    Error =:= enolck orelse Error =:= enolink orelse Error =:= enoent orelse
    Error =:= enomem orelse Error =:= enospc orelse Error =:= enosr orelse
    Error =:= enostr orelse Error =:= enosys orelse Error =:= enotblk orelse
    Error =:= enotdir orelse Error =:= enotsup orelse Error =:= enxio orelse
    Error =:= eopnotsupp orelse Error =:= eoverflow orelse Error =:= eperm orelse
    Error =:= epipe orelse Error =:= erange orelse Error =:= erofs orelse
    Error =:= espipe orelse Error =:= esrch orelse Error =:= estale orelse
    Error =:= etxtbsy orelse Error =:= exdev
).

-spec atom_from_string(binary()) -> {ok, atom()} | {error, atom_not_loaded}.
atom_from_string(S) ->
    try {ok, binary_to_existing_atom(S)}
    catch error:badarg -> {error, atom_not_loaded}
    end.

atom_from_dynamic(Data) when is_atom(Data) ->
    {ok, Data};
atom_from_dynamic(Data) ->
    {error, [{decode_error, <<"Atom">>, gleam@dynamic:classify(Data), []}]}.

-spec get_line(io:prompt()) -> {ok, unicode:unicode_binary()} | {error, eof | no_data}.
get_line(Prompt) ->
    case io:get_line(Prompt) of
        eof -> {error, eof};
        {error, _} -> {error, no_data};
        Data when is_binary(Data) -> {ok, Data};
        Data when is_list(Data) -> {ok, unicode:characters_to_binary(Data)}
    end.

rescue(F) ->
    try {ok, F()}
    catch
        throw:X -> {error, {thrown, X}};
        error:X -> {error, {errored, X}};
        exit:X -> {error, {exited, X}}
    end.

ensure_all_started(Application) ->
    case application:ensure_all_started(Application) of
        {ok, _} = Ok -> Ok;

        {error, {ProblemApp, {"no such file or directory", _}}} ->
            {error, {unknown_application, ProblemApp}}
    end.

sleep(Microseconds) ->
    timer:sleep(Microseconds),
    nil.

sleep_forever() ->
    timer:sleep(infinity),
    nil.

posix_result(Result) ->
    case Result of
        ok -> {ok, nil};
        {ok, Value} -> {ok, Value};
        {error, Reason} when ?is_posix_error(Reason) -> {error, Reason}
    end.

read_file(Filename) ->
    posix_result(file:read_file(Filename)).

write_file(Contents, Filename) ->
    posix_result(file:write_file(Filename, Contents)).

append_file(Contents, Filename) ->
    posix_result(file:write_file(Filename, Contents, [append])).

delete_file(Filename) ->
    posix_result(file:delete(Filename)).

make_directory(Dir) ->
    posix_result(file:make_dir(Dir)).

list_directory(Dir) ->
    case file:list_dir(Dir) of
        {ok, Filenames} ->
            {ok, [list_to_binary(Filename) || Filename <- Filenames]};
        {error, Reason} when ?is_posix_error(Reason) ->
            {error, Reason}
    end.

delete_directory(Dir) ->
    posix_result(file:del_dir(Dir)).

recursive_delete(Dir) ->
    posix_result(file:del_dir_r(Dir)).

get_all_env() ->
    BinVars = lists:map(fun(VarString) ->
        [VarName, VarVal] = string:split(VarString, "="),
        {list_to_binary(VarName), list_to_binary(VarVal)}
    end, os:getenv()),
    maps:from_list(BinVars).

get_env(Name) ->
    case os:getenv(binary_to_list(Name)) of
        false -> {error, nil};
        Value -> {ok, list_to_binary(Value)}
    end.

set_env(Name, Value) ->
    os:putenv(binary_to_list(Name), binary_to_list(Value)),
    nil.

unset_env(Name) ->
    os:unsetenv(binary_to_list(Name)),
    nil.

os_family() ->
    case os:type() of
        {win32, nt} ->
            windows_nt;
        {unix, linux} ->
            linux;
        {unix, darwin} ->
            darwin;
        {unix, freebsd} ->
            free_bsd;
        {_, Other} ->
            {other, atom_to_binary(Other, utf8)}
    end.

new_selector() ->
    {selector, #{}}.

map_selector({selector, Handlers}, Fn) ->
    MappedHandlers = maps:map(fun(_Tag, Handler) ->
        fun(Message) -> Fn(Handler(Message)) end
    end, Handlers),
    {selector, MappedHandlers}.

merge_selector({selector, HandlersA}, {selector, HandlersB}) ->
    {selector, maps:merge(HandlersA, HandlersB)}.

insert_selector_handler({selector, Handlers}, Tag, Fn) ->
    {selector, Handlers#{Tag => Fn}}.

select(Selector) ->
    {ok, Message} = select(Selector, infinity),
    Message.

select({selector, Handlers}, Timeout) ->
    AnythingHandler = maps:get(anything, Handlers, undefined),
    receive
        % Monitored process down messages.
        % This is special cased so we can selectively receive based on the
        % reference as well as the record tag.
        {'DOWN', Ref, process, Pid, Reason} when is_map_key(Ref, Handlers) ->
            Fn = maps:get(Ref, Handlers),
            {ok, Fn({process_down, Pid, Reason})};

        Msg when is_map_key({element(1, Msg), tuple_size(Msg)}, Handlers) ->
            Fn = maps:get({element(1, Msg), tuple_size(Msg)}, Handlers),
            {ok, Fn(Msg)};
        
        Msg when AnythingHandler =/= undefined ->
            {ok, AnythingHandler(Msg)}
    after Timeout ->
        {error, nil}
    end.

demonitor({_, Reference}) ->
    erlang:demonitor(Reference, [flush]).

link(Pid) ->
    try 
        erlang:link(Pid)
    catch
        error:_ -> false
    end.

trap_exits(ShouldTrap) ->
    erlang:process_flag(trap_exit, ShouldTrap),
    nil.

flush_messages() ->
    receive _Message -> flush_messages()
    after 0 -> nil
    end.
