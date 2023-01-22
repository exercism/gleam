-module(gleam@erlang@process).
-compile(no_auto_import).

-export([self/0, start/2, new_subject/0, subject_owner/1, send/2, 'receive'/2, new_selector/0, select/2, select_forever/1, map_selector/2, merge_selector/2, selecting_trapped_exits/2, flush_messages/0, selecting/3, selecting_record2/3, selecting_record3/3, selecting_record4/3, selecting_anything/2, sleep/1, sleep_forever/0, is_alive/1, monitor_process/1, selecting_process_down/3, demonitor_process/1, try_call/3, call/3, link/1, unlink/1, send_after/3, cancel_timer/1, kill/1, send_exit/1, send_abnormal_exit/2, trap_exits/1]).
-export_type([pid_/0, subject/1, do_not_leak/0, selector/1, exit_message/0, exit_reason/0, anything_selector_tag/0, process_monitor_flag/0, process_monitor/0, process_down/0, call_error/1, timer/0, cancelled/0, kill_flag/0]).

-type pid_() :: any().

-opaque subject(ELJ) :: {subject, pid_(), gleam@erlang:reference_()} |
    {gleam_phantom, ELJ}.

-type do_not_leak() :: any().

-type selector(Payload) :: any() | {gleam_phantom, Payload}.

-type exit_message() :: {exit_message, pid_(), exit_reason()}.

-type exit_reason() :: normal | killed | {abnormal, binary()}.

-type anything_selector_tag() :: anything.

-type process_monitor_flag() :: process.

-opaque process_monitor() :: {process_monitor, gleam@erlang:reference_()}.

-type process_down() :: {process_down, pid_(), gleam@dynamic:dynamic()}.

-type call_error(ELL) :: {callee_down, gleam@dynamic:dynamic()} |
    call_timeout |
    {gleam_phantom, ELL}.

-type timer() :: any().

-type cancelled() :: timer_not_found | {cancelled, integer()}.

-type kill_flag() :: kill.

-spec self() -> pid_().
self() ->
    erlang:self().

-spec start(fun(() -> any()), boolean()) -> pid_().
start(Implementation, Link) ->
    case Link of
        true ->
            erlang:spawn_link(Implementation);

        false ->
            erlang:spawn(Implementation)
    end.

-spec new_subject() -> subject(any()).
new_subject() ->
    {subject, erlang:self(), erlang:make_ref()}.

-spec subject_owner(subject(any())) -> pid_().
subject_owner(Subject) ->
    erlang:element(2, Subject).

-spec send(subject(ELU), ELU) -> nil.
send(Subject, Message) ->
    erlang:send(
        erlang:element(2, Subject),
        {erlang:element(3, Subject), Message}
    ),
    nil.

-spec 'receive'(subject(ELW), integer()) -> {ok, ELW} | {error, nil}.
'receive'(Subject, Milliseconds) ->
    _pipe = gleam_erlang_ffi:new_selector(),
    _pipe@1 = selecting(_pipe, Subject, fun(X) -> X end),
    gleam_erlang_ffi:select(_pipe@1, Milliseconds).

-spec new_selector() -> selector(any()).
new_selector() ->
    gleam_erlang_ffi:new_selector().

-spec select(selector(EMC), integer()) -> {ok, EMC} | {error, nil}.
select(Field@0, Field@1) ->
    gleam_erlang_ffi:select(Field@0, Field@1).

-spec select_forever(selector(EMG)) -> EMG.
select_forever(Field@0) ->
    gleam_erlang_ffi:select(Field@0).

-spec map_selector(selector(EMK), fun((EMK) -> EMI)) -> selector(EMI).
map_selector(Field@0, Field@1) ->
    gleam_erlang_ffi:map_selector(Field@0, Field@1).

-spec merge_selector(selector(EMM), selector(EMM)) -> selector(EMM).
merge_selector(Field@0, Field@1) ->
    gleam_erlang_ffi:merge_selector(Field@0, Field@1).

-spec selecting_trapped_exits(selector(EMQ), fun((exit_message()) -> EMQ)) -> selector(EMQ).
selecting_trapped_exits(Selector, Handler) ->
    Tag = erlang:binary_to_atom(<<"EXIT"/utf8>>),
    Handler@1 = fun(Message) ->
        Reason = erlang:element(3, Message),
        Normal = gleam@dynamic:from(normal),
        Killed = gleam@dynamic:from(killed),
        Reason@2 = case gleam@dynamic:string(Reason) of
            _@1 when Reason =:= Normal ->
                normal;

            _@2 when Reason =:= Killed ->
                killed;

            {ok, Reason@1} ->
                {abnormal, Reason@1};

            {error, _@3} ->
                {abnormal, gleam@string:inspect(Reason)}
        end,
        Handler({exit_message, erlang:element(2, Message), Reason@2})
    end,
    gleam_erlang_ffi:insert_selector_handler(Selector, {Tag, 3}, Handler@1).

-spec flush_messages() -> nil.
flush_messages() ->
    gleam_erlang_ffi:flush_messages().

-spec selecting(selector(EMT), subject(EMV), fun((EMV) -> EMT)) -> selector(EMT).
selecting(Selector, Subject, Transform) ->
    Handler = fun(Message) -> Transform(erlang:element(2, Message)) end,
    gleam_erlang_ffi:insert_selector_handler(
        Selector,
        {erlang:element(3, Subject), 2},
        Handler
    ).

-spec selecting_record2(
    selector(EMY),
    any(),
    fun((gleam@dynamic:dynamic()) -> EMY)
) -> selector(EMY).
selecting_record2(Selector, Tag, Transform) ->
    Handler = fun(Message) -> Transform(erlang:element(2, Message)) end,
    gleam_erlang_ffi:insert_selector_handler(Selector, {Tag, 2}, Handler).

-spec selecting_record3(
    selector(ENC),
    any(),
    fun((gleam@dynamic:dynamic(), gleam@dynamic:dynamic()) -> ENC)
) -> selector(ENC).
selecting_record3(Selector, Tag, Transform) ->
    Handler = fun(Message) ->
        Transform(erlang:element(2, Message), erlang:element(3, Message))
    end,
    gleam_erlang_ffi:insert_selector_handler(Selector, {Tag, 3}, Handler).

-spec selecting_record4(
    selector(ENG),
    any(),
    fun((gleam@dynamic:dynamic(), gleam@dynamic:dynamic(), gleam@dynamic:dynamic()) -> ENG)
) -> selector(ENG).
selecting_record4(Selector, Tag, Transform) ->
    Handler = fun(Message) ->
        Transform(
            erlang:element(2, Message),
            erlang:element(3, Message),
            erlang:element(4, Message)
        )
    end,
    gleam_erlang_ffi:insert_selector_handler(Selector, {Tag, 4}, Handler).

-spec selecting_anything(selector(ENK), fun((gleam@dynamic:dynamic()) -> ENK)) -> selector(ENK).
selecting_anything(Selector, Handler) ->
    gleam_erlang_ffi:insert_selector_handler(Selector, anything, Handler).

-spec sleep(integer()) -> nil.
sleep(Field@0) ->
    gleam_erlang_ffi:sleep(Field@0).

-spec sleep_forever() -> nil.
sleep_forever() ->
    gleam_erlang_ffi:sleep_forever().

-spec is_alive(pid_()) -> boolean().
is_alive(Field@0) ->
    erlang:is_process_alive(Field@0).

-spec monitor_process(pid_()) -> process_monitor().
monitor_process(Pid) ->
    _pipe = process,
    _pipe@1 = erlang:monitor(_pipe, Pid),
    {process_monitor, _pipe@1}.

-spec selecting_process_down(
    selector(ENS),
    process_monitor(),
    fun((process_down()) -> ENS)
) -> selector(ENS).
selecting_process_down(Selector, Monitor, Mapping) ->
    gleam_erlang_ffi:insert_selector_handler(
        Selector,
        erlang:element(2, Monitor),
        Mapping
    ).

-spec demonitor_process(process_monitor()) -> nil.
demonitor_process(Field@0) ->
    gleam_erlang_ffi:demonitor(Field@0).

-spec try_call(subject(ENV), fun((subject(ENX)) -> ENV), integer()) -> {ok, ENX} |
    {error, call_error(ENX)}.
try_call(Subject, Make_request, Timeout) ->
    Reply_subject = new_subject(),
    Monitor = monitor_process(subject_owner(Subject)),
    send(Subject, Make_request(Reply_subject)),
    Result = begin
        _pipe = gleam_erlang_ffi:new_selector(),
        _pipe@1 = selecting(
            _pipe,
            Reply_subject,
            fun(Field@0) -> {ok, Field@0} end
        ),
        _pipe@2 = selecting_process_down(
            _pipe@1,
            Monitor,
            fun(Down) -> {error, {callee_down, erlang:element(3, Down)}} end
        ),
        gleam_erlang_ffi:select(_pipe@2, Timeout)
    end,
    gleam_erlang_ffi:demonitor(Monitor),
    case Result of
        {error, nil} ->
            {error, call_timeout};

        {ok, Res} ->
            Res
    end.

-spec call(subject(EOC), fun((subject(EOE)) -> EOC), integer()) -> EOE.
call(Subject, Make_request, Timeout) ->
    {ok, Resp@1} = case try_call(Subject, Make_request, Timeout) of
        {ok, Resp} -> {ok, Resp};
        _try ->
            erlang:error(#{gleam_error => assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _try,
                        module => <<"gleam/erlang/process"/utf8>>,
                        function => <<"call"/utf8>>,
                        line => 490})
    end,
    Resp@1.

-spec link(pid_()) -> boolean().
link(Field@0) ->
    gleam_erlang_ffi:link(Field@0).

-spec unlink(pid_()) -> nil.
unlink(Pid) ->
    erlang:unlink(Pid),
    nil.

-spec send_after(subject(EOH), integer(), EOH) -> timer().
send_after(Subject, Delay, Message) ->
    erlang:send_after(
        Delay,
        erlang:element(2, Subject),
        {erlang:element(3, Subject), Message}
    ).

-spec cancel_timer(timer()) -> cancelled().
cancel_timer(Timer) ->
    case gleam@dynamic:int(erlang:cancel_timer(Timer)) of
        {ok, I} ->
            {cancelled, I};

        {error, _@1} ->
            timer_not_found
    end.

-spec kill(pid_()) -> nil.
kill(Pid) ->
    erlang:exit(Pid, kill),
    nil.

-spec send_exit(pid_()) -> nil.
send_exit(Pid) ->
    erlang:exit(Pid, normal),
    nil.

-spec send_abnormal_exit(pid_(), binary()) -> nil.
send_abnormal_exit(Pid, Reason) ->
    erlang:exit(Pid, {abnormal, Reason}),
    nil.

-spec trap_exits(boolean()) -> nil.
trap_exits(Field@0) ->
    gleam_erlang_ffi:trap_exits(Field@0).
