-module(vutil).
-export([
         calculate_delay_mcs/2,
         pmap/2,
         floor/1,
         ceiling/1,
         significant_round/2,
         any_to_binary/1,
         any_to_list/1,
         join/2,
         join/4,
         binstrip/2,
         binstrip_light/2,
         unify_proplists_keys/2,
         unify_proplists_values/2,
         number_format/2,
         has_exported_function/2,
         has_exported_function/3,
         binary_to_beam/1,
         load_beam/1,
         unload_module/1,
         run_wait/2,
         run_wait/3,
         top/0
    ]).

-include_lib("eunit/include/eunit.hrl").

-compile({parse_transform, ct_expand}).

calculate_delay_mcs({Mega, Sec, Micro} = _Past, {Mega1, Sec1, Micro1} = _Future) ->
    ((Mega1 - Mega) * 1000000000000) + ((Sec1 - Sec) * 1000000) + Micro1 - Micro.

pmap(F, L) ->
    Parent = self(),
    [receive {'vutil:pmap', Pid, Result} -> Result end || Pid <- [spawn_link(fun() -> Parent ! {'vutil:pmap', self(), catch F(X)} end) || X <- L]].

floor(X) ->
    T = erlang:trunc(X),
    case (X - T) of
        Neg when Neg < 0 -> T - 1;
        Pos when Pos > 0 -> T;
        _ -> T
    end.

ceiling(X) ->
    T = erlang:trunc(X),
    case (X - T) of
        Neg when Neg < 0 -> T;
        Pos when Pos > 0 -> T + 1;
        _ -> T
    end.

significant_round(Number, _) when Number == 0 -> 0.0;
significant_round(Number, Digits) ->
    D = ceiling(math:log10(abs(Number))),
    P = Digits - round(D),
    M = math:pow(10, P),
    S = round(Number * M),
    S/M.

number_format(Number, Digits) when is_list(Number) ->
    case string:chr(Number, $.) of
        0 ->
            lists:duplicate(max(0, Digits - length(Number)), $0) ++ Number;
        _ ->
            Number ++ lists:duplicate(max(0, Digits + 1 - length(Number)), $0)
    end.

any_to_binary(X) when is_binary(X) ->
    X;
any_to_binary(X) when is_list(X) ->
    try
        unicode:characters_to_binary(X)
    catch
        _:_ -> unicode:characters_to_binary(io_lib:format("~p", [X]))
    end;
any_to_binary(X) when is_integer(X) ->
    unicode:characters_to_binary(integer_to_list(X));
any_to_binary(X) when is_float(X) ->
    unicode:characters_to_binary(float_to_list(X));
any_to_binary(X) when is_atom(X) ->
    unicode:characters_to_binary(atom_to_list(X));
any_to_binary(X) ->
    unicode:characters_to_binary(io_lib:format("~p", [X])).

any_to_list(X) when is_binary(X) ->
    unicode:characters_to_list(X);
any_to_list(X) when is_list(X) ->
    X;
any_to_list(X) when is_integer(X) ->
    integer_to_list(X);
any_to_list(X) when is_float(X) ->
    float_to_list(X);
any_to_list(X) when is_atom(X) ->
    atom_to_list(X);
any_to_list(X) ->
    lists:flatten(io_lib:format("~p", [X])).

join(ListOfSomething, Separator) when is_list(ListOfSomething) ->
    lists:reverse(
      lists:foldl(fun(El, []) -> [El];
                     (El, List) -> [El, Separator | List]
                  end, [], ListOfSomething)
     ).

join(ListOfSomething, Separator, Prefix, Suffix) when is_list(ListOfSomething) ->
    lists:reverse(
      lists:foldl(fun(El, []) -> [Suffix, El, Prefix];
                     (El, List) -> [Suffix, El, Prefix, Separator | List]
                  end, [], ListOfSomething)
     ).

% strips '\n', '\t', '\r', ' '
% from Direction (left | right | both) sides
binstrip(Binary, Direction) when is_binary(Binary) andalso
                                 (Direction == left orelse Direction == right orelse Direction == both) ->
    Re = case Direction of
             left -> ct_expand:term(element(2, re:compile("^\\s+")));
             right -> ct_expand:term(element(2, re:compile("\\s+$")));
             both -> ct_expand:term(element(2, re:compile("^\\s+|\\s+$")))
         end,
    case re:run(Binary, Re, [global]) of
        nomatch -> Binary;
        {match, [[{0, L1}], [{O2, _}]]} -> binary:part(Binary, L1, O2 - L1);
        {match, [[{0, L1}]]} -> binary:part(Binary, L1, size(Binary) - L1);
        {match, [[{O2, _}]]} -> binary:part(Binary, 0, O2)
    end.

% works faster than binstrip(Binary, left) when size(Binary) <= 50
binstrip_light(<<$ , Rest/binary>>, left) -> binstrip_light(Rest, left);
binstrip_light(<<$\t, Rest/binary>>, left) -> binstrip_light(Rest, left);
binstrip_light(<<$\n, Rest/binary>>, left) -> binstrip_light(Rest, left);
binstrip_light(<<$\r, Rest/binary>>, left) -> binstrip_light(Rest, left);
binstrip_light(Binary, left) when is_binary(Binary) -> Binary.

unify_proplists_keys(PList, binary) ->
    lists:map(fun({Key, Value}) -> {any_to_binary(Key), Value} end, PList); 
unify_proplists_keys(PList, list) ->
    lists:map(fun({Key, Value}) -> {any_to_list(Key), Value} end, PList).

unify_proplists_values(PList, binary) ->
    lists:map(fun({Key, Value}) -> {Key, any_to_binary(Value)} end, PList); 
unify_proplists_values(PList, list) ->
    lists:map(fun({Key, Value}) -> {Key, any_to_list(Value)} end, PList).

has_exported_function(Module, Func) when is_atom(Module),
                                         (is_list(Func) orelse is_binary(Func)) ->
    has_exported_function(Module, Func, -1).

has_exported_function(Module, Func, Arity) when is_atom(Module),
                                                is_list(Func),
                                                is_integer(Arity) ->
    has_exported_function(Module, list_to_binary(Func));
has_exported_function(Module, Func, Arity) when is_atom(Module),
                                                is_binary(Func),
                                                is_integer(Arity) ->
    [exist || {F, A} <- Module:module_info(exports),
              list_to_binary(atom_to_list(F)) == Func,
              (A == Arity orelse Arity == -1)] =/= [].


%memo_body(Arg, OriginalF, #{Arg := Value} = Cache) ->
%    {Value, OriginalF, fun(Arg) -> memo_body(Arg, OriginalF, Cache) end};
%memo_body(Arg, OriginalF, Cache) ->
%    NewValue = OriginalF(Arg),
%    {NewValue, OriginalF, maps:put(Arg, NewValue, Cache)}.
%
%memo(F) -> fun(Arg) -> memo_body(Arg, F, #{}) end.
%

binary_to_beam(Binary) ->
    {ok, Ts, _} = erl_scan:string(binary_to_list(Binary)),
    {FsI, _} = lists:foldl(fun({dot,_} = Dot, {Parts, Remaining}) -> {[lists:reverse([Dot | Remaining]) | Parts], []};
                              (Other, {Parts, Remaining}) -> {Parts, [Other | Remaining]} end,
                           {[], []}, Ts),
    compile:forms([begin {ok, PFs} = erl_parse:parse_form(F), PFs end || F <- lists:reverse(FsI)]). % {ok, Module, Binary}

load_beam({ok, Module, Beam}) ->
    {module, Module} = code:load_binary(Module, "nofile", Beam).

unload_module(M) ->
    code:delete(M),
    code:purge(M).

run_wait(Timeout, F) -> run_wait(Timeout, 0, F).
run_wait(Timeout, SleepInBetween, F) ->
    erlang:send_after(Timeout, self(), run_wait_timeout),
    erlang:send_after(0, self(), run_wait_try_again),
    CyclerF = fun(CyclerF) ->
        receive
            run_wait_timeout ->
                timeout;
            run_wait_try_again ->
                case F() of
                    wait ->
                        erlang:send_after(SleepInBetween, self(), run_wait_try_again),
                        CyclerF(CyclerF);
                    Other ->
                        Other
                end
        end
              end,
    CyclerF(CyclerF).

run_wait_1_test() ->
    timeout = run_wait(10, fun() -> timeout end),
    ag = run_wait(10, fun() -> case put(ag, ag) of undefined -> wait; ag -> ag end end).

top() ->
    spawn(fun() -> top_internal() end).

top_internal() ->
    erlang:send_after(1000, self(), print),
    Pids = [{P, catch erlang:process_info(P, message_queue_len),
             catch erlang:process_info(P, reductions), catch erlang:process_info(P, current_stacktrace)}
            || P <- erlang:processes(), catch erlang:process_info(P, status) == {status, running}],
    Pids2 = lists:filtermap(fun({P, {message_queue_len, MQL}, {reductions, R}, {current_stacktrace, S}}) ->
                                    {true, {MQL, R, P, top_extract_stacktrace(P, S)}};
                               (_) -> false
                            end, Pids),
    Sorted = lists:reverse(lists:sort(Pids2)),
    top_print(Sorted),
    receive print -> ok end,
    top_internal().

top_extract_stacktrace(Pid, [{gen_server, F, A, [{file, Fl}, {line, Ln}]} | _]) ->
    RealModule = case catch erlang:process_info(Pid, dictionary) of
        {dictionary, D} when is_list(D) ->
            case proplists:get_value('$initial_call', D) of
                {M1, _F1, _A1} -> M1;
                _ -> gen_server
            end;
        _ -> gen_server
    end,
    {RealModule, gen_server, F, A, Fl, Ln};
top_extract_stacktrace(_Pid, [{M, F, A, [{file, Fl}, {line, Ln}]} | _]) ->
    {M, M, F, A, Fl, Ln}.

top_print(Top) ->
    io:format("~nMsgs\tReds\tPid\tRealM\tM:F/Args\tFile:Line~n", []),
    top_print_1(Top, 10).

top_print_1([], _) -> done;
top_print_1(_, 0) -> done;
top_print_1([{MQL, R, P, {RM, M, F, A, Fl, Ln}} | Other], N) ->
    io:format("~p\t~p\t~p\t~p\t~p:~p/~p\t~p:~p~n", [MQL, R, P, RM, M, F, A, Fl, Ln]),
    top_print_1(Other, N - 1).


%memo(F) ->
%    Cache = ets:new(unnamed, [public, {read_concurrency, true}, {write_concurrency, true}]),
%    fun
%        (destruct) -> ets:delete(Cache);
%        (ArgList) ->
%            case ets:lookup(Cache, ArgList) of
%                [{ArgList, Result}] -> Result;
%                [] ->
%                    Result = apply(F, ArgList),
%                    ets:insert(Cache, {ArgList, Result}),
%                    Result
%            end
%    end.
%
%memo_rr(F, MaxSize) ->
%    Cache = ets:new(unnamed, [public, {read_concurrency, true}, {write_concurrency, true}]),
%    fun
%        (destruct) -> ets:delete(Cache);
%        (ArgList) ->
%            case ets:lookup(Cache, ArgList) of
%                [{ArgList, Result}] -> Result;
%                [] ->
%                    case ets:info(Cache, size) of
%                        N when N >= MaxSize ->
%                            ets:
%memo(F) ->
%    Cache = ets:new(unnamed, [public, {read_concurrency, true}, {write_concurrency, true}]),
%    fun
%        (destruct) -> ets:delete(Cache);
%        (ArgList) ->
%            case ets:lookup(Cache, ArgList) of
%                [{ArgList, Result}] -> Result;
%                [] ->
%                    Result = apply(F, ArgList),
%                    ets:insert(Cache, {ArgList, Result}),
%                    Result
%            end
%    end.
%                    Result = apply(F, ArgList),
%                    ets:insert(Cache, {ArgList, Result}),
%                    Result
%            end
%    end.
