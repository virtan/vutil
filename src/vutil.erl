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
         recursive_make_dir/1
    ]).

-compile({parse_transform, ct_expand}).

calculate_delay_mcs({Mega, Sec, Micro} = _Past, {Mega1, Sec1, Micro1} = _Future) ->
    ((Mega1 - Mega) * 1000000000000) + ((Sec1 - Sec) * 1000000) + Micro1 - Micro.

pmap(F, L) ->
    Parent = self(),
    [receive {Pid, Result} -> Result end || Pid <- [spawn_link(fun() -> Parent ! {self(), F(X)} end) || X <- L]].

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

significant_round(0, _) -> 0;
significant_round(Number, Digits) ->
    D = ceiling(math:log10(abs(Number))),
    P = Digits - round(D),
    M = math:pow(10, P),
    S = round(Number * M),
    S/M.

any_to_binary(X) when is_binary(X) ->
    X;
any_to_binary(X) when is_list(X) ->
    try
        list_to_binary(X)
    catch
        _:_ -> list_to_binary(io_lib:format("~p", [X]))
    end;
any_to_binary(X) when is_integer(X) ->
    list_to_binary(integer_to_list(X));
any_to_binary(X) when is_float(X) ->
    list_to_binary(float_to_list(X));
any_to_binary(X) when is_atom(X) ->
    list_to_binary(atom_to_list(X));
any_to_binary(X) ->
    list_to_binary(io_lib:format("~p", [X])).

any_to_list(X) when is_binary(X) ->
    binary_to_list(X);
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

recursive_make_dir([]) -> ok;
recursive_make_dir(".") -> ok;
recursive_make_dir(Dirname) ->
    case file:make_dir(Dirname) of
        ok -> ok;
        _ ->
            recursive_make_dir(filename:dirname(Dirname)),
            file:make_dir(Dirname),
            ok
    end.


%memo_body(Arg, OriginalF, #{Arg := Value} = Cache) ->
%    {Value, OriginalF, fun(Arg) -> memo_body(Arg, OriginalF, Cache) end};
%memo_body(Arg, OriginalF, Cache) ->
%    NewValue = OriginalF(Arg),
%    {NewValue, OriginalF, maps:put(Arg, NewValue, Cache)}.
%
%memo(F) -> fun(Arg) -> memo_body(Arg, F, #{}) end.
%
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
