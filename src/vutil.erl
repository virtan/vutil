-module(vutil).
-export([
         calculate_delay_mcs/2,
         pmap/2,
         floor/1,
         ceiling/1,
         significant_round/2,
         any_to_binary/1
    ]).

calculate_delay_mcs({Mega, Sec, Micro} = _Past, {Mega1, Sec1, Micro1} = _Future) ->
    ((Mega1 - Mega) * 1000000000000) + ((Sec1 - Sec) * 1000000) + Micro1 - Micro.

pmap(F, L) ->
    Parent = self(),
    [receive {Pid, Result} -> Result end || Pid <- [spawn(fun() -> Parent ! {self(), F(X)} end) || X <- L]].

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
    list_to_binary(X);
any_to_binary(X) when is_integer(X) ->
    list_to_binary(integer_to_list(X));
any_to_binary(X) when is_float(X) ->
    list_to_binary(float_to_list(X));
any_to_binary(X) when is_atom(X) ->
    list_to_binary(atom_to_list(X));
any_to_binary(X) ->
    list_to_binary(io_lib:format("~p", [X])).
