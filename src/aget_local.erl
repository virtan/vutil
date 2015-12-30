-module(aget_local).
-export([
         init/1,
         terminate/1,
         on_message/2,
         lookup/2,
         delete/2,
         insert/2,
         insert_new/2,
         get/3,
         current_ets/1
]).


%% ETS-like storage with limited lifetime elements
%% There is the one lifetime for the whole storage
%% All expired elements are deleted at once which
%% more effecient than having individual timers.

%% aget_local works in the current process (this is
%% the main difference with aget module)

-record(?MODULE, {
           cur_map,
           prev_map,
           interval,
           read_extends
          }).
-type aget_local_table() :: #?MODULE{}.

-include_lib("eunit/include/eunit.hrl").

%% ETSOptions ++
%% [{lifetime, TTLInMS}] ++
%% [{read_extends, Boolean}]
-spec init([proplists:property()]) -> aget_local_table().
init(Options) ->
    {OurOptions, EtsOptions} = proplists:split(Options, [lifetime, read_extends]),
    CurMap = ets:new(unnamed, EtsOptions),
    PrevMap = ets:new(unnamed, EtsOptions),
    ReadExtends = proplists:get_value(read_extends, lists:flatten(OurOptions), true),
    TimerMS = proplists:get_value(lifetime, lists:flatten(OurOptions), 3600*1000),
    erlang:send_after(TimerMS, self(), {aget_local, lifecycle}),
    #?MODULE{cur_map = CurMap, prev_map = PrevMap, interval = TimerMS, read_extends = ReadExtends}.

-spec terminate(aget_local_table()) -> normal.
terminate(#?MODULE{cur_map = CurMap, prev_map = PrevMap}) ->
    ets:delete(CurMap),
    ets:delete(PrevMap),
    normal.

-spec on_message(aget_local_table(), any()) -> aget_local_table().
on_message(#?MODULE{cur_map = CurMap, prev_map = PrevMap, interval = TimerMS} = AGetTable, {aget_local, lifecycle}) ->
    ets:delete_all_objects(PrevMap),
    erlang:send_after(TimerMS, self(), {aget_local, lifecycle}),
    AGetTable#?MODULE{cur_map = PrevMap, prev_map = CurMap};
on_message(AGetTable, _) ->
    AGetTable.

-spec lookup(aget_local_table(), any()) -> [tuple()].
lookup(#?MODULE{cur_map = CurMap, prev_map = PrevMap, read_extends = ReadExtends}, Key) ->
    case ets:lookup(CurMap, Key) of
        [] ->
            case {ets:lookup(PrevMap, Key), ReadExtends} of
                {[], _} -> [];
                {ToBeMoved, true} ->
                    ets:insert(CurMap, ToBeMoved),
                    ToBeMoved;
                {WontBeMoved, _} ->
                    WontBeMoved
            end;
        Some -> Some
    end.

-spec delete(aget_local_table(), any()) -> true.
delete(#?MODULE{cur_map = CurMap, prev_map = PrevMap}, Key) ->
    ets:delete(CurMap, Key),
    ets:delete(PrevMap, Key),
    true.

-spec insert(aget_local_table(), tuple() | [tuple()]) -> true.
insert(#?MODULE{cur_map = CurMap}, ObjectOrObjects) ->
    ets:insert(CurMap, ObjectOrObjects).

-spec insert_new(aget_local_table(), tuple() | [tuple()]) -> true.
insert_new(#?MODULE{cur_map = CurMap}, ObjectOrObjects) ->
    ets:insert_new(CurMap, ObjectOrObjects).

-spec get(aget_local_table(), any(), any()) -> any().
get(AGetTable, Key, CreateF) when is_function(CreateF) ->
    case lookup(AGetTable, Key) of
        [] ->
            Value = CreateF(),
            insert(AGetTable, {Key, Value}),
            Value;
        [{Key, Value} | _] ->
            Value
    end;
get(AGetTable, Key, Default) ->
    case lookup(AGetTable, Key) of
        [] ->
            insert(AGetTable, {Key, Default}),
            Default;
        [{Key, Value} | _] ->
            Value
    end.

-spec current_ets(#?MODULE{}) -> integer().
current_ets(#?MODULE{cur_map = CurMap}) ->
    CurMap.

%% tests

ets_like_test() ->
    AGT = init([]),
    ?assertEqual([], lookup(AGT, hello)),
    ?assertEqual(true, insert(AGT, {hello, world})),
    ?assertEqual([{hello, world}], lookup(AGT, hello)),
    ?assertEqual(true, delete(AGT, hello)),
    ?assertEqual([], lookup(AGT, hello)),
    ?assertEqual(true, insert_new(AGT, {hello, world})),
    ?assertEqual([{hello, world}], lookup(AGT, hello)),
    ?assertEqual(world, get(AGT, hello, fun() -> mmm end)),
    ?assertEqual(world, get(AGT, hello, mmm)),
    ?assertEqual(true, delete(AGT, hello)),
    ?assertEqual(mmm, get(AGT, hello, fun() -> mmm end)),
    ?assertEqual(true, delete(AGT, hello)),
    ?assertEqual(mmm, get(AGT, hello, mmm)),
    ?assertEqual(normal, terminate(AGT)).

expiration_test() ->
    AGT = init([]),
    ?assertEqual([], lookup(AGT, hello)),
    ?assertEqual(true, insert(AGT, {hello, world})),
    ?assertEqual([{hello, world}], lookup(AGT, hello)),
    AGT2 = on_message(AGT, {aget_local, lifecycle}),
    AGT3 = on_message(AGT2, {aget_local, lifecycle}),
    ?assertEqual([], lookup(AGT3, hello)),
    ?assertEqual(normal, terminate(AGT3)).

read_non_expiration_test() ->
    AGT = init([]),
    ?assertEqual([], lookup(AGT, hello)),
    ?assertEqual(true, insert(AGT, {hello, world})),
    ?assertEqual([{hello, world}], lookup(AGT, hello)),
    AGT2 = on_message(AGT, {aget_local, lifecycle}),
    ?assertEqual([{hello, world}], lookup(AGT2, hello)),
    AGT3 = on_message(AGT2, {aget_local, lifecycle}),
    ?assertEqual([{hello, world}], lookup(AGT3, hello)),
    ?assertEqual(normal, terminate(AGT3)).

read_expiration_test() ->
    AGT = init([{read_extends, false}]),
    ?assertEqual([], lookup(AGT, hello)),
    ?assertEqual(true, insert(AGT, {hello, world})),
    ?assertEqual([{hello, world}], lookup(AGT, hello)),
    AGT2 = on_message(AGT, {aget_local, lifecycle}),
    ?assertEqual([{hello, world}], lookup(AGT2, hello)),
    AGT3 = on_message(AGT2, {aget_local, lifecycle}),
    ?assertEqual([], lookup(AGT3, hello)),
    ?assertEqual(normal, terminate(AGT3)).

timer_test() ->
    AGT = init([{lifetime, 1}]),
    ?assertEqual({aget_local, lifecycle}, receive {aget_local, _} = N -> N after 2 -> timeout end),
    AGT2 = on_message(AGT, {aget_local, lifecycle}),
    ?assertEqual({aget_local, lifecycle}, receive {aget_local, _} = N -> N after 2 -> timeout end),
    ?assertEqual(normal, terminate(AGT2)).


