-module(aget).
-export([
    start/1,
    start/2,
    start_link/1,
    start_link/2,
    stop/1,
    code_change/3,
    handle_call/3,
    init/1,
    terminate/2,
    handle_info/2,
    handle_cast/2,
    lookup/2,
    delete/2,
    insert/2,
    get/3
]).

-behaviour(gen_server).

-record(state, {cur_map, prev_map, timer}).

start(Name) ->
    start(Name, []).

start(Name, Options) ->
    gen_server:start({local, Name}, ?MODULE, Options, []).

start_link(Name) ->
    start_link(Name, []).

start_link(Name, Options) ->
    gen_server:start_link({local, Name}, ?MODULE, Options, []).

stop(Name) ->
    gen_server:call(Name, stop).

init(Options) ->
    {OurOptions, EtsOptions} = proplists:split(Options, [lifetime]),
    {ok, Timer} = timer:send_interval(proplists:get_value(lifetime, lists:flatten(OurOptions), 3600000), lifecycle),
    EtsOptions1 = [private, {write_concurrency, false}, {read_concurrency, false} | EtsOptions],
    CurMap = ets:new(unnamed, EtsOptions1),
    PrevMap = ets:new(unnamed, EtsOptions1),
    {ok, #state{cur_map = CurMap, prev_map = PrevMap, timer = Timer}}.

handle_call({lookup, Key}, _From, State = #state{cur_map = CurMap, prev_map = PrevMap}) ->
    case ets:lookup(CurMap, Key) of
        [] ->
            case ets:lookup(PrevMap, Key) of
                [] -> {reply, [], State};
                ToBeMoved ->
                    ets:insert(CurMap, ToBeMoved),
                    {reply, ToBeMoved, State}
            end;
        Some -> {reply, Some, State}
    end;
handle_call({delete, Key}, _From, State = #state{cur_map = CurMap, prev_map = PrevMap}) ->
    ets:delete(CurMap, Key),
    ets:delete(PrevMap, Key),
    {reply, true, State};
handle_call({insert, ObjectOrObjects}, _From, State = #state{cur_map = CurMap}) ->
    ets:insert(CurMap, ObjectOrObjects),
    {reply, true, State};
handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};
handle_call(_, _From, State) ->
    {reply, undefined, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info(lifecycle, State = #state{cur_map = CurMap, prev_map = PrevMap}) ->
    ets:delete_all_objects(PrevMap),
    {noreply, State#state{cur_map = PrevMap, prev_map = CurMap}};
handle_info(_, State) ->
    {noreply, State}.

code_change(_, State, _) ->
    {ok, State}.

terminate(_, #state{cur_map = CurMap, prev_map = PrevMap, timer = Timer}) ->
    ets:delete(CurMap),
    ets:delete(PrevMap),
    timer:cancel(Timer),
    ok.

lookup(Name, Key) ->
    gen_server:call(Name, {lookup, Key}).

delete(Name, Key) ->
    gen_server:call(Name, {delete, Key}).

insert(Name, ObjectOrObjects) ->
    gen_server:call(Name, {insert, ObjectOrObjects}).

get(Name, Key, CreateF) ->
    case lookup(Name, Key) of
        [] ->
            Value = CreateF(),
            insert(Name, {Key, Value}),
            Value;
        [{Key, Value} | _] ->
            Value
    end.
