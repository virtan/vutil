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
    insert_new/2,
    get/3,
    set_interval/2,
    update/3,
    update/4,
    update_recent/3,
    update_recent/4,
    get_both/2
]).

-behaviour(gen_server).

%%% API %%%

-type aget_name() :: atom().
-type aget_opt() :: {lifetime, pos_integer()}.
-type aget_opts() :: [ aget_opt() ].

-spec start(Name :: aget_name()) -> {ok, pid()}.
start(Name) ->
    start(Name, []).

-spec start(Name :: aget_name(), Options :: aget_opts()) -> {ok, pid()}.
start(Name, Options) ->
    gen_server:start({local, Name}, ?MODULE, Options, []).

-spec start_link(Name :: aget_name()) -> {ok, pid()}.
start_link(Name) ->
    start_link(Name, []).

-spec start_link(Name :: aget_name(), Options :: aget_opts()) -> {ok, pid()}.
start_link(Name, Options) ->
    gen_server:start_link({local, Name}, ?MODULE, Options, []).

-spec stop(Name :: aget_name()) -> stopped.
stop(Name) ->
    gen_server:call(Name, stop).

-spec set_interval(Name :: aget_name(), NewInterval :: pos_integer()) -> ok.
set_interval(Name, NewInterval)
    when is_atom(Name)
    andalso is_integer(NewInterval)
    andalso NewInterval > 0
->
    gen_server:call(Name, {set_interval, NewInterval}).

%%% gen_server %%%
-record(state, {cur_map, prev_map, timer, lifetime}).

init(Options) ->
    {OurOptions, EtsOptions} = proplists:split(Options, [lifetime]),
    Lifetime = proplists:get_value(lifetime, lists:flatten(OurOptions), 3600000),
    Timer = erlang:send_after(Lifetime, self(), lifecycle),
    EtsOptions1 = [private, {write_concurrency, false}, {read_concurrency, false}
                   | EtsOptions],
    CurMap = ets:new(unnamed, EtsOptions1),
    PrevMap = ets:new(unnamed, EtsOptions1),
    {ok, #state{cur_map = CurMap, prev_map = PrevMap, timer = Timer, lifetime = Lifetime}}.

handle_call({set_interval, NewInterval}, _From, State = #state{timer = OldTimer}) when is_integer(NewInterval) andalso NewInterval > 0 ->
    error_logger:info_report([?MODULE, handle_call, {set_interval, NewInterval}]),
    erlang:cancel_timer(OldTimer),
    NewTimer = erlang:send_after(NewInterval, self(), lifecycle),
    {reply, ok, State #state{timer = NewTimer, lifetime = NewInterval}};
handle_call({lookup, Key}, _From, State = #state{cur_map = CurMap, prev_map = PrevMap}) ->
    {reply, get_move(CurMap, PrevMap, Key), State};
handle_call({get_both, Key}, _From, State = #state{cur_map = CurMap, prev_map = PrevMap}) ->
    {reply, {get_recent(CurMap, Key), get_old(PrevMap, Key)}, State};
handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};
handle_call(UnexpectedCall, _From, State) ->
    error_logger:warning_report([?MODULE, handle_call, {unexpected_call, UnexpectedCall}]),
    {reply, badarg, State}.

handle_cast({delete, Key}, State = #state{cur_map = CurMap, prev_map = PrevMap}) ->
    ets:delete(CurMap, Key),
    ets:delete(PrevMap, Key),
    {noreply, State};
handle_cast({insert, ObjectOrObjects}, State = #state{cur_map = CurMap}) ->
    ets:insert(CurMap, ObjectOrObjects),
    {noreply, State};
handle_cast({insert_new, ObjectOrObjects}, State = #state{cur_map = CurMap}) ->
    ets:insert_new(CurMap, ObjectOrObjects),
    {noreply, State};
handle_cast({UpdateOrUpdateRecent, GetF, Key, UpdateF, Default},
            State = #state{cur_map = CurMap, prev_map = PrevMap})
  when UpdateOrUpdateRecent == update orelse UpdateOrUpdateRecent == update_recent ->
    case GetF(CurMap, PrevMap, Key, dont_move) of
        [] -> ets:insert_new(CurMap, {Key, Default});
        [SingleOldValue] -> ets:insert(CurMap, {Key, UpdateF(SingleOldValue)});
        [_|_] = MultipleOldValues -> ets:insert(CurMap, {Key, UpdateF(MultipleOldValues)})
    end,
    {noreply, State};
handle_cast(UnexpectedCast, State) ->
    error_logger:warning_report([?MODULE, handle_cast, {unexpected_cast, UnexpectedCast}]),
    {noreply, State}.

handle_info(lifecycle, State = #state{cur_map = CurMap, prev_map = PrevMap, lifetime = Lifetime}) ->
    ets:delete_all_objects(PrevMap),
    Timer = erlang:send_after(Lifetime, self(), lifecycle),
    {noreply, State#state{cur_map = PrevMap, prev_map = CurMap, timer = Timer}};
handle_info(UnexpectedInfo, State) ->
    error_logger:warning_report([?MODULE, handle_info, {unexpected_info, UnexpectedInfo}]),
    {noreply, State}.

code_change(_, State, _) ->
    {ok, State}.

terminate(_, #state{cur_map = CurMap, prev_map = PrevMap, timer = Timer}) ->
    ets:delete(CurMap),
    ets:delete(PrevMap),
    erlang:cancel_timer(Timer),
    ok.

get_move(CurMap, PrevMap, Key) ->
    get_move(CurMap, PrevMap, Key, move).

get_move(CurMap, PrevMap, Key, DoMove) ->
    case ets:lookup(CurMap, Key) of
        [] ->
            case {ets:lookup(PrevMap, Key), DoMove} of
                {[], _} -> [];
                {ToBeMoved, move} ->
                    ets:insert(CurMap, ToBeMoved),
                    ToBeMoved;
                {WontBeMoved, _} -> WontBeMoved
            end;
        Some -> Some
    end.

get_recent(CurMap, Key) ->
    ets:lookup(CurMap, Key).

get_old(PrevMap, Key) ->
    ets:lookup(PrevMap, Key).


%% External API

lookup(Name, Key) ->
    gen_server:call(Name, {lookup, Key}).

delete(Name, Key) ->
    gen_server:cast(Name, {delete, Key}).

insert(Name, ObjectOrObjects) ->
    gen_server:cast(Name, {insert, ObjectOrObjects}).

insert_new(Name, ObjectOrObjects) ->
    gen_server:cast(Name, {insert_new, ObjectOrObjects}).

get(Name, Key, CreateF) ->
    case lookup(Name, Key) of
        [] ->
            Value = CreateF(),
            insert(Name, {Key, Value}),
            Value;
        [{Key, Value} | _] ->
            Value
    end.

update(Name, Key, UpdateF) ->
    update(Name, Key, UpdateF, undefined).

update(Name, Key, UpdateF, Default) ->
    gen_server:cast(Name, {update, get_move, Key, UpdateF, Default}).

update_recent(Name, Key, UpdateF) ->
    update_recent(Name, Key, UpdateF, undefined).

update_recent(Name, Key, UpdateF, Default) ->
    gen_server:cast(Name, {update_recent, get_recent, Key, UpdateF, Default}).

get_both(Name, Key) ->
    gen_server:call(Name, {get_both, Key}).

