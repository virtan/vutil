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
    set_interval/2
]).

-behaviour(gen_server).

%%% API %%%

-type aget_name() :: atom().
-type aget_opt() :: {lifetime, pos_integer()}.
-type aget_opts() :: [ aget_opt() ].

-spec start( Name :: aget_name() ) -> {ok, pid()}.
start(Name) ->
    start(Name, []).

-spec start( Name :: aget_name(), Options :: aget_opts() ) -> {ok, pid()}.
start(Name, Options) ->
    gen_server:start({local, Name}, ?MODULE, Options, []).

-spec start_link( Name :: aget_name() ) -> {ok, pid()}.
start_link(Name) ->
    start_link(Name, []).

-spec start_link( Name :: aget_name(), Options :: aget_opts() ) -> {ok, pid()}.
start_link(Name, Options) ->
    gen_server:start_link({local, Name}, ?MODULE, Options, []).

-spec stop(Name :: aget_name()) -> stopped.
stop(Name) ->
    gen_server:call(Name, stop).

-spec set_interval( Name :: aget_name(), NewInterval :: pos_integer() ) -> ok.
set_interval( Name, NewInterval )
    when is_atom( Name )
    andalso is_integer( NewInterval )
    andalso NewInterval > 0
->
    gen_server:call(Name, {set_interval, NewInterval}).

%%% gen_server %%%
-record(state, {cur_map, prev_map, timer}).

init(Options) ->
    {OurOptions, EtsOptions} = proplists:split(Options, [lifetime]),
    {ok, Timer} = timer:send_interval(proplists:get_value(lifetime, lists:flatten(OurOptions), 3600000), lifecycle),
    EtsOptions1 = [private, {write_concurrency, false}, {read_concurrency, false} | EtsOptions],
    CurMap = ets:new(unnamed, EtsOptions1),
    PrevMap = ets:new(unnamed, EtsOptions1),
    {ok, #state{cur_map = CurMap, prev_map = PrevMap, timer = Timer}}.
handle_call({set_interval, NewInterval}, _From, State = #state{ timer = OldTimer }) when is_integer( NewInterval ) andalso NewInterval > 0 ->
    error_logger:info_report([?MODULE, handle_call, {set_interval, NewInterval}]),
    timer:cancel( OldTimer ),
    {ok, NewTimer} = timer:send_interval( NewInterval, lifecycle ),
    {reply, ok, State #state{ timer = NewTimer }};

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
    Res = ets:insert(CurMap, ObjectOrObjects),
    {reply, Res, State};
handle_call({insert_new, ObjectOrObjects}, _From, State = #state{cur_map = CurMap}) ->
    Res = ets:insert_new(CurMap, ObjectOrObjects),
    {reply, Res, State};
handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};
handle_call(UnexpectedCall, _From, State) ->
    error_logger:warning_report([?MODULE, handle_call, {unexpected_call, UnexpectedCall}]),
    {reply, badarg, State}.

handle_cast(UnexpectedCast, State) ->
    error_logger:warning_report([?MODULE, handle_cast, {unexpected_cast, UnexpectedCast}]),
    {noreply, State}.

handle_info(lifecycle, State = #state{cur_map = CurMap, prev_map = PrevMap}) ->
    ets:delete_all_objects(PrevMap),
    {noreply, State#state{cur_map = PrevMap, prev_map = CurMap}};

handle_info(UnexpectedInfo, State) ->
    error_logger:warning_report([?MODULE, handle_info, {unexpected_info, UnexpectedInfo}]),
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

insert_new(Name, ObjectOrObjects) ->
    gen_server:call(Name, {insert_new, ObjectOrObjects}).

get(Name, Key, CreateF) ->
    case lookup(Name, Key) of
        [] ->
            Value = CreateF(),
            insert(Name, {Key, Value}),
            Value;
        [{Key, Value} | _] ->
            Value
    end.
