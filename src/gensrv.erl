-module(gensrv).
-behaviour(gen_server).

-export([
          start/1,
          start_link/1,
          stop/1,

          init/1,
          handle_call/3,
          handle_cast/2,
          handle_info/2,
          code_change/3,
          terminate/2
        ]).

-include("gensrv.hrl").

%% Usage
%% child_spec() ->
%%     {test, {gensrv, start_link, [#gensrv{
%%                                     register = {local, ?MODULE},
%%                                     init = fun(_) -> erlang:send_after(5000, self(), timeout) end,
%%                                     handle_info = fun(timeout, _) ->
%%                                                           io:format("got timeout~n", []),
%%                                                           erlang:send_after(5000, self(), timeout)
%%                                                   end,
%%                                     terminate = fun(Reason, TimerRef) -> erlang:cancel_timer(TimerRef), Reason end
%%                                    }]},
%%      permanent, infinity, worker, dynamic}.
%%  call() ->
%%      gen_server:call(?MODULE, Message).


%% Exported

start(#gensrv{register = undefined} = GenSrv) ->
    gen_server:start(?MODULE, GenSrv, []);
start(#gensrv{register = Register} = GenSrv) ->
    gen_server:start(Register, ?MODULE, GenSrv, []).

start_link(#gensrv{register = undefined} = GenSrv) ->
    gen_server:start_link(?MODULE, GenSrv, []);
start_link(#gensrv{register = Register} = GenSrv) ->
    gen_server:start_link(Register, ?MODULE, GenSrv, []).

stop(Instance) ->
    gen_server:call(Instance, stop).

%% Internal

init(#gensrv{init = undefined} = State) ->
    {ok, State};
init(#gensrv{init = Init, opaque = Opaque} = State) ->
    try Init(Opaque) of
        {error, Reason} -> {stop, Reason};
        ignore -> {ok, State};
        NewOpaque -> {ok, State#gensrv{opaque = NewOpaque}}
    catch
        Type:Error ->
            {stop, {Type, Error, erlang:get_stacktrace()}}
    end.


handle_call(stop, _From, #gensrv{} = State) ->
    {stop, normal, stopped, State};

handle_call(Unexpected, _From, #gensrv{handle_call = undefined} = State) ->
    {stop, {error_unexpected, Unexpected}, error_unexpected, State};

handle_call(Message, From, #gensrv{handle_call = HandleCall, opaque = Opaque} = State) ->
    try HandleCall(Message, From, Opaque) of
        {error, Reason} -> {stop, Reason, {error, Reason}, State};
        {ignore, ignore} -> {noreply, State};
        {ignore, NewOpaque} -> {noreply, State#gensrv{opaque = NewOpaque}};
        {Reply, ignore} -> {reply, Reply, State};
        {Reply, NewOpaque} -> {reply, Reply, State#gensrv{opaque = NewOpaque}}
    catch
        error:function_clause ->
            {stop, {error_unexpected, Message}, error_unexpected, State};
        Type:Reason ->
            {stop, {Type, Reason, erlang:get_stacktrace()}, {error, {Type, Reason}}, State}
    end.


handle_cast(Unexpected, #gensrv{handle_cast = undefined} = State) ->
    {stop, {error_unexpected, Unexpected}, State};

handle_cast(Message, #gensrv{handle_cast = HandleCast, opaque = Opaque} = State) ->
    try HandleCast(Message, Opaque) of
        {error, Reason} -> {stop, Reason, State};
        ignore -> {noreply, State};
        NewOpaque -> {noreply, State#gensrv{opaque = NewOpaque}}
    catch
        error:function_clause ->
            {stop, {error_unexpected, Message}, State};
        Type:Reason ->
            {stop, {Type, Reason, erlang:get_stacktrace()}, State}
    end.


handle_info(Unexpected, #gensrv{handle_info = undefined} = State) ->
    {stop, {error_unexpected, Unexpected}, State};

handle_info(Message, #gensrv{handle_info = HandleInfo, opaque = Opaque} = State) ->
    try HandleInfo(Message, Opaque) of
        {error, Reason} -> {stop, Reason, State};
        ignore -> {noreply, State};
        NewOpaque -> {noreply, State#gensrv{opaque = NewOpaque}}
    catch
        error:function_clause ->
            {stop, {error_unexpected, Message}, State};
        Type:Reason ->
            {stop, {Type, Reason, erlang:get_stacktrace()}, State}
    end.


code_change(_OldVsn, #gensrv{code_change = undefined} = State, _Extra) ->
    {ok, State};

code_change(_OldVsn, #gensrv{code_change = CodeChange, opaque = Opaque} = State, _Extra) ->
    try CodeChange(Opaque) of
        {error, Reason} -> {error, Reason};
        ignore -> {ok, State};
        NewOpaque -> {ok, State#gensrv{opaque = NewOpaque}}
    catch
        Type:Error ->
            {error, {Type, Error, erlang:get_stacktrace()}}
    end.


terminate(Reason, #gensrv{terminate = undefined} = _State) ->
    Reason;

terminate(Reason, #gensrv{terminate = Terminate, opaque = Opaque} = _State) ->
    Terminate(Reason, Opaque).

