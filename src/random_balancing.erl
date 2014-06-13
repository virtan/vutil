-module(random_balancing).

-behaviour(supervisor).

-export([
         start_link/2,
         start_link/3,
         start_link/4,
         child_spec/0,
         pick_worker/1,
         not_abuse/2
        ]).

-export([init/1]).

child_spec() ->
    {?MODULE,
     {?MODULE, start_link, []},
     permanent, infinity, supervisor,
     [?MODULE]
    }.

%% Options is a plist of
%% {strategy, _} %% default is one_for_one
%% {maxr, _} %% default is 10
%% {maxt, _} %% default is 5000

start_link(WorkerModule, NumberOfWorkers) when is_atom(WorkerModule) andalso
                                               is_integer(NumberOfWorkers) ->
    start_link(WorkerModule, NumberOfWorkers, []).

start_link(WorkerModule, NumberOfWorkers, Options) when is_atom(WorkerModule) andalso
                                                        is_integer(NumberOfWorkers) andalso
                                                        is_list(Options) ->
    supervisor:start_link(?MODULE, {WorkerModule, NumberOfWorkers, Options});

start_link(Name, WorkerModule, NumberOfWorkers) when is_atom(Name) andalso
                                                     is_atom(WorkerModule) andalso
                                                     is_integer(NumberOfWorkers) ->
    start_link({local, Name}, WorkerModule, NumberOfWorkers, []);

start_link(TupleName, WorkerModule, NumberOfWorkers) when is_tuple(TupleName) andalso
                                                          is_atom(WorkerModule) andalso
                                                          is_integer(NumberOfWorkers) ->
    start_link(TupleName, WorkerModule, NumberOfWorkers, []).

start_link(Name, WorkerModule, NumberOfWorkers, Options) when is_atom(Name) andalso
                                                              is_atom(WorkerModule) andalso
                                                              is_integer(NumberOfWorkers) andalso
                                                              is_list(Options) ->
    start_link({local, Name}, WorkerModule, NumberOfWorkers, Options);

start_link(TupleName, WorkerModule, NumberOfWorkers, Options) when is_tuple(TupleName) andalso
                                                                   is_atom(WorkerModule) andalso
                                                                   is_integer(NumberOfWorkers) andalso
                                                                   is_list(Options) ->
    supervisor:start_link(TupleName, ?MODULE, {WorkerModule, NumberOfWorkers, Options}).

init({WorkerModule, NumberOfWorkers, Options}) ->
    Workers = [WorkerModule:child_spec(N) || N <- lists:seq(1, NumberOfWorkers)],
    {ok, {{
       proplists:get_value(strategy, Options, one_for_one),
       proplists:get_value(maxr, Options, 50),
       proplists:get_value(maxt, Options, 1000)},
          Workers}}.

-spec pick_worker(pid() | atom()) -> {ok, pid()} | {error, no_active_workers}.
pick_worker(Supervisor) ->
    Children = supervisor:which_children(Supervisor),
    ActiveChildren = [Pid || {_, Pid, _, _} <- Children, is_pid(Pid)],
    pick_random_worker(ActiveChildren).

pick_random_worker([]) ->
    {error, no_active_workers};
pick_random_worker(Workers) when is_list(Workers) ->
    NumWorkers = length(Workers),
    RandomWorker = erlang:phash2(make_ref(), NumWorkers) + 1,
    {ok, lists:nth(RandomWorker, Workers)}.

-spec not_abuse(integer(), {ok, pid()} | pid() | {error, term()}) ->
    {ok, pid()} | {error, worker_busy} | {error, worker_unavailable} | {error, term()}.
not_abuse(_, {error, _} = Error) -> Error;
not_abuse(MaxQueueLength, {ok, Pid}) when is_integer(MaxQueueLength) andalso
                                          is_pid(Pid) ->
    not_abuse(MaxQueueLength, Pid);
not_abuse(MaxQueueLength, Pid) when is_integer(MaxQueueLength) andalso
                                    is_pid(Pid) ->
    try
        case process_info(Pid, message_queue_len) of
            {message_queue_len, MessageQueueLength} when MessageQueueLength < MaxQueueLength ->
                {ok, Pid};
            {message_queue_len, _} ->
                {error, worker_busy}
        end
    catch
        _:_ -> {error, worker_unavailable}
    end.
