-module(performance_test).
-export([
         start/4,
         performance_working_cycle/3
        ]).

start(Seconds, ListOfFuncs, ValueFunc, ChangeValueEachSecs)
  when is_integer(Seconds) andalso is_list(ListOfFuncs) andalso ListOfFuncs =/= []
       andalso is_function(ValueFunc) andalso is_integer(ChangeValueEachSecs) ->
    InitialValue = ValueFunc(),
    {ok, TRef1} = timer:send_interval(1000 div length(ListOfFuncs), next_fun),
    {ok, TRef2} = timer:send_interval(ChangeValueEachSecs * 1000, change_value),
    timer:send_after(Seconds * 1000 + 500, done),
    Worker = spawn(?MODULE, performance_working_cycle, [lists:nth(1, ListOfFuncs), InitialValue, 0]),
    performance_control_cycle(Worker, {ListOfFuncs, 1, length(ListOfFuncs)}, ValueFunc, []),
    timer:cancel(TRef1),
    timer:cancel(TRef2),
    done.

performance_control_cycle(Worker, {ListOfFuncs, Current, Total}, ValueFunc, Counters) ->
    receive
        done ->
            Worker ! quit,
            done;
        change_value ->
            NewValue = ValueFunc(),
            Worker ! {change_value, NewValue},
            performance_control_cycle(Worker, {ListOfFuncs, Current, Total}, ValueFunc, Counters);
        next_fun when Current == Total ->
            Worker ! {change_fun, self(), lists:nth(1, ListOfFuncs)},
            receive {did, Counter} ->
                        performance_report(lists:reverse([Counter | Counters]))
            end,
            performance_control_cycle(Worker, {ListOfFuncs, 1, Total}, ValueFunc, []);
        next_fun when Current < Total ->
            Worker ! {change_fun, self(), lists:nth(Current + 1, ListOfFuncs)},
            receive {did, Counter} ->
                        performance_control_cycle(Worker, {ListOfFuncs, Current + 1, Total}, ValueFunc, [Counter | Counters])
            end;
        _ ->
            performance_control_cycle(Worker, {ListOfFuncs, Current, Total}, ValueFunc, Counters)
    end.

performance_working_cycle(Fun, Value, Counter) ->
    receive
        {change_value, NewValue} ->
            performance_working_cycle(Fun, NewValue, Counter);
        {change_fun, ReportTo, NewFun} ->
            ReportTo ! {did, Counter},
            performance_working_cycle(NewFun, Value, 0);
        quit ->
            done
    after 0 ->
            catch Fun(Value),
            performance_working_cycle(Fun, Value, Counter + 1)
    end.

performance_report([First | _] = Counters) ->
    Report = lists:map(fun(I) -> io_lib:format("f~b: ~bcps ~.1fx",
                                               [I, lists:nth(I, Counters), lists:nth(I, Counters) / First]) end,
                       lists:seq(1, length(Counters))),
    io:format(string:join(Report, ", ") ++ [10], []).
