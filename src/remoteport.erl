-module(remoteport).

-export([
         start/3,
         stop/1,
         port_loop/3
        ]).

% returns pid of remote process
start(Node, PortName, PortSettings) ->
    Caller = self(),
    spawn_link(Node, ?MODULE, port_loop, [Caller, PortName, PortSettings]).

stop(Pid) ->
    Pid ! {self(), close}.

port_loop(Caller, PortName, PortSettings) ->
    Port = open_port(PortName, PortSettings),
    port_loop_2(Caller, Port).

port_loop_2(Caller, Port) ->
    receive
        {Caller, {command, Data}} ->
            io:format("sending to port~n", []),
            Port ! {self(), {command, Data}},
            port_loop_2(Caller, Port);
        {Caller, close} ->
            io:format("closing port~n", []),
            Port ! {self(), close},
            port_loop_2(Caller, Port);
        {Caller, {connect, NewCaller}} ->
            Caller ! {self(), connected},
            port_loop_2(NewCaller, Port);
        {Port, {data, Data}} ->
            io:format("data from port~n", []),
            Caller ! {self(), {data, Data}},
            port_loop_2(Caller, Port);
        {Port, closed} ->
            io:format("port closed~n", []),
            Caller ! {self(), closed},
            ok;
        {Port, {exit_status, Status}} ->
            Caller ! {self(), {exit_status, Status}},
            ok;
        {'EXIT', Port, Reason} ->
            Caller ! {'EXIT', self(), Reason},
            ok;
        _Ignore ->
            port_loop_2(Caller, Port)
    end.
