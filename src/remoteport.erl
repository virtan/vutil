-module(remoteport).

-export([
         start/3,
         stop/1,
         port_loop/3
        ]).

% returns pid of remote process
start(Node, PortName, PortSettings) ->
    Caller = self(),
    spawn(Node, ?MODULE, port_loop, [Caller, PortName, PortSettings]).

stop(Pid) ->
    Pid ! {self(), close}.

port_loop(Caller, PortName, PortSettings) ->
    Port = open_port(PortName, PortSettings),
    port_loop_2(Caller, Port).

port_loop_2(Caller, Port) ->
    receive
        {Caller, {command, Data}} ->
            Port ! {self(), {command, Data}},
            port_loop_2(Caller, Port);
        {Caller, close} ->
            Port ! {self(), close},
            port_loop_2(Caller, Port);
        {Caller, {connect, NewCaller}} ->
            Caller ! {self(), connected},
            port_loop_2(NewCaller, Port);
        {Port, {data, Data}} ->
            Caller ! {self(), {data, Data}},
            port_loop_2(Caller, Port);
        {Port, closed} ->
            Caller ! {self(), closed},
            ok;
        {'EXIT', Port, Reason} ->
            Caller ! {'EXIT', self(), Reason},
            ok;
        _Ignore ->
            port_loop_2(Caller, Port)
    end.
