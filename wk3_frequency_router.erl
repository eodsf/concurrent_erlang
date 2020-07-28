-module(wk3_frequency_router).
-export([start/0, allocate/0, deallocate/1, stop/0]).
-export([init/0]).

%% Frequency router client API
allocate() -> 
    handle(allocate).

deallocate(Freq) -> 
   handle({deallocate, Freq}).

stop() -> 
    handle(stop).

%% handles any of the interface commands
handle(Command) ->
    try frequency_router ! {request, self(), Command} of
        _ ->
            receive
                {reply, Reply} -> Reply
            after 500 ->
                erlang:error(timeout)
            end
    catch error:badarg ->
        erlang:error(serverdown)
    end.

%% entry point for the router; spawn the process, register & init.
start() ->
    register(frequency_router, spawn(?MODULE, init, [])).

%% Internal router functions

%% init sets up the router as a supervisor for the servers that
%% will handle the client load. We split the client load between 2
%% servers, whose frequency lists are provided to them.
init() ->
    erlang:process_flag(trap_exit, true),
    Servers = lists:map(
		fun({Id, Frequencies}) ->
			Pid = wk3_frequency_server_scaled:start(Frequencies),
			link(Pid),
			{Id, Pid} 
		end, 
		get_frequencies()),
    io:format("started router with servers: ~w.~n", [Servers]),
    loop(Servers).

%% Returns a hardcoded list of {id, frequencies} 
%% for each of 2 servers.
get_frequencies() ->
    [{freqs1, [10,11,12,13,14,15]}, 
    {freqs2, [20,21,22,23,24,25]}].

%% The main router loop.
loop(Servers) ->
    receive 
        {request, _, _} = Req ->
			%% handle any request type, passing along the server list
            case handle_request(Req, Servers) of
                ok -> ok;
                NewServers -> loop(NewServers)
            end;
        {'EXIT', Pid, _Reason} ->
            loop(handle_exited(Pid, Servers))
    end.

%% handle an allocation request; we send the allocation to the head of the list
%% of servers, and then concat that to the original list, effecting a round-robin distro.
handle_request({request, Pid, allocate}, [{Id, Server}|Servers]) ->
    Server ! {request, Pid, allocate},
    Servers ++ [{Id, Server}];
%% route deallocate requests for to the server handling that frequency
handle_request({request, Pid, {deallocate, Freq}}, Servers) ->
    case get_server_for(Freq) of
        {ok, Id} -> 
            Server = proplists:get_value(Id, Servers),
            Server !  {request, Pid, {deallocate, Freq}};
        {error, _} = Error ->
            Pid ! {reply, Error}
    end,
    Servers;

%% handle a stop request.
handle_request({request, Pid, stop}, Servers) ->
    stop_servers(Servers),
    Pid ! {reply, stopped},
    ok.

%% restart crashed server
handle_exited(Pid, Servers) ->
    case lists:keytake(Pid, 2, Servers) of
        {value, {Id, Pid}, Rest} ->    
            New = wk3_frequency_server_scaled:start(proplists:get_value(Id, get_frequencies())),
            link(New),
            [{Id, New}|Rest];
        false ->
            Servers
    end.

%% stop all servers in the provided list in succession.
stop_servers([]) ->
    ok;
stop_servers([{_, Server}|Servers]) ->
    _ = wk3_frequency_server_scaled:stop(Server),
    stop_servers(Servers).

%% returns the server id that is handling the given frequency
get_server_for(Freq) ->
    case lists:filter(
			fun({_, Frequencies}) -> 
				lists:member(Freq, Frequencies) 
			end, get_frequencies()) of
		[{Id, _}] -> {ok, Id};
		[] -> {error, illegal_frequency}
    end.
    