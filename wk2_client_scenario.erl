-module(scenario).
-export([setup/0,client/3	,random_elem/1]).

% Use this module to exercise the behaviour of the 
% hardened frequency server.

% Calling setup will launch the server and two clients: alice and bob.

setup() ->
    frequency_hardened:start(),
	AliceTime = floor((rand:uniform() * 100)),
	BobTime = floor((rand:uniform() * 100)),
    AlicePid  =  spawn(?MODULE,client,[alice,[], AliceTime]),
    BobPid = spawn(?MODULE,client,[bob,[], BobTime]),	
	io:format("Spawned client alice with Pid ~w & time ~w, and client bob with Pid ~w and time ~w.~n", 
		[AlicePid, AliceTime, BobPid, BobTime]).

% A client, parametrised by its name (optional, but useful instrumentation),
% and the list of frequencies currently allocated to that process. Needed
% to produce calls to deallocate/1 that don't fail.

% Could also 
%   - parameterise on the ratio of allocates to deallocates
%   - deal with case when no frequencies available: here a client fails
%   - add stop commands.

%% Main loop of the client 'simulation'; based on a random no. gen'd, it
%% a client either tries to allocate or deallocate a frequency. Client are set up
%% to loop a set number of times, also based on a random init'd in the setup().
%% After a client's simulation ends, it deallocates its frequencies by calling the 
%% server, which in turn 

%% This handles the case where the client should be stopped
%% (its simulation time is up)
client(Id, _Freqs, Iterations) when Iterations =< 0 ->	
	io:format("Client ~w terminating.~n",[Id]),
	%% Note : I initially added a manual de-allocate, but if the server
	%% is linked to these processes, and setup to trap exists, it can deallocate
	%% the frequencies when handling that EXIT signal, as it does in this impl.
	%deallocate_all_freqs(Freqs, Id);
	io:format("Stopping client  ~w.~n", [Id]);
	
%% 
client(Id, Freqs, Iterations) when Iterations > 0 ->
	io:format("Client ~w will iterate ~w more times.~n", [Id, Iterations]),
    case rand:uniform(2) of
        1 -> 
            Reply = frequency_hardened:allocate(),
			case Reply of
				{ok,Freq} ->
					io:format("Frequency ~w allocated to client ~w.~n", [Freq,Id]),
					timer:sleep(1000),
					client(Id,[Freq|Freqs], Iterations-1);
				%% handle the case where the server is out of frequencies to
				%% distribute; if not handled, the client crashes; if the freq server
				%% has linked to the clients, and is set to trap exists, it could 
				%% deallocate the freqs of the client when processing the EXIT signal
				%% (which it does in this impl).
				{error, no_frequency} ->
					io:format("No more frequencies left...~n", []),
					client(Id,Freqs, Iterations-1)				
			end;
        2 ->
            Len = length(Freqs),
            case Len of 
                0 -> 
                    io:format("No frequencies to deallocate by client ~w.~n", [Id]),
                    timer:sleep(1000),
                    client(Id,Freqs, Iterations-1);  
                _ -> 
                    Freq = lists:nth(rand:uniform(Len),Freqs),
					io:format("Attempting : Frequency ~w deallocation by client ~w.~n", [Freq,Id]),
                    frequency_hardened:deallocate(Freq), 
                    io:format("Frequency ~w deallocated by client ~w.~n", [Freq,Id]),
                    timer:sleep(1000),
                    client(Id,lists:delete(Freq,Freqs), Iterations-1)
            end
    end.

deallocate_all_freqs([], Id) ->
	io:format("All Frequencies deallocated by client ~w.~n", [Id]);
deallocate_all_freqs([F|Fs], Id) ->
	frequency_hardened:deallocate(F),
	io:format("Frequency ~w deallocated by client ~w.~n", [F,Id]),
	timer:sleep(1000),
	deallocate_all_freqs(Fs, Id).
	


% for debugging purposes: chooses a random element of a non-empty list.

random_elem([]) ->
    empty;
random_elem(Xs) ->
    Len = length(Xs),
    lists:nth(rand:uniform(Len),Xs).  
