-module(wk3_client_scenario_exceptions).
-export([setup/0,setup/2,client/5,random_elem/1]).

%% A client test simulator for the hardened frequency server.


%% Calling setup will launch the server and two clients: alice and bob.
%% The server has a functional interface to allow client to request allocations of
%% frequencies, and to allow de-allocation of the same.
setup() ->
	setup(false, false).

%% An expanded version of setup that allows random 'chaos' of 
%% trying to deallocate an unallocted frequency & unknown commands 
%% to the server to examine exception handling.

setup(TryDeallocUnallocedFreq, TrySendingUnknownMsg) ->
    wk3_frequency_hardened_exceptions:start(),
	AliceTime = floor((rand:uniform() * 100)),
	BobTime = floor((rand:uniform() * 100)),
	%% spawn each client process in turn
    AlicePid  =  spawn(?MODULE,client,[alice,[], AliceTime, TryDeallocUnallocedFreq, TrySendingUnknownMsg]),
    BobPid = spawn(?MODULE,client,[bob,[], BobTime, TryDeallocUnallocedFreq, TrySendingUnknownMsg]),	
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

%% This clause handles the case where the client should be stopped
%% (its simulation time is up)
client(Id, _Freqs, Iterations, _TryDeallocUnallocedFreq, _TrySendingUnknownMsg) when Iterations =< 0 ->	
	io:format("Client ~w terminating.~n",[Id]),
	%% Note : I initially added a manual de-allocate, but if the server
	%% is linked to these processes, and setup to trap exists, it can deallocate
	%% the frequencies when handling that EXIT signal, as it does in this impl.
	%% Special note: if you are starting the server through this simulation, using the
	%% setup(), once the last client dies, you will not see the ""Server trapped the exit of Pid..."
	%% for that client, because the client process (eg, this module) will have terminated ... the server
	%% still lives, as can be verified from issuing this at the shell (using the registered name):
	%% whereis(frequency).
	%deallocate_all_freqs(Freqs, Id);
	io:format("Stopping client  ~w.~n", [Id]);
	
%% This clause handles the normal case where we try an allocation or deallocation.
client(Id, Freqs, Iterations, TryDeallocUnallocedFreq, TrySendingUnknownMsg) when Iterations > 0 ->
	io:format("Client ~w will iterate ~w more times.~n", [Id, Iterations]),
	
	%% Note: this param check could have been part of a guard clause for the function (arguably cleaner?)
	case TrySendingUnknownMsg of 
		true -> 
			io:format("Sending unhandled message type to frequency server.~n", []),
			%% If the server doesn't explicitly handle this message in its receive loop, or throw
			%% an exception or erorr, the message will just sit in its mailbox; 
			%% and unless the client here expects some reply from the server where pattern matching 
			%% might fail, the process will remain alive.
			frequency ! "Unexpected Msg type",
			client(Id,Freqs, Iterations-1, TryDeallocUnallocedFreq, TrySendingUnknownMsg);
		_->			
			%% Normal case of an expected message type.
			case rand:uniform(2) of
				1 -> 
					Reply = wk3_frequency_hardened_exceptions:allocate(),
					case Reply of
						{ok,Freq} ->
							io:format("Frequency ~w allocated to client ~w.~n", [Freq,Id]),
							timer:sleep(1000),
							client(Id,[Freq|Freqs], Iterations-1, TryDeallocUnallocedFreq, TrySendingUnknownMsg);
						%% handle the case where the server is out of frequencies to
						%% distribute; if not handled, the client crashes; if the freq server
						%% has linked to the clients, and is set to trap exists, it could 
						%% deallocate the freqs of the client when processing the EXIT signal
						%% (which it does in this impl).
						{error, no_frequency} ->
							io:format("No more frequencies available this time...~n", []),
							client(Id,Freqs, Iterations-1, TryDeallocUnallocedFreq, TrySendingUnknownMsg)				
					end;
				2 ->
					Len = length(Freqs),
					case Len of 
						0 -> 
							io:format("No frequencies to deallocate for client ~w.~n", [Id]),
							timer:sleep(1000),
							client(Id,Freqs, Iterations-1,TryDeallocUnallocedFreq, TrySendingUnknownMsg);  
						_ -> 
							case TryDeallocUnallocedFreq of
								true ->
									%% try a deallocation of a freq out of bounds for this client
									Sorted = lists:sort(Freqs),
									LastFreq = lists:last(Sorted),
									ToDealloacte = LastFreq+1,
									io:format("Attempting : Frequency ~w deallocation by client ~w who holds frequencies ~w.~n", 
											[ToDealloacte,Id, Freqs]),
									wk3_frequency_hardened_exceptions:deallocate(ToDealloacte),
									client(Id, Freqs, Iterations-1, TryDeallocUnallocedFreq, TrySendingUnknownMsg);
								_->						
									Freq = lists:nth(rand:uniform(Len),Freqs),
									io:format("Attempting : Frequency ~w deallocation by client ~w who holds frequencies ~w.~n", 
											[Freq,Id, Freqs]),
									ok = wk3_frequency_hardened_exceptions:deallocate(Freq), 
									io:format("Frequency ~w deallocated by client ~w.~n", [Freq,Id]),
									timer:sleep(1000),
									client(Id,lists:delete(Freq,Freqs), Iterations-1, TryDeallocUnallocedFreq, TrySendingUnknownMsg)
							end
					end
			end
	end.
		

deallocate_all_freqs([], Id) ->
	io:format("All Frequencies deallocated by client ~w.~n", [Id]);
deallocate_all_freqs([F|Fs], Id) ->
	wk3_frequency_hardened_exceptions:deallocate(F),
	io:format("Frequency ~w deallocated by client ~w.~n", [F,Id]),
	timer:sleep(1000),
	deallocate_all_freqs(Fs, Id).
	


% for debugging purposes: chooses a random element of a non-empty list.

random_elem([]) ->
    empty;
random_elem(Xs) ->
    Len = length(Xs),
    lists:nth(rand:uniform(Len),Xs).  
