%% Based on code from 
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(wk3_frequency_hardened_exceptions).
-export([start/0,allocate/0,deallocate/1,stop/0]).
-export([init/0]).

%% These are the start functions used to create and
%% initialize the server.

start() ->
	Pid = spawn(?MODULE, init, []),
	io:format("Spawned freq server with Pid ~w.~n", [Pid]), 
    register(frequency, Pid).

init() ->
	%% this process will be notified of any processes that it links to
	%% if those processes die/exit.
  process_flag(trap_exit, true),    %%% ADDED
  Frequencies = {get_frequencies(), []},
  loop(Frequencies).

% Hard Coded
get_frequencies() -> [10,11,12,13,14,15].

%% The Main Loop

loop(Frequencies) ->
  receive
    {request, Pid, allocate} ->
      {NewFrequencies, Reply} = allocate(Frequencies, Pid),
      Pid ! {reply, Reply},
      loop(NewFrequencies);
    {request, Pid , {deallocate, Freq}} ->		
		try deallocate(Frequencies, Freq) of 
			NewFrequencies ->	
				%% normal case, we get the new freqs back
			  Pid ! {reply, ok},
			  loop(NewFrequencies)
			catch error:Error ->
				Err = {error, Error},
				io:format("Signalling bad deallocation request ~w to ~w.~n",[Err, Pid]),
				Pid ! Err,
				loop(Frequencies)
			end;
    {request, Pid, stop} ->
      Pid ! {reply, stopped};
    {'EXIT', Pid, _Reason} ->
		%% process the exit message of any linked processes
      NewFrequencies = exited(Frequencies, Pid), 
      loop(NewFrequencies)
  end.

%% Functional interface

allocate() -> 
    frequency ! {request, self(), allocate},
    receive 
      {reply, Reply} -> Reply;
      _              -> ok
    end.

deallocate(Freq) -> 
    frequency ! {request, self(), {deallocate, Freq}},
    receive 
      {reply, Reply} -> Reply;
      _              -> ok
    end.

stop() -> 
    frequency ! {request, self(), stop},
    receive 
	    {reply, Reply} -> Reply
    end.


%% The Internal Help Functions used to allocate and
%% deallocate frequencies.

allocate({[], Allocated}, _Pid) ->
  {{[], Allocated}, {error, no_frequency}};
allocate({[Freq|Free], Allocated}, Pid) ->
	%% set up a link to the client requesting the allocation
  link(Pid),								
  {{Free, [{Freq, Pid}|Allocated]}, {ok, Freq}}.

deallocate({Free, Allocated}, Freq) ->
	%% handle the case where a de-allocation of a freq
	%% is requested by a client that doesn't hold that freq.
	%% i) first, handle the possible keysearch failure
	%% ii) raise an error (arguably, we could just do nothing).
  case lists:keysearch(Freq,1,Allocated) of
	  {value,{Freq,Pid}} ->
		  {value,{Freq,Pid}} = lists:keysearch(Freq,1,Allocated),
		  %% unlink the client requesting the de-allocation
		  unlink(Pid),                                           
		  NewAllocated=lists:keydelete(Freq, 1, Allocated),
		  {[Freq|Free],  NewAllocated};
	   false ->
			io:format("Got a request to deallocate freq ~w, and the lookup failed, erroring.~n", [Freq]),
			erlang:error(bad_dealloc_request)
   
   end.

%% Handle the termination of the client by de-allocating all its frequencies
exited({Free, Allocated}, Pid) ->
	io:format("Server trapped the exit of Pid ~w, and will deallocate its freqs.~n", [Pid]), 
    case lists:keysearch(Pid,2,Allocated) of
      {value,{Freq,Pid}} ->
		unlink(Pid),
        NewAllocated = lists:keydelete(Freq,1,Allocated),
        {[Freq|Free],NewAllocated};		
      false ->
        {Free,Allocated} 
    end.