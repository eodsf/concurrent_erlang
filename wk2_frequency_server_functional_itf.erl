%% Modified version of the frequency server 'refactored', which hides the inner workings of
%% the server & protocol behind a clean functional interface; the original was provided for the 
%% course, which has the following attribution:

%% Based on code from 
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(wk2_frequency_server_functional_itf).
-export([start/0,allocate/0,deallocate/1,stop/0]).
-export([init/0,test_suite/0]).

%% These are the start functions used to create and
%% initialize the server.

start() ->
    register(frequency,
	     spawn(?MODULE, init, [])).

init() ->
  Frequencies = {get_frequencies(), []},
  loop(Frequencies).

%% Hard Coded
get_frequencies() -> [10,11,12,13,14,15].

%% The Main Loop

loop(Frequencies) ->
  receive
    {request, Pid, allocate} ->
      {NewFrequencies, Reply} = allocate(Frequencies, Pid),
      Pid ! {reply, Reply},
      loop(NewFrequencies);
    {request, Pid , {deallocate, Freq}} ->
      NewFrequencies = deallocate(Frequencies, Freq),
      Pid ! {reply, ok},
      loop(NewFrequencies);
    {request, Pid, stop} ->
      Pid ! {reply, stopped}
	 %_->
		%io:format("Got unexpected message ~n",[]),
		%loop(Frequencies)
  end.

%% Functional interface

allocate() -> 
    frequency ! {request, self(), allocate},
    receive 
	    {reply, Reply} -> Reply
    end.

deallocate(Freq) -> 
    frequency ! {request, self(), {deallocate, Freq}},
    receive 
	    {reply, Reply} -> Reply
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
  {{Free, [{Freq, Pid}|Allocated]}, {ok, Freq}}.

deallocate({Free, Allocated}, Freq) ->
  NewAllocated=lists:keydelete(Freq, 1, Allocated),
  {[Freq|Free],  NewAllocated}.

%%
%% Additonal methods added for this exercise
%%

%% clear the process mailbox, ensuring it does not block unnecessarily.
%% the basic definition with no timeout was provided.
%% 

clear() ->
  receive
     _-> 				%% match on any message
		io:format("Cleared message: ~n",[]),			
		clear()				%% loop again
	after 0 ->			%% if there are no messages, after the specified time ...
		io:format("No more messages ~n",[]),
		ok					%% ...terminate.
  end.


%%
%% TESTS
%%

test_clear() ->
	start(),
	allocate(),
	allocate(),
	frequency ! "Hello",
	timer:sleep(1000),
	{message_queue_len,1} = erlang:process_info(whereis(frequency), message_queue_len),
	io:fwrite("Unprocessed messages: ~p\n", [erlang:process_info(whereis(frequency), messages)]),
	clear(),
	timer:sleep(1000),
	{message_queue_len,0} = erlang:process_info(whereis(frequency), message_queue_len),
	stop(),
pass.	
	
test_suite() ->
	pass = test_clear(),
pass.	
	