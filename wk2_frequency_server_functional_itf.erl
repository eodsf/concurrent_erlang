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
    {request, Pid, allocate, SleepFor} ->
	  timer:sleep(SleepFor),	
      {NewFrequencies, Reply} = allocate(Frequencies, Pid),
      Pid ! {reply, Reply},
      loop(NewFrequencies);  
  
    {request, Pid, allocate} ->
      {NewFrequencies, Reply} = allocate(Frequencies, Pid),
      Pid ! {reply, Reply},
      loop(NewFrequencies);
    {request, Pid , {deallocate, Freq}} ->
      NewFrequencies = deallocate(Frequencies, Freq),
      Pid ! {reply, ok},
      loop(NewFrequencies);
    {request, Pid, stop} ->	 
	  %% as a last cleanup step before exiting, especially useful 
	  %% for re-running tests from the same shell.
	  unregister(frequency),
      Pid ! {reply, stopped}
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
	clear(),
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
  io:fwrite("Stopping : Clearing unprocessed messages: ~p\n", 
			[erlang:process_info(whereis(frequency), messages)]),
  receive
     _Msg -> 				%% match on any message
		clear()				%% loop again
	after 0 ->			%% if there are no messages, after the specified time ...
		ok					%% ...terminate the function.
  end.

%% Helper function to ensure that a client calling the server fails 
%% when the specified time has elapsed w/o receiving a message
client_receive_timeout(Timeout) ->
  receive 
	Msg -> 
		io:format("client_receive_timeout : got message: ~w~n",[Msg]),
		Msg									%% return Msg
	after Timeout -> 
		{reply, error_receive_timeout}
  end.

%% Helper function to send out-of-band messages to freq server.
spam_server_mailbox(0,_) ->
	[];

	spam_server_mailbox(N, Term) when N > 0 ->
		frequency ! "Spam",
		[Term | spam_server_mailbox(N-1, Term)].

%%
%% TESTS
%%

test_clear() ->
	start(),
	%% these messages will get processed as part of the normal processing loop
	allocate(),
	allocate(),	
	%% send a message that is not supported by our client interface
	{message_queue_len,0} = erlang:process_info(whereis(frequency), message_queue_len),
	frequency ! "Hello",		
	{message_queue_len,1} = erlang:process_info(whereis(frequency), message_queue_len),	
	%% in the stop() the mailbox is cleared
	stop(),
pass.	


test_client_timeout() ->
  start(),
  ClientTimoutThreshold = 200,
  %% verify our process is started
  true = is_process_alive(whereis(frequency)),
  %% to test the client timeout on the stop, we need to stop the server like so, passing our
  %% Pid to it so that client_receive_timeout/1 can check our mailbox
  frequency ! {request, self(), allocate},
  %% the server is not behind on processing, so we expect no error
  {reply,{ok,_}} = client_receive_timeout(ClientTimoutThreshold),
  %% to help simulate a slow server response, fill the server mailbox with some messages that don't
  %% pattern match (match in the main processing loop
  N = 100,
  spam_server_mailbox(N, 1),
  {message_queue_len, N} = erlang:process_info(whereis(frequency), message_queue_len),
  
  frequency ! {request, self(), allocate},
  %% despite the server mailbox having unprocessed messages, it still efficiently matches 
  %% an expected message and delivers on time.
  {reply,{ok,_}} = client_receive_timeout(ClientTimoutThreshold),
  
  %% so let's try another tact to delay server processing of the allocate ... we have introduced
  %% a new message that takes a timeout for this purpose.
  frequency ! {request, self(), allocate, ClientTimoutThreshold*3},
  %% and this does the trick ... 
  {reply, error_receive_timeout} = client_receive_timeout(ClientTimoutThreshold),
  stop(),
pass.

	
test_suite() ->
	%%pass = test_clear(),
	pass = test_client_timeout(),
pass.	
	