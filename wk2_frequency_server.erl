%% Modified version of the frequency server provided for the 
%% course, which has the following attribution:

%% Based on code from 
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson


%% Tests are included in the same module for convenience.

-module(wk2_frequency_server).
-export([init/0,start/0, test_suite/0]).


%% These are the start functions used to create and
%% initialize the server.

%% spawn the function process, using the init function to 
%% initialize its state, and register it under the given name
start() ->
    register(frequency_server,
         spawn(?MODULE, init, [])).

%% Init w/ hardcoded frequencies, and start the main receive loop.
init() ->
  Frequencies = {get_frequencies(), []},
  loop(Frequencies).

%% Hard coded frequencies used to init the server.
get_frequencies() -> [10,11,12,13,14,15].


%% The Main Loop

%% matches on messages from clients having the tuple form
%% {request, <Pid>, <allocate | deallocate | stop>}
%% and acts on the command contained  in the last part of the 
%% tuple acordingly. 
%% The Pid of the process requesting the actionn is notified
%% of  the outcome.
%% The internal state of the frequencies is re-computed
%% as part of the allocate or deallocate action, and 
%% fed back in as the new state of the next loop (recursion)
loop(Frequencies) ->
  receive
    {request, Pid, allocate} ->
      {NewFrequencies, Reply} = allocate(Frequencies, Pid),
      Pid ! {reply, Reply},
      loop(NewFrequencies);
    {request, Pid , {deallocate, Freq}} ->
      {NewFrequencies, Reply} = deallocate(Frequencies, Freq, Pid),
      Pid ! {reply, Reply},
      loop(NewFrequencies);
    {request, Pid, stop} ->
      Pid ! {reply, stopped}
  end.

%% The Internal Help Functions used to allocate and
%% deallocate frequencies.

%% Expected arguments : 
%%	i) a tuple of 2 lists representing the available frequencies & the allocated ones;
%% 		the Allocated list is expected have members of tuples in the form {Freq, Pid}
%% 		that indicate which freq has been allocated to a Pid
%%	ii) the requesting Pid
%% Returns : 
%%		if allocation succeeds, a tuple w/ 2 sub-tuples of the form :
%%			i) {[a list of the free freuencies], [a list of, first, one tuple
%%				containing the frequency just allocated & the pid, followed by the
%%				rest of the allocated frequencies]
%%			ii) a tuple containing atom 'ok' with the frequency just allocated.
%%	
allocate({[], Allocated}, _Pid) ->
	%% no available frequencies, return an error
  {{[], Allocated}, {error, no_frequency}};
  
allocate({[Freq|Free], Allocated}, Pid) ->
	%% attempt to allocate the head of the provided free freqs to the Pid;
	%% compare the 2nd element of all tuples in the tuple list against Pid to 
	%% determine allocation.
  case lists:keymember(Pid, 2, Allocated) of
    true ->
		%% the Pid was determined to be allocated to the freq at the head of the
		%% provided 'free' freqs, return an error
      {{[Freq|Free], Allocated}, {error, already_allocated}};
    _ ->
		%% catch all case : allocate the head of the 'free' freqs to the Pid, creating
		%% expected the tuple & sticking that at the head of the Allocated list, 
		%% which is returned along with the remaining free freqs & the tuple containing
		%% the ok atom & the just allocated freq
      {{Free, [{Freq, Pid}|Allocated]}, {ok, Freq}}
  end.

deallocate({Free, Allocated}, Freq, Pid) ->
	%% similar to the allocate function, attempt to match the 'Freq' in the tuple list 'Allocated'
	%% where the tuples in that list are in the form {Freq, Pid}
	%% It may be found, or not (not allocated to any Pid)
	FreqAllocFound = lists:keyfind(Freq, 1, Allocated),
	case FreqAllocFound of
		false ->
			%% the frequency 'Freq' is not allocated to any Pid
			{{Free, Allocated}, {error, not_allocated}};		
		_ ->
			io:format("Alloc found for freq : ~w : ~w~n",[Freq, FreqAllocFound]),
			%% Now we need to determine if the requesting Pid is allocated this frequency 
			%% & therefore has permission to deallocate it.
			case match_pid(FreqAllocFound, Pid) of 
				true ->
					%% the freq was allocated to the Pid provided;
					%% it can be deleted from Allocated, and is added to head of Free,
					%% and we return both				
					NewAllocated=lists:keydelete(Freq, 1, Allocated),
					{{[Freq|Free],  NewAllocated}, {ok, deallocated}};				
				_->
					%% the freq was not allocated to the Pid provided, deny the request
					{{Free, Allocated}, {error, not_authorized}}
			end
	end.

%% Helper function to match a provided Pid w/ a Pid in the tuple, returning true or false.
match_pid({_Freq, Pid}, PidToMatch) ->
	Pid=:=PidToMatch.

%% Helper function used to start a throw-away process for testing.
temp_process() ->
    receive
        Msg ->
            io:format("Tmp process got message: ~w~n",[Msg]),			
			temp_process()							%% loop to keep alive
    end.



%% TESTS

%% helper method to request allocation to a different Pid than our test method.
%% This creates a temp process for the purpose, so that our frequency server can messge it
%% via its Pid, rather than using c:pid(0,250,0) to gen a fake Pid that is not attached / backed
%% by a running process.
allocate(Freq_server) ->
	FakePid = spawn(?MODULE, temp_process, []),
	Freq_server ! {request, FakePid, allocate}.
  
%% tests the private allocate method directly
test_allocate_internal() ->
	FakePid = c:pid(0,250,0),
	Self = self(),
	%% try an allocation given no avail or allocated freqs
	{{[],[]}, {error, no_frequency}} = allocate({[],[]}, Self),
	%% try an allocation given no avail but some allocated freqs
	Allocated = [{1,FakePid}],
	{{[], Allocated}, {error, no_frequency}} = allocate({[],Allocated}, Self),
	
	%% perform a sucessful allocation; range 4-6 are available, 1-3 are taken.
	%% we get back, in the first tuple's first list the remaining unallocated freqs,
	%% followed by a list containing first, a tuple of the one we were allocated along
	%% with our Pid, followed by the rest of the allocated ones, and in the 2nd tuple,
	%% the 'ok' atom along with the allocated freq.
	NewAllocated = [{4, Self},{1,FakePid}],
	{{[5,6], NewAllocated}, {ok, 4}} = allocate({[4,5,6], Allocated}, Self),
	%% Now if we try to reallocate the same freq (to a different Pid) again it should fail.
	FakePid2 = c:pid(0,251,0),
	{{[4,5,6], NewAllocated}, {error, already_allocated}} = allocate({[4,5,6], NewAllocated}, FakePid2),
	
	%% or if we try to allocate another frequency we should also get an error (only 1 allocation allowed)
	%% Now if we try to reallocate the same freq again it should fail.
	{{[5,6], NewAllocated}, {error, ok}} = allocate({[5,6], NewAllocated}, Self),
	

pass.

%% tests the private deallocate method directly
test_deallocate_internal() ->
	Self = self(),
	%% set up the condition where freqs 1-3 are allocated to our Pid, 4-6 are 'free'
	%% deallocation should be successful for '1', and we expect the freed freq to be 
	%% head of the 'free' list returned.
	{{[1,4,5,6], [{2,Self},{3,Self}]}, {ok, deallocated}} = 
		deallocate({[4,5,6], [{1,Self}, {2,Self}, {3,Self}]}, 1, Self),

pass.

%% based on the internal tests for allocate, this should be self-explanatory
test_server_allocate() ->
	Self = self(),
	Freq_server = spawn(?MODULE, init, []),
	%% request an allocation
	Freq_server ! {request, Self, allocate},
	%% we expect a success msg; match expected reply for the allocated one (first in our hardcoded seq)
	ExpectedAllocatedFreq = 10,
	receive
		Alloc ->
				{reply,{ok, ExpectedAllocatedFreq}} = Alloc
	end,	
	Freq_server ! {request, Self, stop},
	receive
		Stop ->
				{reply,stopped} = Stop
	end,	
pass.


test_server_deallocate() ->
	Self = self(),
	Freq_server = spawn(?MODULE, init, []),
	%% request a deallocation of a non-existant frequency
	Freq_server ! {request, Self, {deallocate, 1}},
	receive
		DeallocNonExistant ->
			{reply,{error, not_allocated}} = DeallocNonExistant
	end,	
	%% request an allocation from our helper function (different Pid); since we are using
	%% hardcoded freqs, the first in the list should be allocated.
	FreqAllocatedToAnother = 10,
	allocate(Freq_server),
	%% and try and request dealloc of that freq
	Freq_server ! {request, Self, {deallocate, FreqAllocatedToAnother}},
	receive
		DeallocUnauth ->
			{reply,{error,not_authorized}} = DeallocUnauth
	end,	
	%% Finally, wrap up by allocating the next in the sequence of frequencies to our
	%% process, and deallocate it
	ExpectedAllocatedFreq = 11,
	Freq_server ! {request, Self, allocate},
	receive
		Alloc ->
			{reply,{ok, ExpectedAllocatedFreq}} = Alloc
	end,	
	Freq_server ! {request, Self, {deallocate, ExpectedAllocatedFreq}},
	receive
		Dealloc ->
			{reply,{ok, deallocated}} = Dealloc
	end,		
	
	Freq_server ! {request, Self, stop},
	receive
		Stopped ->
			{reply,stopped} = Stopped
	end,		
pass.

test_match_pid() ->
	Self = self(),
	false = match_pid({1, 100}, Self),
	true = match_pid({1, Self}, Self),
pass.	

test_suite() ->
	pass = test_match_pid(),
	pass = test_allocate_internal(),
	pass = test_deallocate_internal(),
	pass = test_server_allocate(),
	pass = test_server_deallocate(),
pass.


