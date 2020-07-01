-module(wk1_mailbox).
-export([test_forwarder/0, test_forwarder_seq/0, forwarder/1, forwarder_seq/1]).

% This is a simple process function that is meant to be spawned, takes a process ID as argument,
% and simply runs in a tight loop, printing out any messages that it gets in its mailbox, 
% and forwarding those messages to the Pid provided.
% The message 'stop' will kill the process.

% See the accompanying test for usage of how it can be spawned from the shell, for example.

forwarder(Pid) ->    
    receive
        stop ->
			io:format("Stopping...~n"),
            ok;
        Msg ->
            io:format("Got message: ~w~n",[Msg]),			
			Pid ! Msg,								%fwd the message to the Pid provided
			forwarder(Pid)							%loop again
    end.
    
	
% This is a derivative of the above function; it is designed to process particular
% types of messages in sequence.
% As above, it forwards the messages to the provided Pid 	
forwarder_seq(Pid) ->
    receive
        {first, _FirstString} -> io:format("received first message.~n",[]),
		Pid ! {first, _FirstString}
    end,
    receive
        {second, _SecondString} -> io:format("received second message.~n",[]),
		Pid ! {second, _SecondString}
    end,
    receive
        {third, _AnyOtherString} -> io:format("received another message.~n",[]),
		Pid ! {third, _AnyOtherString}		
    end,
    receive
        stop -> io:format("Stopping...~n")
    end.
	
	
% Test for the forwarder/1 function process.

test_forwarder() ->
	% get the Pid of the test function	
	Self = self(),						
	
	% spawn our function under test, passing our test Pid as an argument.
	% note the convention for denoting 'this module'

	Forwarder = spawn(?MODULE, forwarder, [Self]),

	% send the spawned process a message
	Forwarder ! foo,
	
	% now, we can check the message that the spawned process got & fwd'd back to us
	% by checking our own mailbox
	receive
		Msg -> io:format("Test got message: ~w~n",[Msg]), 
				% pattern match on the message to ensure its the one sent
				% (this will throw an exception if it's not)
				foo = Msg
	end,	
	
	Forwarder ! stop,
	pass.
	
	
test_forwarder_seq() ->
	Self = self(),
	Forwarder = spawn(?MODULE, forwarder_seq, [Self]),
	
	% test by sending out of order; the function under test should receive in the 
	% order defined by its receive clause pattern matches
	Forwarder ! {third, mate},
	Forwarder ! {first, hi},
	Forwarder ! {second, there},
	
	timer:sleep(5000),	
	
	io:fwrite("PROCESS_INFO: ~p\n",
                                 [erlang:process_info( Self, message_queue_len)]),
								 
	% TODO : how to pull all messges from this mailbox in order to match on them ?
	receive
		Msg ->	io:format("Test got message: ~w~n",[Msg])
				%,{first, hi} = Msg
	end,	
	
	timer:sleep(5000),	
	Forwarder ! stop,
	pass.
