-module(wk2_super).
-export([super/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Observations from experimenting with killing the 'echo' & 'talk' process from the shell 
%% as outlined the task suggestions:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 		- Killing the 'talk' process using "exit(Pid,kill)." kills the talk process only.
%% 		(this can be verified to using the following command in the shell to verify the echo server
%% 		is still alive : "process_info(whereis(talk)")
%% 		- If I re-spawn the 'talk' process in the same manner ("Pid=spawn(talk,worker,[])."), 
%% 		the echo process picks right up & starts echoing again.

%% 		- Killing the echo process ("exit(whereis(echo),kill).") results in a runtime error in the
%% 		talk process when it tries to send its message to that process; my shell reported the error
%% 		as follows:

%% 		"
%% 		=ERROR REPORT==== 12-Jul-2020::12:46:41.636000 ===
%% 		Error in process <0.114.0> with exit value:
%% 		{badarg,[{wk2_talk,work,1,[{file,"wk2_talk.erl"},{line,9}]}]}

%% 		** exception error: bad argument
%% 			 in function  exit/2
%%				called as exit(undefined,kill)
%% 		"		
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Main entry point for the supervisor, which also starts the 2 child processes.

%% The following commentary is part of the assignment, observing the effects of killing either of the
%% child processes, and the killing of the echo process with and without a sleep before the supervisor
%% respawns it:

%% ====================== START OF COMMENTARY ===========================
%% The following observations are with the 'timer:sleep(1000)' code uncommented in the '{'EXIT', E, _} ->' block of code:

%% i) start observer from the shell (observer:start().)

%% ii) start the supervisor process :

%% 2> wk2_super:super().
%% echo spawned as Pid <0.97.0>.
%% worked spawned as Pid <0.98.0>.
%% {<0.98.0>,0} sent.
%% 0 echoed.
%% {<0.98.0>,1} sent.
%% ....

%% iii) kill the echo process from observer - notice that because talk is not coded 
%% 'defensively', and it expects echo to be up, it encounters an error sending it's message 
%% to echo, and also crashes ... the end result is that both processes are restarted by the 
%% supervisor.

%% {<0.98.0>,175} sent.
%% 175 echoed.
%% Supervisor trapped exit for echo process.
%% =ERROR REPORT==== 13-Jul-2020::08:09:44.214000 ===
%% Error in process <0.98.0> with exit value:
%% {badarg,[{wk2_talk,work,1,[{file,"wk2_talk.erl"},{line,9}]}]}

%% echo re-spawned as Pid <0.1505.0>.
%% Supervisor trapped exit for talk process.
%% talk re-spawned as Pid <0.1506.0>.
%% {<0.1506.0>,0} sent.
%% 0 echoed.
%% ....

%% iv) kill the talk process from observer - this does not take down the echo process, as
%% expected, and the supervisor starts the talk process again :

%% {<0.1506.0>,941} sent.
%% 941 echoed.
%% Supervisor trapped exit for talk process.
%% talk re-spawned as Pid <0.15991.0>.
%% {<0.15991.0>,0} sent.

%% -------------

%% With the above line of code commented out, and killing the 
%% echo server in the same way, the supervisor is able to re-spawn
%% the echo server in time to service the talk server's message send, 
%% so there is no apparant delay or error observed in the system :

%% {<0.105.0>,74} sent.
%% 74 echoed.
%% Supervisor trapped exit for echo process.
%% echo re-spawned as Pid <0.2497.0>.
%% {<0.105.0>,75} sent.
%% 75 echoed.
%% {<0.105.0>,76} sent.

%% ====================== END OF COMMENTARY ===========================

super() ->
	%% this process will trap exits for any 'child' processes it links to
    process_flag(trap_exit, true),
    E = spawn_link(wk2_echo,listener,[]),
    register(echo,E),
    io:format("echo spawned as Pid ~w.~n", [whereis(echo)]),
    T = spawn_link(wk2_talk,worker,[]),
    register(talk,T),
    io:format("worked spawned as Pid ~w.~n",[whereis(talk)]),
	%% start the listen loop for receiving messages from the child processes.
    loop(E,T).

loop(E,T) ->
     receive
        {'EXIT', T, _} -> 
			io:format("Supervisor trapped exit for talk process.~n", []),
            NewT = spawn_link(wk2_talk,worker,[]),
            register(talk,NewT),
            io:format("talk re-spawned as Pid ~w.~n",[whereis(talk)]),
            loop(E,NewT);
         {'EXIT', E, _} -> 
			io:format("Supervisor trapped exit for echo process.~n", []),
            %% timer:sleep(1000), 
            NewE = spawn_link(wk2_echo,listener,[]),
            register(echo,NewE),
            io:format("echo re-spawned as Pid ~w.~n",[whereis(echo)]),
            loop(NewE,T)
    end.    