-module(wk1_palindrome_server).
-export([palin_server/1]).

% Simple palindrome checking server function that takes a PID to whose mailbox the result of 
% the palindrome check is sent to.

% To run a palindrome check, the server expects messages of the form {check, <palindrome>} 
% (eg, {check,"Madam I\'m Adam"}) and returns a result of the form "{result,"\"Madam I\'m Adam\" is a palindrome"}" 
% to the mailbox of the PID it was started with, indicating if the supplied string was a palindrome or not.
% The function also takes a message of the form "stop" which will cause it to terminate.

% The palindrome function is inlined for simplicity.

palin_server(Pid) ->
    receive
		stop 		   -> 
			io:format("stopped~n");
        {check,String} ->
            case palindrome(String) of
                 true ->
                     Pid ! {result, "\"" ++ String ++ "\" is a palindrome."},
                     palin_server(Pid);
                 _ ->
                     Pid ! {result, "\"" ++ String ++ "\" is not a palindrome."},
                     palin_server(Pid)
            end;
        _Message ->
            ok
    end.



palindrome(Xs) ->
    palin(nocaps(nopunct(Xs))).

nopunct([]) ->
    [];
nopunct([X|Xs]) ->
    case lists:member(X,".,\ ;:\t\n\'\"") of
	true ->
	    nopunct(Xs);
	false ->
	    [ X | nopunct(Xs) ]
    end.

nocaps([]) ->
    [];
nocaps([X|Xs]) ->
    [ nocap(X) | nocaps(Xs) ].

nocap(X) ->
    case $A =< X andalso X =< $Z of
	true ->
	    X+32;
	false ->
	    X
    end.

% literal palindrome

palin(Xs) ->
    Xs == reverse(Xs).

reverse(Xs) ->
    shunt(Xs,[]).

shunt([],Ys) ->
    Ys;
shunt([X|Xs],Ys) ->
    shunt(Xs,[X|Ys]).
