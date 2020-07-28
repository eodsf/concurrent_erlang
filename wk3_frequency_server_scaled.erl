%% Based on code from 
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

%% This is a mod of the frequency server, designed to sit behind 
%% a router, and able to serve a specific set of frequencies designated
%% by the router.
-module(wk3_frequency_server_scaled).
-export([start/1, allocate/1, deallocate/2, stop/1]).
-export([init/1]).

%% These are the start functions used to create and
%% initialize the server.

start(Frequencies) ->
    spawn(?MODULE, init, [Frequencies]).

init(Frequencies) ->
    Frequencies = {Frequencies, []},
    loop(Frequencies).

%% The Main Loop

loop(Frequencies) ->
    receive
        {request, Pid, allocate} ->
            {NewFrequencies, Reply} = handle_allocate(Frequencies, Pid),
            Pid ! {reply, Reply},
            loop(NewFrequencies);
        {request, Pid , {deallocate, Freq}} ->
            NewFrequencies = handle_deallocate(Frequencies, Freq),
            Pid ! {reply, ok},
            loop(NewFrequencies);
        {request, Pid, stop} ->
            Pid ! {reply, stopped}
    end.

%% Functional interface

allocate(Pid) -> 
    Pid ! {request, self(), allocate},
    receive 
        {reply, Reply} -> Reply
    end.

deallocate(Pid, Freq) -> 
    Pid ! {request, self(), {deallocate, Freq}},
    receive 
        {reply, Reply} -> Reply
    end.

stop(Pid) -> 
    Pid ! {request, self(), stop},
    receive 
        {reply, Reply} -> Reply
    end.

%% The Internal Help Functions used to allocate and
%% deallocate frequencies.

handle_allocate({[], Allocated}, _Pid) ->
    {{[], Allocated}, {error, no_frequency}};
handle_allocate({[Freq|Frequencies], Allocated}, Pid) ->
    {{Frequencies, [{Freq, Pid}|Allocated]}, {ok, Freq}}.

handle_deallocate({Frequencies, Allocated}, Freq) ->
    NewAllocated = lists:keydelete(Freq, 1, Allocated),
    {[Freq|Frequencies],  NewAllocated}.