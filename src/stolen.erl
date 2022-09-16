-module(stolen).

-behaviour(gen_event).

-export([
    init/1,
    handle_call/2,
    handle_event/2,
    terminate/2
]).

init(_) ->
    TabRef = ets:new(suspicious, [set]),
    {ok, TabRef}.

handle_call(Request, TabRef) ->
    io:format("Received request: ~p~n", [Request]),
    {ok, ok, TabRef}.

handle_event({invalid_pin, AccNo}, TabRef) ->
    Result = ets:update_counter(TabRef, AccNo, 1, {0, 0}),
    handle_invalid_pin(Result, AccNo),
    {ok, TabRef};
handle_event({Event, AccNo}, TabRef) when Event == valid_pin; Event == account_unblocked ->
    true = ets:delete(TabRef, AccNo),
    {ok, TabRef};
handle_event(_IgnoreEvent, TabRef) ->
    {ok, TabRef}.

handle_invalid_pin(Attempts, AccNo) when Attempts >= 3 ->
    backend:block(AccNo),
    Attempts;
handle_invalid_pin(Attempts, _AccNo) ->
    Attempts.

terminate(_Args, Dbref) ->
    ets:delete(Dbref),
    ok.
