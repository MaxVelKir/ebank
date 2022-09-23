-module(statistics).

-behaviour(gen_event).

-export([
    init/1,
    handle_event/2,
    handle_call/2,
    terminate/2
]).

-record(statistics, {
    total_transfer_amount :: non_neg_integer(),
    account_blocked :: non_neg_integer(),
    transfer :: non_neg_integer(),
    withdraw :: non_neg_integer(),
    deposit :: non_neg_integer(),
    avg_transfer_money :: non_neg_integer()
}).

% Server

init(_) ->
    TabRef = ets:new(statistics, [set]),
    {ok, TabRef}.

handle_event({Event, _A}, TabRef) when
    Event == withdraw; Event == deposit; Event == account_blocked; Event == account_unblocked
->
    increase_counter(Event, TabRef),
    {ok, TabRef};
handle_event({transfer, Amount}, TabRef) ->
    increase_counter(transfer, TabRef),
    ets:update_counter(TabRef, total_transfer_amount, Amount, {total_transfer_amount, 0}),
    {ok, TabRef};
handle_event(_, TabRef) ->
    {ok, TabRef}.

handle_call(get_stats, TabRef) ->
    {ok, build_stats(TabRef), TabRef}.

terminate(_Args, Dbref) ->
    ets:delete(Dbref),
    ok.

% Private
-spec build_stats(ets:id()) -> #statistics{}.
build_stats(TabRef) ->
    Stats = maps:from_list(ets:tab2list(TabRef)),
    TotalTransferAmount = maps:get(total_transfer_amount, Stats, 0),
    TransfersCount = maps:get(transfer, Stats, 0),
    #statistics{
        total_transfer_amount = TotalTransferAmount,
        account_blocked = maps:get(account_blocked, Stats, 0),
        transfer = TransfersCount,
        withdraw = maps:get(withdraw, Stats, 0),
        deposit = maps:get(deposit, Stats, 0),
        avg_transfer_money = avg(TotalTransferAmount, TransfersCount)
    }.

increase_counter(Event, TabRef) ->
    ets:update_counter(TabRef, Event, 1, {Event, 0}).

avg(Amount, Count) when Amount == 0; Count == 0 -> 0;
avg(Amount, Count) -> Amount / Count.
