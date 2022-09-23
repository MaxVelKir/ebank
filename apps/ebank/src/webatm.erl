-module(webatm).

-export([
    cancel/1,
    clear/2,
    show_balance/2,
    show_initial_text/2,
    show_input/2,
    show_mini_statement/2,
    show_pin_valid_message/3,
    show_pin_invalid_message/1,
    show_pin_request/2,
    show_successful_withdraw/1,
    show_timeout/1,
    show_unsuccessful_withdraw/2,
    show_withdraw_message/2
]).

-type atm_id() :: atom().

-spec cancel(atm_id()) -> term().
cancel(AtmId) ->
    broadcast(
        {"Hello! <br/>Welcome to the Erlang ATM. <br/>Please insert your card.", idle}, AtmId
    ).

clear(AtmId, get_pin) ->
    broadcast("", AtmId);
clear(AtmId, withdraw) ->
    show_withdraw_message(AtmId, none);
clear(_AtmId, _State) ->
    ok.

show_balance(AtmId, Balance) ->
    broadcast("Your account balance is " ++ io_lib:format("~.2f", [Balance]), AtmId).

show_mini_statement(AtmId, AccNo) ->
    broadcast({mini_statement, AccNo}, AtmId).

-spec show_pin_valid_message(any(), [any()], any()) -> ok.
show_pin_valid_message(AtmId, Name, State) ->
    broadcast({"Welcome " ++ Name ++ ". Please choose an option.", State}, AtmId).

show_pin_invalid_message(AtmId) ->
    broadcast("The introduced pin is not valid.", AtmId).

show_input(AtmId, Digits) ->
    Input = lists:foldr(
        fun(Digit, Acc) ->
            Acc ++ integer_to_list(Digit)
        end,
        "",
        Digits
    ),
    broadcast(Input, AtmId).

show_withdraw_message(AtmId, none) ->
    broadcast("Please introduce amount to withdraw:", AtmId);
show_withdraw_message(AtmId, Integer) ->
    broadcast("Please introduce amount to withdraw:" ++ integer_to_list(Integer), AtmId).

show_successful_withdraw(AtmId) ->
    broadcast("Money withdrawn successfully", AtmId).

show_unsuccessful_withdraw(AtmId, Reason) ->
    broadcast(Reason, AtmId).

show_pin_request(AtmId, State) ->
    broadcast({"Please introduce the pin of your card.", State}, AtmId).

show_initial_text(AtmId, State) ->
    broadcast(
        {"Hello! <br/>Welcome to the Erlang ATM. <br/>Please insert your card.", State}, AtmId
    ).

show_timeout(_AtmId) ->
    ok.

broadcast(Message, Topic) ->
    io:format("Broadcasted message: ~p on topic: ~p", [Message, Topic]).
