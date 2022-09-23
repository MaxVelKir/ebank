-module(atm).

-behaviour(gen_statem).

-include_lib("../include/backend.hrl").

-record(atm_data, {
    acc_no :: account_number(),
    digits = [] :: [digit()],
    pin :: pin() | undefined,
    acc_name :: acc_name(),
    atm_id :: atom()
}).

-ifdef(TEST).
-export([build_integer/1]).
-define(TIMEOUT, 500).
-else.
-define(TIMEOUT, 60000).
-endif.

-define(TIMEOUT_ACTION, {{timeout, idle}, ?TIMEOUT, idle}).

%% Client

-export([
    start_link/1,
    event/2,
    card_inserted/3
]).

%% States

-export([
    idle/3,
    get_pin/3,
    selection/3,
    withdraw/3
]).

%% Server (callbacks)

-export([
    init/1,
    callback_mode/0,
    stop/1,
    terminate/3
]).

-type selection() :: withdraw | balance | statement.
-type button() :: clear | stop | cancel | enter.
-type atm_state() :: idle | get_pin | selection | withdraw | none.

-type timeout_action() :: {{timeout, idle}, non_neg_integer(), idle}.

-type event() ::
    {button, button()}
    | {card_inserted, account_number(), acc_name()}
    | {selection, selection()}
    | {digit | digit()}.

-type state_action() ::
    timeout_action()
    | {reply, pid(), reply()}
    | timeout_action().

-type state_actions() :: [state_action()].

-type transition() ::
    {next_state, atm_state(), #atm_data{}}
    | {next_state, atm_state(), #atm_data{}, state_actions()}.

-type reply() :: pin() | balance() | amount() | ok | [#transaction{}].

% Client

-spec start_link(atom()) -> {ok, pid()} | {error, Reason :: term()}.
start_link(AtmId) ->
    gen_statem:start_link({local, AtmId}, ?MODULE, AtmId, []).

-spec event(atom(), event()) -> reply().
event(AtmId, Event = {card_inserted, _AccNo, _AccName}) ->
    gen_statem:cast(AtmId, Event);
event(AtmId, Event) ->
    gen_statem:call(AtmId, Event).

-spec card_inserted(atom(), account_number(), binary()) -> reply().
card_inserted(AtmId, AccNo, AccName) ->
    event(AtmId, {card_inserted, AccNo, AccName}).

-spec stop(atom()) -> ok.
stop(AtmId) ->
    gen_statem:stop(AtmId).

%% States / Transitions

-spec idle(atom() | {atom(), pid()}, event(), #atm_data{}) -> transition().
idle(cast, {card_inserted, AccNo, AccName}, Data = #atm_data{atm_id = AtmId}) ->
    NewData = Data#atm_data{acc_no = AccNo, acc_name = AccName},
    webatm:show_pin_request(AtmId, get_pin),
    {next_state, get_pin, NewData, [?TIMEOUT_ACTION]};
idle(C, E, D) ->
    handle_common(C, E, D, idle).

-spec get_pin(atom() | {atom(), pid()}, event(), #atm_data{}) -> transition().
get_pin({call, From}, {digit, _Digit}, #atm_data{digits = Digits}) when
    length(Digits) >= 4
->
    {keep_state_and_data, [{reply, From, build_integer(Digits)}, ?TIMEOUT_ACTION]};
get_pin({call, From}, {digit, Digit}, Data = #atm_data{atm_id = AtmId, digits = Digits}) ->
    NewDigits = [Digit | Digits],
    NewData = Data#atm_data{digits = NewDigits},
    webatm:show_input(AtmId, NewDigits),
    {next_state, get_pin, NewData, [{reply, From, build_integer(NewDigits)}, ?TIMEOUT_ACTION]};
get_pin(
    {call, From},
    {button, enter},
    Data = #atm_data{atm_id = AtmId, digits = Digits, acc_no = AccNo, acc_name = _AccName}
) ->
    Pin = build_integer(Digits),
    case backend:pin_valid(AccNo, Pin) of
        true ->
            NewData = Data#atm_data{pin = Pin, digits = []},
            % webatm:show_pin_valid_message(AtmId, AccName, selection),
            {next_state, selection, NewData, [{reply, From, ok}, ?TIMEOUT_ACTION]};
        false ->
            webatm:show_pin_invalid_message(AtmId),
            {keep_state_and_data, [{reply, From, {error, wrong_pin}}, ?TIMEOUT_ACTION]}
    end;
get_pin(C, E, D) ->
    handle_common(C, E, D, get_pin).

-spec selection(atom() | {atom(), pid()}, event(), #atm_data{}) -> transition().
selection(
    {call, From}, {selection, balance}, Data = #atm_data{atm_id = AtmId, acc_no = AccNo, pin = Pin}
) ->
    Balance = backend:balance(AccNo, Pin),
    webatm:show_balance(AtmId, Balance),
    {keep_state, Data, [{reply, From, Balance}, ?TIMEOUT_ACTION]};
selection(
    {call, From},
    {selection, statement},
    Data = #atm_data{atm_id = AtmId, acc_no = AccNo, pin = Pin}
) ->
    Transactions = backend:transactions(AccNo, Pin),
    webatm:show_mini_statement(AtmId, AccNo),
    {keep_state, Data, [{reply, From, Transactions}, ?TIMEOUT_ACTION]};
selection({call, From}, {selection, withdraw}, Data = #atm_data{atm_id = AtmId}) ->
    NewData = Data#atm_data{digits = []},
    webatm:show_withdraw_message(AtmId, none),
    {next_state, withdraw, NewData, [{reply, From, ok}, ?TIMEOUT_ACTION]};
selection(C, E, D) ->
    handle_common(C, E, D, selection).

-spec withdraw(atom() | {atom(), pid()}, event(), #atm_data{}) -> transition().
withdraw({call, From}, {digit, Digit}, Data = #atm_data{atm_id = AtmId, digits = Digits}) ->
    NewDigits = [Digit | Digits],
    NewData = Data#atm_data{digits = NewDigits},
    Integer = build_integer(NewDigits),
    webatm:show_withdraw_message(AtmId, Integer),
    {keep_state, NewData, [{reply, From, Integer}, ?TIMEOUT_ACTION]};
withdraw(
    {call, From},
    {button, enter},
    Data = #atm_data{atm_id = AtmId, digits = Digits, acc_no = AccNo, pin = Pin}
) ->
    Result =
        case backend:withdraw(AccNo, Pin, build_integer(Digits)) of
            {error, Reason} ->
                webatm:show_unsuccessful_withdraw(AtmId, Reason),
                error;
            Res ->
                webatm:show_successful_withdraw(AtmId),
                Res
        end,
    {next_state, selection, Data, [{reply, From, Result}, ?TIMEOUT_ACTION]};
withdraw(C, E, D) ->
    handle_common(C, E, D, withdraw).

% Server (callbacks)
init(AtmId) ->
    {ok, idle, #atm_data{atm_id = AtmId}}.

callback_mode() ->
    state_functions.

terminate(_Reason, _State, _Data) ->
    ok.

%% Private

%%% Common state handling
handle_common({call, From}, {button, clear}, Data = #atm_data{atm_id = AtmId}, State) ->
    webatm:clear(AtmId, State),
    {keep_state, clear_digits(Data), [{reply, From, ok}, ?TIMEOUT_ACTION]};
handle_common({call, From}, {button, cancel}, Data = #atm_data{atm_id = AtmId}, _State) ->
    webatm:cancel(AtmId),
    {next_state, idle, clear_digits(Data), [{reply, From, ok}, ?TIMEOUT_ACTION]};
handle_common(cast, _, Data = #atm_data{}, _State) ->
    {keep_state, Data, [?TIMEOUT_ACTION]};
handle_common({call, From}, _, #atm_data{}, _State) ->
    {keep_state_and_data, [{reply, From, ok}, ?TIMEOUT_ACTION]};
handle_common({timeout, idle}, NextState, Data = #atm_data{atm_id = AtmId}, _State) ->
    webatm:show_initial_text(AtmId, NextState),
    {next_state, NextState, Data}.

build_integer([]) ->
    -1;
build_integer([_H | _T] = Digits) ->
    list_to_integer(lists:concat(lists:reverse(Digits))).

clear_digits(Data = #atm_data{}) -> Data#atm_data{digits = []}.
