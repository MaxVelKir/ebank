%%%-------------------------------------------------------------------
%%% @author Josep Giralt D'Lacoste <pep.g.dlacoste@erlang-solutions.com>
%%% @doc
%%% == Common tests ==
%%% This module tests the correct behaviour of the atm among its
%%% different states and transitions.
%%%
%%% There is an overuse of the common test capabilities, so if you
%%% think that grouping tests here is not required, you are
%%% right ;). This has been done only for learning purposes.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(atm_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([
    groups/0,
    all/0,
    init_per_testcase/2,
    end_per_testcase/2,
    init_per_group/2,
    end_per_group/2
]).

-export([
    idle_test/1,
    get_pin_test/1,
    selection_test/1,
    withdraw_test/1,
    cancel_test/1,
    timeout_test/1
]).

-define(GROUP_OPTS, [shuffle, sequence]).

groups() ->
    [
        {stateful, ?GROUP_OPTS, [
            idle_test,
            get_pin_test,
            selection_test,
            withdraw_test,
            cancel_test,
            timeout_test
        ]}
    ].

all() -> [{group, stateful}].

init_per_group(_Type, Config) ->
    {ok, _} = helpers:ensure_started(backend),
    Config.

end_per_group(_Group, _Config) ->
    ok.

init_per_testcase(Test, Config) ->
    {ok, _} = helpers:ensure_started(backend),
    start_atm(Test, Config).

end_per_testcase(_, Config) ->
    ok = backend:reboot(),
    stop(Config).

start_atm(Atm, Config) ->
    {ok, Pid} = atm:start_link(Atm),
    [{atm, Atm}, {atm_pid, Pid} | Config].

stop(Config) ->
    Pid = ?config(atm_pid, Config),
    true = unlink(Pid),
    ok = atm:stop(?config(atm, Config)),
    false = is_process_alive(Pid).

idle_test(Config) ->
    Atm = ?config(atm, Config),
    Pid = ?config(atm_pid, Config),
    ok = atm:event(Atm, {button, clear}),
    ok = atm:card_inserted(Atm, 1, "Name"),
    true = is_process_alive(Pid),
    ok = atm:event(Atm, {button, cancel}).

get_pin_test(Config) ->
    Atm = ?config(atm, Config),
    ok = atm:card_inserted(Atm, 1, "Name"),
    4 = atm:event(Atm, {digit, 4}),
    42 = atm:event(Atm, {digit, 2}),
    423 = atm:event(Atm, {digit, 3}),
    4231 = atm:event(Atm, {digit, 1}),
    % if additional digits are pressed they should be ignored
    4231 = atm:event(Atm, {digit, 1}),
    {error, wrong_pin} = atm:event(Atm, {button, enter}),
    % % pin is wrong because the right one for `acc_no` 1 is 1234
    ok = atm:event(Atm, {button, clear}),
    1 = atm:event(Atm, {digit, 1}),
    12 = atm:event(Atm, {digit, 2}),
    123 = atm:event(Atm, {digit, 3}),
    1234 = atm:event(Atm, {digit, 4}),
    % right pin goes to selection state
    ok = atm:event(Atm, {button, enter}).

selection_test(Config) ->
    Atm = ?config(atm, Config),
    ok = atm:card_inserted(Atm, 1, "Name"),
    1 = atm:event(Atm, {digit, 1}),
    12 = atm:event(Atm, {digit, 2}),
    123 = atm:event(Atm, {digit, 3}),
    1234 = atm:event(Atm, {digit, 4}),
    ok = atm:event(Atm, {button, enter}),
    123045.00 = atm:event(Atm, {selection, balance}),
    [{transaction, credit, _, 110.0} | _] = atm:event(Atm, {selection, statement}).

withdraw_test(Config) ->
    Atm = ?config(atm, Config),
    ok = atm:card_inserted(Atm, 1, "Name"),
    1 = atm:event(Atm, {digit, 1}),
    12 = atm:event(Atm, {digit, 2}),
    123 = atm:event(Atm, {digit, 3}),
    1234 = atm:event(Atm, {digit, 4}),
    ok = atm:event(Atm, {button, enter}),
    ok = atm:event(Atm, {selection, withdraw}),
    3 = atm:event(Atm, {digit, 3}),
    34 = atm:event(Atm, {digit, 4}),
    ok = atm:event(Atm, {button, clear}),
    3 = atm:event(Atm, {digit, 3}),
    33 = atm:event(Atm, {digit, 3}),
    ok = atm:event(Atm, {button, enter}),
    123012.0 = backend:balance(1, 1234).

cancel_test(Config) ->
    Atm = ?config(atm, Config),
    Pid = ?config(atm_pid, Config),
    ok = atm:card_inserted(Atm, 1, "Name"),
    {get_pin, _} = sys:get_state(Pid),
    ok = atm:event(Atm, {button, cancel}),
    {idle, _} = sys:get_state(Pid),

    ok = atm:card_inserted(Atm, 1, "Name"),
    {get_pin, _} = sys:get_state(Pid),
    1 = atm:event(Atm, {digit, 1}),
    12 = atm:event(Atm, {digit, 2}),
    123 = atm:event(Atm, {digit, 3}),
    1234 = atm:event(Atm, {digit, 4}),
    ok = atm:event(Atm, {button, enter}),
    {selection, _} = sys:get_state(Pid),
    ok = atm:event(Atm, {button, cancel}),
    {idle, _} = sys:get_state(Pid),

    ok = atm:card_inserted(Atm, 1, "Name"),
    {get_pin, _} = sys:get_state(Pid),
    1 = atm:event(Atm, {digit, 1}),
    12 = atm:event(Atm, {digit, 2}),
    123 = atm:event(Atm, {digit, 3}),
    1234 = atm:event(Atm, {digit, 4}),
    ok = atm:event(Atm, {button, enter}),
    ok = atm:event(Atm, {selection, withdraw}),
    {withdraw, _} = sys:get_state(Pid),
    1 = atm:event(Atm, {digit, 1}),
    ok = atm:event(Atm, {button, enter}),
    {selection, _} = sys:get_state(Pid),
    ok = atm:event(Atm, {button, cancel}),
    {idle, _} = sys:get_state(Pid).

timeout_test(Config) ->
    Atm = ?config(atm, Config),
    Pid = ?config(atm_pid, Config),
    ok = atm:card_inserted(Atm, 1, "Name"),
    {get_pin, _} = sys:get_state(Pid),
    timer:sleep(600),
    {idle, _} = sys:get_state(Pid),

    ok = atm:card_inserted(Atm, 1, "Name"),
    {get_pin, _} = sys:get_state(Pid),
    1 = atm:event(Atm, {digit, 1}),
    12 = atm:event(Atm, {digit, 2}),
    123 = atm:event(Atm, {digit, 3}),
    1234 = atm:event(Atm, {digit, 4}),
    ok = atm:event(Atm, {button, enter}),
    {selection, _} = sys:get_state(Pid),
    timer:sleep(600),
    {idle, _} = sys:get_state(Pid),

    ok = atm:card_inserted(Atm, 1, "Name"),
    {get_pin, _} = sys:get_state(Pid),
    1 = atm:event(Atm, {digit, 1}),
    12 = atm:event(Atm, {digit, 2}),
    123 = atm:event(Atm, {digit, 3}),
    1234 = atm:event(Atm, {digit, 4}),
    ok = atm:event(Atm, {button, enter}),
    ok = atm:event(Atm, {selection, withdraw}),
    1 = atm:event(Atm, {digit, 1}),
    ok = atm:event(Atm, {button, enter}),
    {selection, _} = sys:get_state(Pid),
    timer:sleep(600),
    {idle, _} = sys:get_state(Pid).

build_integer_test() ->
    ?assert(-1 =:= atm:build_integer([])),
    ?assert(4 =:= atm:build_integer([4])),
    ?assert(14 =:= atm:build_integer([4, 1])),
    ?assert(324 =:= atm:build_integer([4, 2, 3])).
