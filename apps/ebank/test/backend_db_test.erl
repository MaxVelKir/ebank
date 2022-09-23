-module(backend_db_test).
-include_lib("eunit/include/eunit.hrl").

happy_path_test() ->
    {ok, _} = helpers:ensure_started(event_manager),
    DB = backend_db:create_db(),
    ok = backend_db:new_account(1, "1234", "Donald Duck", DB),
    {account, 1, 0, false, "Donald Duck", "1234", []} = backend_db:lookup(1, DB),

    ok = backend_db:credit(1, 100, DB),

    {account, 1, 100, false, "Donald Duck", "1234", [{transaction, credit, _TrTime, 100}]} =
        backend_db:lookup(
            1, DB
        ),

    ok = backend_db:credit(1, 100, DB),

    {account, 1, 200, false, "Donald Duck", "1234", [
        {transaction, credit, _TrTime1, 100},
        {transaction, credit, _TrTime2, 100}
    ]} = backend_db:lookup(1, DB),

    ok = backend_db:debit(1, 50, DB),

    {account, 1, 150, false, "Donald Duck", "1234", [
        {transaction, debit, _TrTime3, 50},
        {transaction, credit, _TrTime4, 100},
        {transaction, credit, _TrTime5, 100}
    ]} = backend_db:lookup(1, DB),

    {error, "Not enough money on account!"} = backend_db:debit(1, 200, DB),
    true = backend_db:is_pin_valid(1, "1234", DB),
    false = backend_db:is_pin_valid(1, "1111", DB),
    ok = backend_db:close(DB).

lookup_error_test() ->
    DB = backend_db:create_db(),
    Acc = backend_db:lookup(1, DB),
    ?assert(Acc =:= {error, instance}).

lookup_by_name_error_test() ->
    DB = backend_db:create_db(),
    Acc = backend_db:lookup_by_name("asd", DB),
    ?assert(Acc =:= []).

new_account_error_test() ->
    DB = backend_db:create_db(),
    ok = backend_db:new_account(1, "1234", "Donald Duck", DB),
    {error, exists} = backend_db:new_account(1, "1234", "Donald Duck", DB).

more_than_one_account_test() ->
    DB = backend_db:create_db(),
    ok = backend_db:new_account(1, "1234", "Donald Duck", DB),
    ok = backend_db:new_account(2, "1234", "Donald Duck", DB).

credit_error_test() ->
    DB = backend_db:create_db(),
    Acc = backend_db:credit(1, 100, DB),
    ?assert(Acc =:= {error, instance}).

debit_error_test() ->
    DB = backend_db:create_db(),
    Acc = backend_db:debit(1, 100, DB),
    ?assert(Acc =:= {error, instance}).
