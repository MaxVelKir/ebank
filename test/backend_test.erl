-module(backend_test).
-include_lib("eunit/include/eunit.hrl").

happy_path_test() ->
    application:start(sasl),
    {ok, _} = helpers:ensure_started(event_manager),
    {ok, _} = helpers:ensure_started(backend),
    {account, 1, 123045.0, false, "Henry Nystrom", 1234, _trs} = backend:account(1),

    {error, no_account} = backend:account(7),

    [
        {account, 1, 123045.0, false, "Henry Nystrom", 1234, [
            {transaction, credit, _, 110.0},
            {transaction, debit, _, 120.0},
            {transaction, credit, _, 180.0},
            {transaction, credit, _, 1.5e3},
            {transaction, debit, _, 3150.0},
            {transaction, credit, _, 1525.0},
            {transaction, credit, _, 1.23e5}
        ]},
        {account, 4, 5.0e3, false, "Henry Nystrom", 1234, [
            {transaction, credit, _, 5.0e3}
        ]}
    ] = backend:accounts_by_name("Henry Nystrom"),

    [
        {account, 1, 123045.0, false, "Henry Nystrom", 1234, [
            {transaction, credit, _, 110.0},
            {transaction, debit, _, 120.0},
            {transaction, credit, _, 180.0},
            {transaction, credit, _, 1.5e3},
            {transaction, debit, _, 3150.0},
            {transaction, credit, _, 1525.0},
            {transaction, credit, _, 1.23e5}
        ]},
        {account, 2, 200.0, false, "Martin Gausby", 4321, [
            {transaction, credit, _, 200.0}
        ]},
        {account, 4, 5.0e3, false, "Henry Nystrom", 1234, [
            {transaction, credit, _, 5.0e3}
        ]},
        {account, 3, 1.0e3, false, "Gabor Olah", 1111, [
            {transaction, credit, _, 1.0e3}
        ]}
    ] =
        backend:list_accounts(),
    true = backend:pin_valid(1, 1234),

    false = backend:pin_valid(1, 1111),

    ok = backend:withdraw(1, 1234, 10),

    {error, balance} = backend:withdraw(1, 1234, 200000),

    ok = backend:deposit(1, 100),
    ok = backend:transfer(1, 4, 100, 1234),
    123035.0 = backend:balance(1, 1234),

    [
        {transaction, debit, _, 100},
        {transaction, credit, _, 100},
        {transaction, debit, _, 10},
        {transaction, credit, _, 110.0},
        {transaction, debit, _, 120.0},
        {transaction, credit, _, 180.0},
        {transaction, credit, _, 1.5e3},
        {transaction, debit, _, 3150.0},
        {transaction, credit, _, 1525.0},
        {transaction, credit, _, 1.23e5}
    ] = backend:transactions(1, 1234),

    ok = backend:stop().
