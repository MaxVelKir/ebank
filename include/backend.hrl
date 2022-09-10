-record(transaction, {
    type :: type(),
    time = calendar:local_time(),
    amount :: amount()
}).

-type account_number() :: non_neg_integer().
-type amount() :: non_neg_integer().
-type balance() :: non_neg_integer().
-type blocked() :: boolean().
-type dbref() :: atom() | ets:id().
-type name() :: binary().
-type operation_error() :: {error, Reason :: term()}.
-type pin() :: non_neg_integer().
-type transactions() :: [#transaction{}].
-type type() :: debit | credit.

-record(account, {
    acc_no :: account_number(),
    balance = 0 :: balance(),
    blocked = false :: blocked(),
    name :: name(),
    pin :: pin(),
    transactions = [] :: transactions()
}).

-export_type([
    account_number/0,
    amount/0,
    balance/0,
    blocked/0,
    dbref/0,
    name/0,
    operation_error/0,
    pin/0,
    type/0
]).
