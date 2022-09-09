-module(backend_db).

-include_lib("../include/backend.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-export([
    all_accounts/1,
    create_db/0,
    close/1,
    credit/3,
    debit/3,
    is_pin_valid/3,
    lookup/2,
    lookup_by_name/2,
    new_account/4
]).

-spec create_db() -> dbref().
create_db() -> ets:new(account, [set, {keypos, #account.acc_no}]).

-spec lookup(account_number(), dbref()) -> #account{} | {error, instance}.
lookup(AccNo, Dbref) ->
    MatchSpec = ets:fun2ms(fun(A = #account{acc_no = AccNoInEts}) when AccNoInEts == AccNo -> A end),
    case ets:select(Dbref, MatchSpec) of
        [A = #account{}] -> A;
        _ -> {error, instance}
    end.

-spec lookup_by_name(name(), dbref()) -> [#account{}].
lookup_by_name(Name, Dbref) ->
    MatchSpec = ets:fun2ms(fun(A = #account{name = NameInEts}) when NameInEts == Name -> A end),
    ets:select(Dbref, MatchSpec).

-spec new_account(account_number(), pin(), name(), dbref()) -> ok | {error, instance}.
new_account(AccNo, Pin, Name, Dbref) ->
    Account = #account{acc_no = AccNo, pin = Pin, name = Name},
    ok_or_error(
        ets:insert_new(Dbref, Account),
        {error, exists}
    ).

-spec credit(account_number(), amount(), dbref()) -> ok | {error, instance}.
credit(AccNo, Amount, Dbref) ->
    Acc = lookup(AccNo, Dbref),
    do_credit(Acc, Amount, Dbref).

-spec do_credit(#account{} | operation_error(), amount(), dbref()) -> ok | operation_error().
do_credit(#account{acc_no = AccNo, balance = Balance, transactions = Trs}, Amount, Dbref) ->
    NewBalance = Balance + Amount,
    make_transaction(AccNo, credit, Amount, NewBalance, Trs, Dbref);
do_credit(Error = {error, _}, _Amount, _Dbref) ->
    Error.

-spec debit(account_number(), amount(), dbref()) -> ok | operation_error().
debit(AccNo, Amount, Dbref) ->
    Acc = lookup(AccNo, Dbref),
    do_debit(Acc, Amount, Dbref).

-spec do_debit(#account{} | operation_error(), amount(), dbref()) -> ok | operation_error().
do_debit(#account{balance = Balance}, Amount, _Dbref) when Amount > Balance ->
    {error, "Not enough money on account!"};
do_debit(#account{acc_no = AccNo, balance = Balance, transactions = Trs}, Amount, Dbref) ->
    NewBalance = Balance - Amount,
    make_transaction(AccNo, debit, Amount, NewBalance, Trs, Dbref);
do_debit(Error = {error, _}, _Amount, _Dbref) ->
    Error.

-spec make_transaction(account_number(), type(), amount(), balance(), transactions(), dbref()) ->
    ok | operation_error().
make_transaction(AccNo, Type, Amount, NewBalance, Trs, Dbref) ->
    NewTrs = [#transaction{type = Type, amount = Amount} | Trs],
    MatchSpec = ets:fun2ms(
        fun(A = #account{acc_no = AccNoInEts}) when AccNoInEts == AccNo ->
            A#account{balance = NewBalance, transactions = NewTrs}
        end
    ),
    maybe_ok(
        ets:select_replace(Dbref, MatchSpec)
    ).

-spec is_pin_valid(account_number(), pin(), dbref()) -> boolean().
is_pin_valid(AccNo, Pin, Dbref) ->
    case lookup(AccNo, Dbref) of
        #account{pin = PinInEts} -> PinInEts == Pin;
        Error -> Error
    end.

-spec all_accounts(dbref()) -> [#account{}].
all_accounts(Dbref) ->
    ets:tab2list(Dbref).

-spec close(dbref()) -> ok.
close(Dbref) ->
    ets:delete(Dbref),
    ok.

-spec maybe_ok(non_neg_integer()) -> ok | operation_error().
maybe_ok(0) -> {error, unknown};
maybe_ok(_) -> ok.

-spec ok_or_error(boolean(), operation_error()) -> ok | operation_error().

ok_or_error(true, _Error) -> ok;
ok_or_error(_, Error) -> Error.
