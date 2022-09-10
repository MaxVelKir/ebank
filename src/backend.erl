-module(backend).

-behaviour(gen_server).

-include_lib("../include/backend.hrl").

-record(backend_state, {dbref :: dbref()}).

-define(GID, {global, ?MODULE}).
-define(GCALL, ?GID).

% Client

-export([
    start_link/0,
    stop/0,
    reboot/0,
    account/1,
    accounts_by_name/1,
    list_accounts/0,
    pin_valid/2,
    withdraw/3,
    deposit/2,
    transfer/4,
    balance/2,
    transactions/2
]).

% Server (callbacks)

-export([init/1, handle_call/3, handle_cast/2, terminate/2]).

-type backend_db_error() :: operation_error().
-type backend_error() :: operation_error().

% Client

-spec start_link() -> {ok, pid()} | {error, Reason :: term()}.
start_link() ->
    case gen_server:start_link(?GID, ?MODULE, [], []) of
        Ok = {ok, _Ref1} ->
            Ok;
        {error, {already_started, Pid1}} ->
            {ok, Pid1}
    end.

-spec stop() -> ok.
stop() ->
    gen_server:stop(?GCALL).

-spec reboot() -> ok.
reboot() ->
    gen_server:call(?GCALL, reboot).

-spec account(account_number()) -> #account{} | {error, Reason :: term()}.
account(AccNo) ->
    Result = gen_server:call(?GCALL, {account, AccNo}),
    maybe_error(Result, {error, no_account}).

-spec accounts_by_name(name()) -> [#account{}].
accounts_by_name(Name) ->
    gen_server:call(?GCALL, {accounts_by_name, Name}).

-spec list_accounts() -> [#account{}].
list_accounts() ->
    gen_server:call(?GCALL, {list_accounts}).

-spec pin_valid(account_number(), pin()) -> boolean().
pin_valid(AccNo, Pin) ->
    gen_server:call(?GCALL, {pin_valid, AccNo, Pin}).

-spec withdraw(account_number(), pin(), amount()) -> ok | {error, Reason :: term()}.
withdraw(AccNo, Pin, Amount) ->
    case pin_valid(AccNo, Pin) of
        true ->
            Result = gen_server:call(?GCALL, {withdraw, AccNo, Amount}),
            maybe_error(Result, {error, balance});
        false ->
            {error, invalid_pin}
    end.

-spec deposit(account_number(), amount()) -> ok | {error, Reason :: term()}.
deposit(AccNo, Amount) ->
    gen_server:cast(?GCALL, {deposit, AccNo, Amount}).

-spec transfer(account_number(), account_number(), amount(), pin()) ->
    ok | {error, Reason :: term()}.
transfer(AccNoFrom, AccNoTo, Amount, Pin) ->
    case withdraw(AccNoFrom, Pin, Amount) of
        ok ->
            deposit(AccNoTo, Amount);
        Error ->
            Error
    end.

-spec balance(account_number(), pin()) -> balance() | {error, Reason :: term()}.
balance(AccNo, Pin) ->
    case pin_valid(AccNo, Pin) of
        true ->
            #account{balance = Balance} = account(AccNo),
            Balance;
        false ->
            {error, invalid_pin}
    end.

-spec transactions(account_number(), pin()) -> transactions() | {error, Reason :: term()}.
transactions(AccNo, Pin) ->
    case pin_valid(AccNo, Pin) of
        true ->
            #account{transactions = Trs} = account(AccNo),
            Trs;
        false ->
            {error, invalid_pin}
    end.

-spec maybe_error(ok | #account{} | backend_db_error(), backend_error()) ->
    ok | #account{} | backend_error().
maybe_error({error, _}, Error) ->
    Error;
maybe_error(Term, _Error) ->
    Term.

% Server (callbacks)

-spec init_accounts(dbref()) -> ok.
init_accounts(Dbref) ->
    {ok, Accounts} = file:consult(code:priv_dir(ebank) ++ "/accounts.txt"),
    lists:foreach(
        fun({AccNo, Balance, Pin, Name, Trs}) ->
            backend_db:new_account(AccNo, list_to_integer(Pin), Name, Dbref),
            backend_db:credit(AccNo, Balance, Dbref),
            lists:foreach(
                fun({Type, Amount}) ->
                    apply(backend_db, Type, [AccNo, Amount, Dbref])
                end,
                Trs
            )
        end,
        Accounts
    ).

-spec init(any()) -> {ok, #backend_state{}}.
init(_) ->
    Dbref = backend_db:create_db(),
    init_accounts(Dbref),
    State = #backend_state{dbref = Dbref},
    {ok, State}.

handle_call({account, AccNo}, _From, State = #backend_state{dbref = Dbref}) ->
    Reply = backend_db:lookup(AccNo, Dbref),
    {reply, Reply, State};
handle_call({accounts_by_name, Name}, _From, State = #backend_state{dbref = Dbref}) ->
    Reply = backend_db:lookup_by_name(Name, Dbref),
    {reply, Reply, State};
handle_call({list_accounts}, _From, State = #backend_state{dbref = Dbref}) ->
    Reply = backend_db:all_accounts(Dbref),
    {reply, Reply, State};
handle_call({pin_valid, AccNo, Pin}, _From, State = #backend_state{dbref = Dbref}) ->
    Reply = backend_db:is_pin_valid(AccNo, Pin, Dbref),
    {reply, Reply, State};
handle_call({withdraw, AccNo, Amount}, _From, State = #backend_state{dbref = Dbref}) ->
    Reply = backend_db:debit(AccNo, Amount, Dbref),
    {reply, Reply, State};
handle_call(reboot, _From, _State) ->
    Dbref = backend_db:create_db(),
    State = #backend_state{dbref = Dbref},
    ok = init_accounts(Dbref),
    {reply, ok, State}.

handle_cast({deposit, AccNo, Amount}, State = #backend_state{dbref = Dbref}) ->
    _Reply = backend_db:credit(AccNo, Amount, Dbref),
    {noreply, State}.

terminate(_Reason, #backend_state{dbref = Dbref}) ->
    backend_db:close(Dbref),
    ok.
