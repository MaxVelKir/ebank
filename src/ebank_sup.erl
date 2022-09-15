%%%------------------------------------------------------------
%% @doc ebank top level supervisor.
%% @end
%%%------------------------------------------------------------

-module(ebank_sup).

-behaviour(supervisor).

-export([
    start_atm/0
]).

-export([
    init/1
]).

-define(SERVER, ?MODULE).

start_atm() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 0,
        period => 1
    },

    ChildSpecs = [
        #{
            id => backend,
            start => {atm, start_link, []},
            restart => permanent,
            shutdown => brutal_kill,
            type => worker,
            modules => [backend]
        },
        #{
            id => atm,
            start => {atm, start_link, [atm1]},
            restart => permanent,
            shutdown => brutal_kill,
            type => worker,
            modules => [atm]
        }
    ],
    {ok, {SupFlags, ChildSpecs}}.