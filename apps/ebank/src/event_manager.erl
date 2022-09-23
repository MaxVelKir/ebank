-module(event_manager).

-export([
    start_link/0,
    notify/1,
    get_stats/0,
    delete_handler/2
]).

-spec start_link() -> {ok, pid()} | {error, Reason :: term()}.
start_link() ->
    case gen_event:start_link({local, ?MODULE}) of
        Ok = {ok, _} ->
            add_handler(?MODULE, stolen, []),
            add_handler(?MODULE, statistics, []),
            Ok;
        Error ->
            Error
    end.

-spec notify(term()) -> ok.
notify(Event) ->
    gen_event:notify(?MODULE, Event).

-spec get_stats() -> [{Counter :: atom(), Value :: number()}].
get_stats() ->
    gen_event:call(?MODULE, statistics, get_stats).

-spec add_handler(module(), gen_event:handler(), gen_event:add_handler_args()) ->
    gen_event:add_handler_ret().
add_handler(Manager, Handler, Args) ->
    gen_event:add_handler(Manager, Handler, Args).

-spec delete_handler(gen_event:handler(), gen_event:del_handler_ret()) ->
    gen_event:del_handler_ret().
delete_handler(Handler, Args) -> gen_event:delete_handler(Handler, stolen, Args).
