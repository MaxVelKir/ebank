-module(helpers).

-export([ensure_started/1]).

ensure_started(backend) ->
    avoid_crash(event_manager:start_link()),
    avoid_crash(backend:start_link()).

avoid_crash(Result) ->
    case Result of
        Ok = {ok, _Ref1} ->
            io:format("ok", []),
            Ok;
        {error, {already_started, Pid1}} ->
            io:format("already_started", []),
            {ok, Pid1}
    end.
