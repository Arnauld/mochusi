%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc mochusi.

-module(mochusi).
-author("Mochi Media <dev@mochimedia.com>").
-export([start/0, stop/0]).

ensure_started(App) ->
    io:fwrite("mochusi:Ensure started: \"~p\"...~n", [App]),
	case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.


%% @spec start() -> ok
%% @doc Start the mochusi server.
start() ->
    io:fwrite("mochusi:Starting...~n"),
	mochusi_deps:ensure(),
    ensure_started(crypto),
    application:start(mochusi).


%% @spec stop() -> ok
%% @doc Stop the mochusi server.
stop() ->
    application:stop(mochusi).
