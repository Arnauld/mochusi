%% @author Mochi Media <dev@mochimedia.com>
%% @copyright mochusi Mochi Media <dev@mochimedia.com>

%% @doc Callbacks for the mochusi application.

-module(mochusi_app).
-author("Mochi Media <dev@mochimedia.com>").

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for mochusi.
start(_Type, _StartArgs) ->
    io:fwrite("mochusi_app:Starting...\"~p\" // \"~p\"~n", [_Type,_StartArgs]),
	mochusi_deps:ensure(),
    mochusi_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for mochusi.
stop(_State) ->
    ok.
