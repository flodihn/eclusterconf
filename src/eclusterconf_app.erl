-module(eclusterconf_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
    application:start(eclusterconf).

start(_StartType, _StartArgs) ->
    eclusterconf_sup:start_link().

stop(_State) ->
    ok.
