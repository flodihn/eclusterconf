-module(eclusterconf_srv).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-record(state, {implmod}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-export([
    set_config/2,
    get_config/1
    ]).

start_link(ImplMod) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [ImplMod], []).

set_config(Key, Value) ->
    gen_server:call(?SERVER, {set_config, {key, Key}, {value, Value}}).

get_config(Key) ->
    gen_server:call(?SERVER, {get_config, {key, Key}}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([ImplMod]) ->
    ImplMod:init(),
    {ok, #state{implmod=ImplMod}}.

handle_call({set_config, {key, Key}, {value, Value}}, _From,
        #state{implmod=ImplMod} = State) ->
    ImplMod:set_config(Key, Value),
    {reply, ok, State};

handle_call({get_config, {key, Key}}, _From,
        #state{implmod=ImplMod} = State) ->
    Value = ImplMod:get_config(Key),
    {reply, {ok, Value}, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

