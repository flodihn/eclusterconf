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
    set_config/3,
    get_config/1,
    get_configs/0
    ]).

start_link(ImplMod) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [ImplMod], []).

set_config(Key, Value, Type) ->
    gen_server:call(?SERVER, {set_config, {key, Key}, {value, Value},
        {type, Type}}).

get_config(Key) ->
    gen_server:call(?SERVER, {get_config, {key, Key}}).

get_configs() ->
    gen_server:call(?SERVER, get_configs).


%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([ImplMod]) ->
    ImplMod:init(),
    {ok, #state{implmod=ImplMod}}.

handle_call({set_config, {key, Key}, {value, Value}, {type, Type}}, _From,
        #state{implmod=ImplMod} = State) ->
    ImplMod:set_config(Key, Value, Type),
    {reply, ok, State};

handle_call({get_config, {key, Key}}, _From,
        #state{implmod=ImplMod} = State) ->
    Config = ImplMod:get_config(Key),
    {reply, {ok, Config}, State};

handle_call(get_configs, _From, #state{implmod=ImplMod} = State) ->
    PropList = ImplMod:get_configs(),
    {reply, {ok, PropList}, State};

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

