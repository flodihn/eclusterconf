%%------------------------------------------------------------------
%% @author Christian Flodihn <christian@flodhn.se>
%% @copyright Christian Flodihn
%% @doc
%%  This module stores configuration in a mnesia table.
%% @end
%% ------------------------------------------------------------------
%% @hidden
-module(mnesia_clusterconf).

-export([
		init/0,
		create_db/0,
        set_config/2,
        get_config/1,
        get_configs/0,
        has_config/1,
		wipe/0]).

-record(eclusterconf, {key, value}).
-define(DB_TABLE, eclusterconf).

init() ->
	mnesia:start(),
	create_db(),
	{ok, []}.

create_db() ->
    case mnesia:table_info(schema, disc_copies) of
        [] -> 
            mnesia:change_table_copy_type(schema, node(), disc_copies);
        _ExistingDiskNodes ->
            pass
    end,
	case lists:member(?DB_TABLE, mnesia:system_info(tables)) of
		true ->
			pass;
		false ->
            Attributes = record_info(fields, ?DB_TABLE),
			mnesia:create_table(?DB_TABLE, 
							[{disc_copies, [node()]},
							{type, set},
							{attributes, Attributes}])
	end.

set_config(Key, Value) ->
	Fun = fun() ->
		Record = #eclusterconf{key = Key, value = Value},
		mnesia:write(?DB_TABLE, Record, write)
	end,
	case mnesia:transaction(Fun) of
		{atomic, ok} ->
			{ok, set_config};
		{aborted, Reason} ->
			{error, Reason}
	end.

has_config(Key) -> 
	case mnesia:dirty_read(?DB_TABLE, Key) of
		[] -> false;
		[_Other] -> true
	end.

get_config(Key) ->
    case mnesia:dirty_read(?DB_TABLE, Key) of
        [Record] ->
            Record#eclusterconf.value;
        [] ->
            undefined
    end.

get_configs() ->
    FirstKey = mnesia:dirty_first(?DB_TABLE),
    get_configs(FirstKey, []).

get_configs('$end_of_table', Acc) ->
    Acc;

get_configs(Key, Acc) ->
    Value = get_config(Key),
    NextKey = mnesia:dirty_next(?DB_TABLE, Key),
    get_configs(NextKey, [{Key, Value} | Acc]).

wipe() ->
	mnesia:clear_table(?DB_TABLE).
