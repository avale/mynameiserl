-module(entity).
-compile(export_all).

-behaviour(gen_server).


start_cell(Neighbours) ->
	gen_server:start_link(?MODULE, [Neighbours], []).

%% ====================================================================
%% callback functions
%% ====================================================================

init(Neighbours) ->
	Type = empty,
	{ok,[Neighbours, Type]}.

handle_call(_, _From, State) ->
	{reply, ok, State}.

handle_cast(_, State) ->
	{noreply, State}.

handle_info(Info, State) ->
	case Info of
		{spawn_plant, From} ->
			From ! gen_server:call(?MODULE, spawn_plant);
		{spawn_herbivore, From} ->
			From ! gen_server:call(?MODULE, spawn_herbivore);
		{spawn_carnicore, From} ->
			From ! gen_server:call(?MODULE, spawn_carnicore);
		{move_herbivore, From} ->
			From ! gen_server:call(?MODULE, move_herbivore);
		{move_carnivore, From} ->
			From ! gen_server:call(?MODULE, move_carnivore);
		{eat_grass, From} ->
			From ! gen_server:call(?MODULE, eat_grass);
		{eat_herbivore, From} ->
			From ! gen_server:call(?MODULE, eat_herbivore);
	end
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
