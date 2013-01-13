-module(entity).
%-export([start_cell/3]).
-compile([export_all]).
-behaviour(gen_server).

%% ====================================================================
%% External functions
%% ====================================================================

start_cell(Coordinates, C, R, Color) ->
	{X, Y} = Coordinates,
	Name = list_to_atom("x" ++ integer_to_list(X) ++ "y" ++ integer_to_list(Y)),
	io:format("Hej: ~p~n",[Name]),
	gen_server:start_link({local,Name},?MODULE,[Coordinates, C, R, Color],[]).

%% ====================================================================
%% callback functions
%% ====================================================================

init([Coordinates, C, R, Color]) ->
	%Type = empty,
	Nbr = sur_nodes(Coordinates,C,R),
	{X,Y} = Coordinates,
	case Color of	
		1 -> Code = "#1E5B2D"; %Turf
		2 -> Code = "#EAD8E9"; %Rabbit
		3 -> Code = "#933F17"; %Fox
		_ -> Code = "#867754" %Empty
	end
	frame ! {change_cell, X,Y, Code},
	{ok,[Coordinates,Nbr,Type]}.

handle_call(_, _From, State) ->
	{reply, ok, State}.

handle_cast(_, State) ->
	{noreply, State}.

handle_info(Info, State) ->
	case Info of
		{spawn_plant, From} ->
			From ! gen_server:call(self(), spawn_plant);
		{spawn_herbivore, From} ->
			From ! gen_server:call(self(), spawn_herbivore);
		{spawn_carnicore, From} ->
			From ! gen_server:call(self(), spawn_carnivore);
		{move_herbivore, From} ->
			From ! gen_server:call(self(), move_herbivore);
		{move_carnivore, From} ->
			From ! gen_server:call(self(), move_carnivore);
		{eat_grass, From} ->
			From ! gen_server:call(self(), eat_grass);
		{eat_herbivore, From} ->
			From ! gen_server:call(self(), eat_herbivore)
	end,
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% ====================================================================
%% Internal functions
%% ====================================================================

sur_nodes({X,Y}, Max_X, Max_Y) ->
	L = [{Xn, Yn} || 
			Yn <- [Y-1, Y, Y+1], Xn <- [X-1, X, X+1], 
			Xn >= 0, Xn < Max_X, Yn >=0, Yn < Max_Y, {Xn, Yn} =/= {X,Y}],
	lists:map(fun({C,R}) -> list_to_atom("x" ++ integer_to_list(C) ++ "y" ++ integer_to_list(R)) end ,L).

