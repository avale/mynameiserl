-module(entity).
-include("entity.hrl").
%-export([start_cell/3]).
-compile([export_all]).
-behaviour(gen_server).

%% ====================================================================
%% External functions
%% ====================================================================

start_cell(Coordinates, C, R, T) ->
	{X, Y} = Coordinates,
	Name = list_to_atom("x" ++ integer_to_list(X) ++ "y" ++ integer_to_list(Y)),
	io:format("Hej: ~p~n",[Name]),
	gen_server:start_link({local,Name},?MODULE,[Coordinates, C, R, T],[]).

%% ====================================================================
%% callback functions
%% ====================================================================

init([Coordinates, C, R, T]) ->
	Nbr = sur_nodes(Coordinates,C,R),
	{X,Y} = Coordinates,
	case T of
		1 -> Class = "plant", Type = #life{plant=#plant{class = Class, age = 0, growth = 4, _ = '_'}, animal=#empty{}};
		2 -> Class = "herbivore", Type = #life{plant=#empty{}, animal=#herbivore{class = Class, _ = '_'}};
		3 -> Class = "carnivore", Type = #life{plant=#empty{}, animal=#carnivore{class = Class, _ = '_'}};
		-1 -> Class = "barrier", Type = #barrier{class = Class, _ = '_'};
		_ -> Class = "empty", Type = #empty{class = Class, _ = '_'}
	end,
	frame ! {change_cell, X,Y, Class},
	{ok,[Coordinates,Nbr,Type]}.

handle_call(_, _From, State) ->
	{reply, ok, State}.

handle_cast(stop, State) ->
	{stop, shutdown, State};
handle_cast(_, [Coordinates,Nbr,State]) ->
	{noreply, [Coordinates,Nbr,State]}.


handle_info(Info, [Coordinates,Nbr,State]) ->
	case Info of
		{tick} ->
			{X,Y} = Coordinates,
			Type = element(1, State),
			case Type of
				empty ->
					NewState = State,
					ok;
				barrier ->
					NewState = State,
					ok;
				life ->
					Plant = State#life.plant,
					PlantType = element(1, Plant),
					Animal = State#life.animal,
					AnimalType = element(1, Animal),
					case PlantType of
						plant ->
							OldAge = Plant#plant.age,
							NewPlant = Plant#plant{age=OldAge+1},
							Age = Plant#plant.age,
							io:format("Age: ~p~n",[Age]);
						_ ->
							NewPlant = Plant#empty{},
							ok
					end,
					case AnimalType of
						herbivore ->
							NewAnimal = Animal#herbivore{},
							ok;
						carnivore ->
							NewAnimal = Animal#carnivore{},
							ok;
						_ ->
							NewAnimal = Animal#empty{},
							ok
					end,
					NewState = State#life{plant=NewPlant, animal=NewAnimal}
			end,
			ok;
		{tock} ->
			{X,Y} = Coordinates,
			Type = element(1, State),
			case Type of
				empty ->
					NewState = State,
					ok;
				barrier ->
					NewState = State,
					ok;
				life ->
					Plant = State#life.plant,
					PlantType = element(1, Plant),
					Animal = State#life.animal,
					AnimalType = element(1, Animal),
					case PlantType of
						plant ->
							Age = Plant#plant.age,
							Growth = Plant#plant.growth,
							NewState = State,
							case (Age rem Growth) of
								0 ->
									random:seed(now()),
									Victim = lists:nth(random:uniform(8), Nbr),
									Victim ! {spawn_plant, self()},
									frame ! {change_cell, X, Y, "plant"};
								_ ->
									ok
							end;
						_ ->
							NewState = State,
							ok
					end
					%case Animal of
					%	herbivore ->
					%		NewState = State,
					%		ok;
					%	carnivore ->
					%		NewState = State,
					%		ok;
					%	_ ->
					%		NewState = State,
					%		ok
					%end
			end,
			ok;
		{spawn_plant, From} ->
			case element(1, State) of
				empty ->
					Class = "plant", 
					NewState = #life{plant=#plant{class = Class, age = 0, growth = 4, _ = '_'}, animal=#empty{}};
				_ ->
					NewState = State,
					ok
			end;
		{spawn_herbivore, From} ->
			NewState = State,
			From ! gen_server:call(self(), spawn_herbivore);
		{spawn_carnicore, From} ->
			NewState = State,
			From ! gen_server:call(self(), spawn_carnivore);
		{move_herbivore, From} ->
			NewState = State,
			From ! gen_server:call(self(), move_herbivore);
		{move_carnivore, From} ->
			NewState = State,
			From ! gen_server:call(self(), move_carnivore);
		{eat_grass, From} ->
			NewState = State,
			From ! gen_server:call(self(), eat_grass);
		{eat_herbivore, From} ->
			NewState = State,
			From ! gen_server:call(self(), eat_herbivore)
	end,
	{noreply, [Coordinates,Nbr,NewState]}.

terminate(_Reason, State) ->
	{X,Y} = hd(State),
	frame ! {change_cell, X, Y, pink}, 
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

