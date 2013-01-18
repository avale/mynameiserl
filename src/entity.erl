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
	gen_server:start({local,Name},?MODULE,[Coordinates, C, R, T],[]).

%% ====================================================================
%% callback functions
%% ====================================================================

init([Coordinates, C, R, T]) ->
	Nbr = sur_nodes(Coordinates,C,R),
	{X,Y} = Coordinates,
	case T of
		1 -> Class = "plant", Type = #life{plant=#plant{class = Class, growth = 4, age=0, _ = '_'}, animal=#empty{}};
		2 -> Class = "herbivore", Type = #life{plant=#empty{}, animal=#herbivore{class = Class, hunger=0, starvation=10, _ = '_'}};
		3 -> Class = "carnivore", Type = #life{plant=#empty{}, animal=#carnivore{class = Class, hunger=0, starvation=10, _ = '_'}};
		-1 -> Class = "barrier", Type = #barrier{class = Class, _ = '_'};
		_ -> Class = "empty", Type = #empty{class = Class, _ = '_'}
	end,
	frame ! {change_cell, X,Y, Class},
	{ok,[Coordinates,Nbr,Type,{none, empty}]}.

	



handle_call({move_herbivore, Animal}, _From, [Coordinates, Nbr, State, T]) ->
	{X,Y} = Coordinates,
	Type = element(1, State),
	case Type of
		life ->
			Reply = State#life.animal,
			NewState = State#life{animal = Animal};
		_ ->
			Reply = State#empty{},
			NewState = #life{animal=Animal, plant=#empty{}}
	end,
	frame ! {change_cell, X, Y, "herbivore"},
	{reply, Reply, [Coordinates, Nbr,NewState, T]};

handle_call(is_notAnimal, _From, [Coordinates,Nbr,State|T]) ->
	case element(1, State) of
		empty -> Reply = {yes, self()};
		barrier -> Reply = false;
		life -> 
			case element(1, State#life.animal) of
				empty -> Reply = {yes, self()};
				_ -> Reply = false
			end
	end,
	{reply, Reply, [Coordinates,Nbr,State|T]}.

handle_cast(stop, State) ->
	{stop, shutdown, State};

handle_cast(tick, [Coordinates,Nbr,State,Action]) ->
	Type = element(1, State),
	case Type of
		empty ->
			NewState = State,
			NewAction = Action,
			ok;
		barrier ->
			NewState = State,
			NewAction = Action,
			ok;
		life ->
			Plant = State#life.plant,
			PlantType = element(1, Plant),
			Animal = State#life.animal,
			AnimalType = element(1, Animal),
			case PlantType of
				plant ->
					OldAge = Plant#plant.age,
					NewPlant = Plant#plant{age=OldAge+1};
				_ ->
					NewPlant = Plant#empty{},
					ok
			end,
			case AnimalType of
				herbivore ->
					spawn(?MODULE, find_aval, [Nbr,[], self()]),
					Hunger = Animal#herbivore.hunger,
					NewAnimal = Animal#herbivore{hunger=Hunger+1},
					NewAction = Action,
					ok;
				carnivore ->
					Hunger = Animal#carnivore.hunger,
					NewAnimal = Animal#carnivore{hunger=Hunger+1},
					NewAction = Action,
					ok;
				_ ->
					NewAnimal = Animal#empty{},
					NewAction = Action,
					ok
			end,
			NewState = State#life{plant=NewPlant, animal=NewAnimal}
	end,
	{noreply, [Coordinates, Nbr, NewState, NewAction]};

handle_cast(tock, [Coordinates,Nbr,State,Action]) ->
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
					case (Age rem Growth) of
						0 ->
							random:seed(now()),
							Victim = lists:nth(random:uniform(8), Nbr),
							gen_server:cast(Victim,spawn_plant);
						_ ->
							ok
					end;
				_ ->
					ok
			end,
			case AnimalType of
				herbivore ->
					Hunger = Animal#herbivore.hunger,
					Starvation = Animal#herbivore.starvation,
					case Hunger > Starvation of
						true ->
							case element(1, State#life.plant) of
								plant ->
									NewState = State#life{animal=#empty{}},
									New = "plant";
								_ -> 
									NewState = #empty{},
									New = "empty"
							end,
							io:format("[~p, ~p] Aaaaaaargh~n",[X, Y]),
							frame ! {change_cell, X, Y, New};
						_ ->
							case Action of
								{goto, Target} ->
									io:format("[~p|PLONG]: JAG GAR TILL ~p~n",[Coordinates,Target]),
									NewAnimal = gen_server:call(Target, {move_herbivore, Animal}),
									case element(1, State#life.plant) of
										plant ->
											NewState = State#life{animal= NewAnimal},
											New = "plant";
										_ -> 
											NewState = #empty{},
											New = "empty"
									end,
									frame ! {change_cell, X, Y, New};
								{none, _} ->
									NewState = State
							end
					end,
					ok;
				carnivore ->
					Hunger = Animal#carnivore.hunger,
					Starvation = Animal#carnivore.starvation,
					case Hunger > Starvation of
						true ->
							case element(1, State#life.plant) of
								plant ->
									NewState = State#life{animal=#empty{}},
									New = "plant";
								_ -> 
									NewState = #empty{},
									New = "empty"
							end,
							io:format("[~p, ~p] Meeeh~n",[X, Y]),
							frame ! {change_cell, X, Y, New};
						_ ->
							NewState = State,
							ok
					end,
					ok;
				_ ->
					NewState = State,
					ok 
			end
	end,
	NewAction = {none, empty},
	{noreply, [Coordinates,Nbr,NewState,NewAction]};

handle_cast(spawn_plant, [{X,Y},Nbr,State|T]) ->
	case element(1, State) of
		empty ->
			Class = "plant", 
			NewState = #life{plant=#plant{class = Class, age = 0, growth = 4, _ = '_'}, animal=#empty{}},
			frame ! {change_cell,X,Y,Class};
		barrier ->
			NewState = State,
			ok;
		_ -> 
			Class = element(1, State#life.animal),
			NewState = State#life{plant=#plant{class = "plant", age = 0, growth = 4, _ = '_'}}
	end,
	{noreply, [{X,Y},Nbr,NewState|T]};

handle_cast(_, [Coordinates,Nbr,State|_T]) ->
	{noreply, [Coordinates,Nbr,State|_T]}.


handle_info(Info, [Coordinates,Nbr,State|_T]) ->
	case Info of
		{spawn_herbivore, From} ->
			NewState = State,
			NewAction = {none, empty},
			From ! gen_server:call(self(), spawn_herbivore);
		{spawn_carnicore, From} ->
			NewState = State,
			NewAction = {none, empty},
			From ! gen_server:call(self(), spawn_carnivore);
		{move_herbivore, From} ->
			NewState = State,
			NewAction = {none, empty},
			From ! gen_server:call(self(), move_herbivore);
		{move_carnivore, From} ->
			NewState = State,
			NewAction = {none, empty},
			From ! gen_server:call(self(), move_carnivore);
		{eat_grass, From} ->
			NewState = State,
			NewAction = {none, empty},
			From ! gen_server:call(self(), eat_grass);
		{eat_herbivore, From} ->
			NewState = State,
			NewAction = {none, empty},
			From ! gen_server:call(self(), eat_herbivore);
		{move, To} ->
			case To of
				no -> 
					NewState = State,
					NewAction = {none, empty};
				_ ->
					NewState = State,
					NewAction = {goto, To}
			end;
		{vision, Source, Direction, Range, Objects, Origin} ->
			Next = getAdjecentAt(Coordinates, C, R, Direction),
			case Range>0 of
				true ->
					Type = element(1, State),
					case Type of
						life ->
							Plant = State#life.plant,
							PlantType = element(1, Plant),
							Animal = State#life.animal,
							AnimalType = element(1, Animal),
							case Source of
								herbivore ->
									case {PlantType, AnimalType} of
										{plant, herbivore} ->
											Next ! {vision, Source, Direction, Range-1,
												({plant, Range} ++ {herbivore, Range} ++ Objects), Origin};
										{plant, carnivore} ->
											Next ! {vision, Source, Direction, Range-1,
												({plant, Range} ++ {carnivore, Range} ++ Objects), Origin};
										{empty, herbivore} ->
											Next ! {vision, Source, Direction, Range-1,
												({herbivore, Range} ++ Objects), Origin};
										{empty, carnivore} ->
											Next ! {vision, Source, Direction, Range-1,
												({carnivore, Range} ++ Objects), Origin};
										_ ->
											Next ! {vision, Source, Direction, Range-1,
												({plant, Range} ++ Objects), Origin}
									end;
								carnivore ->
									case AnimalType of
										herbivore ->
											Next ! {vision, Source, Direction, Range-1,
											({herbivore, Range} ++ Objects), Origin};
										_ ->
											Next ! {vision, Source, Direction, Range-1,
											Objects, Origin}
									end
							end;
						empty ->
							Next ! {vision, Source, Direction, Range-1, Objects, Origin};
						barrier ->
							Origin ! {vision_re, Source, Direction, Objects}
					end;
				false ->
					Origin ! {vision_re, Source, Direction, Objects}
			end;
		_ -> 
			io:format("~p: Undefined message: ~p~n",[Coordinates, Info]),
			gen_server:cast(self(), tock),
			NewAction = {none, empty},
			NewState = State
	end,
	{noreply, [Coordinates,Nbr,NewState,NewAction]}.

terminate(_Reason, State) ->
	{X,Y} = hd(State),
	frame ! {change_cell, X, Y, "red"}, 
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

find_aval([],Ack,P) ->
	case Ack of
		[] ->
			P ! {move, no};
		_ -> 
			random:seed(now()),
			Victim = lists:nth(random:uniform(length(Ack)), Ack),
			P ! {move, Victim}
	end;
find_aval([H|T],Ack, P) ->
	Next = try gen_server:call(H, is_notAnimal, 50)
		   catch
				Error:Reason ->
					io:format("I aint touching that!: ~p~n", [H]),
					false
		   end,
	case Next of
		false ->
			find_aval(T, Ack, P);
		{yes, Pid} ->
			find_aval(T, [Pid|Ack], P);
		_ ->
			find_aval(T, Ack, P)
	end.

test() -> 
	driver ! {step}.

getAdjecentAt ({X,Y}, Max_X, Max_Y, Direction) ->
	case Direction of
		n  -> %% North
			case (0<Y) of
				true  -> Next = [x, X, y, Y-1];
				false -> [x, X, y, Y]
			end;
		ne -> %% Northeast
			case (X<Max_X) and (0<Y) of
				true  -> Next = [x, X+1, y, Y-1];
				false -> [x, X, y, Y]
			end;
		e  -> %% East
			case (X<Max_X) of
				true  -> Next = [x, X+1, y, Y];
				false -> [x, X, y, Y]
			end;
		se -> %% Southeast
			case (X<Max_X) and (Y<Max_Y) of
				true  -> Next = [x, X+1, y, Y+1];
				false -> [x, X, y, Y]
			end;
		s  -> %% South
			case (Y<Max_Y) of
				true  -> Next = [x, X, y, Y+1];
				false -> [x, X, y, Y]
			end;
		sw -> %% Southwest
			case (0<X) and (Y<Max_Y) of
				true  -> Next = [x, X-1, y, Y+1];
				false -> [x, X, y, Y]
			end;
		w  -> %% West
			case (0<X) of
				true  -> Next = [x, X-1, y, Y];
				false -> [x, X, y, Y]
			end;
		nw -> %% Northwest
			case (0<X) and (0<Y) of
				true  -> Next = [x, X-1, y, Y-1];
				false -> [x, X, y, Y]
			end;
		_ -> Next = [x, X, y, Y]
	end,
	list_to_atom(Next).