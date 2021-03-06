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
	gen_server:start({local,Name},?MODULE,[Coordinates, C, R, T],[]).

%% ====================================================================
%% callback functions
%% ====================================================================

init([Coordinates, C, R, T]) ->
	Nbr = sur_nodes(Coordinates,C,R),
	{X,Y} = Coordinates,
	case T of
		1 ->
			Class = "plant",
			Type = #life{plant=#plant{class = Class, growth = variables:plantGrowth(), age=0, _ = '_'}, animal=#empty{}};
		2 ->
			Class = "herbivore",
			Type = #life{
						plant=#empty{},
						animal=#herbivore{class = Class, age=0, hunger=0, exited=0, vision=variables:herbivoreVision(), _ = '_', starvation=variables:herbivoreStarvation()}};
		3 ->
			Class = "carnivore",
			Type = #life{plant=#empty{}, animal=#carnivore{class = Class, age=0, hunger=0, exited=0, vision=variables:carnivoreVision(), _ = '_', starvation=variables:carnivoreStarvation()}};
		-1 ->
			Class = "barrier", Type = #barrier{class = Class, _ = '_'};
		_ ->
			Class = "empty", Type = #empty{class = Class, _ = '_'}
	end,
	frame ! {change_cell, X,Y, Class},
	{ok,[Coordinates,Nbr,Type,{none, empty}]}.

handle_call({move_herbivore, Animal}, _From, [Coordinates, Nbr, State, T]) ->
	{X,Y} = Coordinates,
	Type = element(1, State),
	case Type of
		life ->
			CurrentAnimal = State#life.animal,
			case element(1, CurrentAnimal) of
				empty ->
					Reply = State#life.animal,
					NewState = State#life{animal = Animal};
				_ ->
					Reply = #barrier{class = "barrier", _ = '_'},
					NewState = State
			end;
		barrier ->
			Reply = State#barrier{},
			NewState = State;
		_ ->
			Reply = State#empty{},
			NewState = #life{animal=Animal, plant=#empty{}}
	end,
	broadcast(Nbr,element(1, Animal),Coordinates),
	frame ! {change_cell, X, Y, element(1, Animal)},
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
					case is_food(Nbr, plant) of
						{Pid,_} -> 
							NewAction = {eat, Pid},
							NewAnimal = Animal;
						_ ->
							lookAround(State,Nbr),
							Hunger = Animal#herbivore.hunger,
							Age = Animal#herbivore.age,
							Exited = Animal#herbivore.exited,
							NewAnimal = Animal#herbivore{hunger=Hunger+1, age=Age+1, exited=Exited+1},
							NewAction = Action
					end;
				carnivore ->
					case is_food(Nbr, herbivore) of
						{Pid,_} -> 
							NewAction = {eat, Pid},
							NewAnimal = Animal;
						_ ->
							lookAround(State,Nbr),
							Hunger = Animal#carnivore.hunger,
							Age = Animal#carnivore.age,
							Exited = Animal#carnivore.exited,
							NewAnimal = Animal#carnivore{hunger=Hunger+1, age=Age+1, exited=Exited+1},
							NewAction = Action
					end;
				_ ->
					NewAnimal = Animal#empty{},
					NewAction = Action,
					ok
			end,
			NewState = State#life{plant=NewPlant, animal=NewAnimal}
	end,
	{noreply, [Coordinates, Nbr, NewState,NewAction]};

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
							{Victim, _} = lists:nth(random:uniform(8), Nbr),
							gen_server:cast(Victim,{spawn_plant, Growth});
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
									broadcast(Nbr,plant,{X,Y}),
									New = "plant";
								_ -> 
									NewState = #empty{},
									broadcast(Nbr,empty,{X,Y}),
									New = "empty"
							end,
							frame ! {change_cell, X, Y, New};
						_ ->
							case Action of
								{goto, Target} ->
									NewState = State,
									gen_server:cast(Target, {move_animal, Animal, self()});									
								{eat,Pid} ->
									gen_server:cast(Pid, {eaten, plant, self()}),
									NewState = State;
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
									broadcast(Nbr,plant,{X,Y}),
									New = "plant";
								_ -> 
									NewState = #empty{},
									broadcast(Nbr,empty,{X,Y}),
									New = "empty"
							end,
							frame ! {change_cell, X, Y, New};
						_ ->
							case Action of
								{goto, Target} ->
									NewState = State,
									gen_server:cast(Target, {move_animal, Animal, self()});
								{eat,Pid} ->
									gen_server:cast(Pid, {eaten, herbivore, self()}),
									NewState = State;
								{none, _} ->
									NewState = State
							end
					end,
					ok;
				_ ->
					NewState = State,
					ok 
			end
	end,
	NewAction = {none, empty},
	{noreply, [Coordinates,Nbr,NewState,NewAction]};

handle_cast({spawn_plant, ParentGrowth}, [{X,Y},Nbr,State|T]) ->
	random:seed(now()),
	NewGrowth = ParentGrowth + (random:uniform(3) - 2),
	case NewGrowth < 2 of
		true ->
			Growth = 2;
		_ ->
			Growth = NewGrowth
	end,
	case element(1, State) of
		empty ->
			Class = "plant",
			NewState = #life{plant=#plant{class = Class, age = 0, growth = Growth, _ = '_'}, animal=#empty{}},
			broadcast(Nbr,plant,{X,Y}),
			frame ! {change_cell,X,Y,Class};
		barrier ->
			NewState = State,
			ok;
		_ -> 
			NewState = State#life{plant=#plant{class = "plant", age = 0, growth = Growth, _ = '_'}}
	end,
	{noreply, [{X,Y},Nbr,NewState|T]};

handle_cast({spawn_animal, Animal}, [{X,Y},Nbr,State|T]) ->
	StateType = element(1, State),
	AnimalType = element(1, Animal),
	case StateType of
		barrier ->
			NewState = State;
		empty ->	
			case AnimalType of
				herbivore ->
					ParentVision = Animal#herbivore.vision,
					ParentStarvation = Animal#herbivore.starvation,
					{Vision, Starvation} = generateGenetics(ParentVision, ParentStarvation),
					NewState = #life{plant=#empty{}, animal=#herbivore{class="herbivore", age=0, hunger=0, vision=Vision, starvation=Starvation}},
					broadcast(Nbr,herbivore,{X,Y}),
					frame ! {change_cell,X,Y,"herbivore"};
				carnivore ->
					ParentVision = Animal#carnivore.vision,
					ParentStarvation = Animal#carnivore.starvation,
					{Vision, Starvation} = generateGenetics(ParentVision, ParentStarvation),
					NewState = #life{plant=#empty{}, animal=#carnivore{class="carnivore", age=0, hunger=0, vision=Vision, starvation=Starvation}},
					broadcast(Nbr,carnivore,{X,Y}),
					frame ! {change_cell,X,Y,"carnivore"};
				_ ->
					NewState = State
			end;
		life ->
			case element(1, State#life.animal) of
				empty ->
					Plant = State#life.plant,
					case AnimalType of
						herbivore ->
							ParentVision = Animal#herbivore.vision,
							ParentStarvation = Animal#herbivore.starvation,
							{Vision, Starvation} = generateGenetics(ParentVision, ParentStarvation),
							NewState = #life{plant=Plant, animal=#herbivore{class="herbivore", age=0, hunger=0, vision=Vision, starvation=Starvation}},
							broadcast(Nbr,herbivore,{X,Y}),
							frame ! {change_cell,X,Y,"herbivore"};
						carnivore ->
							ParentVision = Animal#carnivore.vision,
							ParentStarvation = Animal#carnivore.starvation,
							{Vision, Starvation} = generateGenetics(ParentVision, ParentStarvation),
							NewState = #life{plant=Plant, animal=#carnivore{class="carnivore", age=0, hunger=0, vision=Vision, starvation=Starvation}},
							broadcast(Nbr,carnivore,{X,Y}),
							frame ! {change_cell,X,Y,"carnivore"};
						_ ->
							NewState = State
					end;
				_ ->
					NewState = State
			end;
		_ ->
			NewState = State
	end,
	{noreply, [{X,Y},Nbr,NewState|T]};

handle_cast({eaten, Type, From}, [Coor,Nbr,State,Action]) ->
	{X,Y} = Coor,
	case element(1, State) of
		life ->
			case Type of
				herbivore ->
					NewAction = {none, empty},
					NewState = State#life{animal= #empty{}},
					broadcast(Nbr,element(1, State#life.plant),Coor),
					frame ! {change_cell,X,Y,element(1, State#life.plant)},
					From ! {im_done};
				plant ->
					NewAction = {none, empty},
					NewState = State#life{plant= #empty{}},
					broadcast(Nbr,empty,Coor),
					frame ! {change_cell,X,Y,"empty"},
					From ! {im_done};
				_ ->
					NewState = State,
					NewAction = Action,
					From ! {not_eaten}
			end;
		_ ->
			NewState = State,
			NewAction = Action,
			From ! {not_eaten}
	end,
	{noreply, [Coor,Nbr,NewState,NewAction]};

handle_cast({move_animal, Animal, From}, [Coor, Nbr, State, Action]) ->
	{X,Y} = Coor,
	case element(1, State) of
		empty -> 
			NewState = #life{animal=Animal, plant=#empty{}},
			frame ! {change_cell,X,Y,element(1, Animal)},
			broadcast(Nbr,element(1, Animal),Coor),
			From ! {move_here};
		barrier ->
			NewState = State,
			From ! {dont_move};
		life ->
			case element(1, State#life.animal) of
				empty ->
					NewState = State#life{animal= Animal},
					frame ! {change_cell,X,Y,element(1, Animal)},
					broadcast(Nbr,element(1, Animal),Coor),
					From ! {move_here};
				_ -> 
				NewState = State,
				From ! {dont_move}
			end;
		_ ->
			NewState = State
	end,
	{noreply, [Coor, Nbr, NewState, Action]};

handle_cast(_, [Coordinates,Nbr,State|_T]) ->
	{noreply, [Coordinates,Nbr,State|_T]}.


handle_info(Info, [Coordinates,Nbr,State,Action]) ->
	case Info of
		{new_type,Type,From} ->
			NewNbr = lists:keyreplace(From, 1, Nbr, {From, Type}),
			NewState = State,
			NewAction = Action;
		{not_eaten} ->
			NewState = State,
			NewNbr = Nbr,
			NewAction = Action;
		{im_done} ->
			Animal = State#life.animal,
			case element(1, Animal) of
				herbivore ->
					Age = Animal#herbivore.age,
					MatureAge = variables:herbivoreMatureAge(),
					case Animal#herbivore.exited >= variables:herbivoreCooldownTime() of
						true when Age >= MatureAge ->
							NewExited = 0,
							case is_empty(Nbr) of
								{Pid, _} ->
									gen_server:cast(Pid, {spawn_animal, Animal});
								_ ->
									ok
							end;
						_ ->
							NewExited = Animal#herbivore.exited
					end,
					NewAnimal = Animal#herbivore{hunger=0, exited=NewExited};
				carnivore ->
					Age = Animal#carnivore.age,
					MatureAge = variables:carnivoreMatureAge(),
					case Animal#carnivore.exited >= variables:carnivoreCooldownTime() of
						true when Age >= MatureAge ->
							NewExited = 0,
							case is_empty(Nbr) of
								{Pid, _} ->
									gen_server:cast(Pid, {spawn_animal, Animal});
								_ ->
									ok
							end;
						_ ->
							NewExited = Animal#carnivore.exited
					end,
					NewAnimal = Animal#carnivore{hunger=0, exited=NewExited};
				_ ->
					NewAnimal = #empty{}
			end,
			NewState = State#life{animal=NewAnimal},
			NewNbr = Nbr,
			NewAction = Action;
		{move_here} ->
			{X,Y} = Coordinates,
			case element(1, State#life.plant) of
				plant -> 
					NewState = State#life{animal=#empty{}},
					broadcast(Nbr,plant,Coordinates),
					frame ! {change_cell, X, Y, "plant"};
				_ -> 
					NewState = #empty{},
					broadcast(Nbr,empty,Coordinates),
					frame ! {change_cell, X, Y, "empty"} 
			end,
			NewNbr = Nbr,
			NewAction = Action;
		{dont_move} -> 
			NewState = State,
			NewNbr = Nbr,
			NewAction = Action;
		{move, To} ->
			NewState = State,
			NewNbr = Nbr,
			NewAction = {goto, getAdjecentAt(Coordinates, Nbr, To)};
		{vision, Source, Direction, Range, Objects, Origin} ->
			NewState = State,
			NewNbr = Nbr,
			NewAction = Action,
			case Range>0 of
				false ->
					Origin ! {vision_re, Source, Direction, Objects};
				true ->
					Type = element(1, State),
					case Type of
						barrier ->
							Origin ! {vision_re, Source, Direction, Objects};
						empty ->
							Next = getAdjecentAt(Coordinates, Nbr, Direction),
							Next ! {vision, Source, Direction, Range-1, Objects, Origin};
						life ->
							Next = getAdjecentAt(Coordinates, Nbr, Direction),
							Plant = State#life.plant,
							PlantType = element(1, Plant),
							Animal = State#life.animal,
							AnimalType = element(1, Animal),
							case Source of
								herbivore ->
									case {PlantType, AnimalType} of
										{plant, herbivore} ->
											Next ! {vision, Source, Direction, Range-1,
												[{plant, Range},{herbivore, Range}|Objects], Origin};
										{plant, carnivore} ->
											Next ! {vision, Source, Direction, Range-1,
												[{plant, Range},{carnivore, Range}|Objects], Origin};
										{empty, herbivore} ->
											Next ! {vision, Source, Direction, Range-1,
												[{herbivore, Range}|Objects], Origin};
										{empty, carnivore} ->
											Next ! {vision, Source, Direction, Range-1,
												[{carnivore, Range}|Objects], Origin};
										_ ->
											Next ! {vision, Source, Direction, Range-1,
												[{plant, Range}|Objects], Origin}
									end;
								carnivore ->
									case AnimalType of
										herbivore ->
											Next ! {vision, Source, Direction, Range-1,
											[{herbivore, Range}|Objects], Origin};
										_ ->
											Next ! {vision, Source, Direction, Range-1,
											Objects, Origin}
									end
							end
					end
			end;
		_ -> 
			gen_server:cast(self(), tock),
			NewAction = Action,
			NewNbr = Nbr,
			NewState = State
	end,
	{noreply, [Coordinates,NewNbr,NewState,NewAction]}.

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
	lists:map(fun({C,R}) -> {list_to_atom("x" ++ integer_to_list(C) ++ "y" ++ integer_to_list(R)), empty} end ,L).

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

is_food(Nbr, Food) ->
	lists:keyfind(Food, 2, Nbr).

is_empty(Nbr) ->
	lists:keyfind(empty, 2, Nbr).

test() -> 
	driver ! {step}.

getAdjecentAt ({_X,_Y}, Nbr,Direction) ->
	case Direction of
		n  -> %% North
			%%{Next,_} = list_to_atom("x" ++ integer_to_list(X) ++ "y" ++ integer_to_list(Y-1));
			{Next,_ } = lists:nth(2, Nbr);
		ne -> %% Northeast
			%%{Next,_} = ["x", X+1, y, Y-1];
			{Next,_} = lists:nth(3, Nbr);
		e  -> %% East
			%%{Next,_} = ["x", X+1, y, Y];
			{Next,_} = lists:nth(5, Nbr);
		se -> %% Southeast
			%%{Next,_} = ["x", X+1, y, Y+1];
			{Next,_} = lists:nth(8, Nbr);
		s  -> %% South
			%%{Next,_} = ["x", X, y, Y+1];
			{Next,_} = lists:nth(7, Nbr);
		sw -> %% Southwest
			%%{Next,_} = ["x", X-1, y, Y+1];
			{Next,_} = lists:nth(6, Nbr);
		w  -> %% West
			%%{Next,_} = ["x", X-1, y, Y];
			{Next,_} = lists:nth(4, Nbr);
		_ -> %% Northwest
			%%{Next,_} = ["x", X-1, y, Y-1];
			{Next,_} = lists:nth(1, Nbr)
	end,
	Next.


lookAround (State,Nbr) ->
	Animal = State#life.animal,
	AnimalType = element(1, Animal),
	case AnimalType of
		herbivore ->
			VisionRange = Animal#herbivore.vision;
		carnivore ->
			VisionRange = Animal#carnivore.vision;
		_ ->
			VisionRange = 0
	end,
	DataCollector = spawn(?MODULE, visionDataCollector, [0, AnimalType,
														{n, -(VisionRange*VisionRange)},
														{s, VisionRange*VisionRange},
														self()]),
	element(1, lists:nth(1, Nbr)) ! {vision, AnimalType, nw, VisionRange, [], DataCollector},
	element(1, lists:nth(2, Nbr)) ! {vision, AnimalType, n, VisionRange, [], DataCollector},
	element(1, lists:nth(3, Nbr)) ! {vision, AnimalType, ne, VisionRange, [], DataCollector},
	element(1, lists:nth(4, Nbr)) ! {vision, AnimalType, w, VisionRange, [], DataCollector},
	element(1, lists:nth(5, Nbr)) ! {vision, AnimalType, e, VisionRange, [], DataCollector},
	element(1, lists:nth(6, Nbr)) ! {vision, AnimalType, sw, VisionRange, [], DataCollector},
	element(1, lists:nth(7, Nbr)) ! {vision, AnimalType, s, VisionRange, [], DataCollector},
	element(1, lists:nth(8, Nbr)) ! {vision, AnimalType, se, VisionRange, [], DataCollector}.

visionDataCollector (8, _AnimalType, {BestDir, BestVal}, {WorstDir, WorstVal}, To) ->
	case (abs(WorstVal) >= BestVal) of
		true ->
			case WorstDir of
				n ->
					To ! {move, s};
				ne ->
					To ! {move, sw};
				e ->
					To ! {move, w};
				se ->
					To ! {move, nw};
				s ->
					To ! {move, n};
				sw ->
					To ! {move, ne};
				w ->
					To ! {move, e};
				_ ->
					To ! {move, se}
			end;
		false ->
			To ! {move, BestDir}
	end;
visionDataCollector (N, AnimalType, {BestDir, BestVal}, {WorstDir, WorstVal}, To) ->
	receive
		{vision_re, _Source, Direction, Objects} ->
			case AnimalType of
				herbivore ->
					Value = herbivoreProspect(Objects, 0);
				carnivore ->
					Value = carnivoreProspect(Objects, 0);
				_ ->
					Value = 0
			end,
			case (Value > BestVal) of
				true -> 
					visionDataCollector (N+1, AnimalType, {Direction, Value}, {WorstDir, WorstVal}, To);
				false ->
					case (Value < WorstVal) of
						true ->
							visionDataCollector (N+1, AnimalType, {BestDir, BestVal}, {Direction, Value}, To);
						false ->
							visionDataCollector (N+1, AnimalType, {BestDir, BestVal}, {WorstDir, WorstVal}, To)
					end
			end
	end.

herbivoreProspect ([], Acc) -> Acc;
herbivoreProspect ([{Object, Range} | Rest], Acc) ->
	case Object of
		carnivore ->
			herbivoreProspect(Rest, (Range*(-1))+Acc);
		plant ->
			herbivoreProspect(Rest, (Range+Acc));
		_ ->
			herbivoreProspect(Rest, Acc)
	end.


carnivoreProspect ([], Acc) -> Acc;
carnivoreProspect ([{Object, Range} | Rest], Acc) ->
	case Object of
		herbivore ->
			carnivoreProspect(Rest, (Range+Acc));
		_ ->
			carnivoreProspect(Rest, Acc)
	end.


broadcast(Nbr,Type,From) ->
	{C,R} = From,
	Name = list_to_atom("x" ++ integer_to_list(C) ++ "y" ++ integer_to_list(R)),
	lists:map(fun({X,_}) -> X ! {new_type, Type, Name} end, Nbr).

generateGenetics(ParentVision, ParentStarvation) ->
	{MinVision, MaxVision} = variables:visionRange(),
	{MinStarvation, MaxStarvation} = variables:starvationRange(),

	random:seed(now()),
	TempVision = ParentVision + (random:uniform(3) - 2),
	TempStarvation = ParentStarvation + (random:uniform(3) - 2),
	if
		TempVision < MinVision ->
			Vision = MinVision;
		TempVision > MaxVision ->
			Vision = MaxVision;
		true ->
			Vision = TempVision
	end,
	if
		TempStarvation < MinStarvation ->
			Starvation = MinStarvation;
		TempStarvation > MaxStarvation ->
			Starvation = MaxStarvation;
		true ->
			Starvation = TempStarvation
	end,
	{Vision, Starvation}.

