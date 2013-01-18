-module(ai).

%% Source = carnivore eller herbivore
%% Direction = n, ne, e, se, s, sw, w, eller nw
%% Range = int
%% Objects = [{plant, 2}, {herbivore, 4}, etc]
%% Origin = self() av första sändaren

addToEntityReceiveLoop () ->
	receive

		{vision, Source, Direction, Range, Objects, Origin} ->
			Next = getAdjecentAt(Coordinates, C, R, Direction),
			case Range>0 of
				true ->
					case Type of %% Obs! Rätt syntax?
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
			end

	end.

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

lookAround () ->
	(lists:nth(1, Nbr)



	Neighbours =
		{getAdjecentAt(Coordinates, Max_X, Max_Y, n), n} ++
		{getAdjecentAt(Coordinates, Max_X, Max_Y, ne), ne} ++
		{getAdjecentAt(Coordinates, Max_X, Max_Y, e), e} ++
		{getAdjecentAt(Coordinates, Max_X, Max_Y, se), se} ++
		{getAdjecentAt(Coordinates, Max_X, Max_Y, s), s} ++
		{getAdjecentAt(Coordinates, Max_X, Max_Y, sw), sw} ++
		{getAdjecentAt(Coordinates, Max_X, Max_Y, w), w} ++
		{getAdjecentAt(Coordinates, Max_X, Max_Y, nw), nw},
	lists:foreach(fun({PID, Direction}) -> N ! (vision, Source, Direction, Range, [], self()), Neighbours),
	Collector = spawn(?MODULE, visionDataCollector, []).

visionDataCollector (N, NE, E, SE, S, SW, W, NW) -> ok.
	receive
		{vision_re, Source, n, Objects} -> ok;
		{vision_re, Source, ne, Objects} -> ok;