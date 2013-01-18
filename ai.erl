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
	(lists:nth(1, Nbr)) ! {vision, AnimalType, nw, VisionRange, [], DataCollector},
	(lists:nth(2, Nbr)) ! {vision, AnimalType, n, VisionRange, [], DataCollector},
	(lists:nth(3, Nbr)) ! {vision, AnimalType, ne, VisionRange, [], DataCollector},
	(lists:nth(4, Nbr)) ! {vision, AnimalType, w, VisionRange, [], DataCollector},
	(lists:nth(5, Nbr)) ! {vision, AnimalType, e, VisionRange, [], DataCollector},
	(lists:nth(6, Nbr)) ! {vision, AnimalType, sw, VisionRange, [], DataCollector},
	(lists:nth(7, Nbr)) ! {vision, AnimalType, s, VisionRange, [], DataCollector},
	(lists:nth(8, Nbr)) ! {vision, AnimalType, se, VisionRange, [], DataCollector},
	receive
		{choose_dir, Dir} -> ok %% FLYTTA FANSKAPET HITÅT
	end.

visionDataCollector (8, AnimalType, {BestDir, BestVal}, {WorstDir, WorstVal}, To) ->
	case (WorstVal >= BestVal) of
		true ->
			case WorstDir of
				n ->
					To ! {choose_dir, s};
				ne ->
					To ! {choose_dir, sw};
				e ->
					To ! {choose_dir, w};
				se ->
					To ! {choose_dir, nw};
				s ->
					To ! {choose_dir, n};
				sw ->
					To ! {choose_dir, ne};
				w ->
					To ! {choose_dir, e};
				_ ->
					To ! {choose_dir, se}
			end;
		false ->
			To ! {choose_dir, BestDir}
	end;
visionDataCollector (n, AnimalType, {BestDir, BestVal}, {WorstDir, WorstVal}, To) ->
	receive
		{vision_re, Source, Direction, Objects} ->
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
					visionDataCollector (n-1, AnimalType, {Direction, Value}, {WorstDir, WorstVal}, To);
				false ->
					case (Value < WorstVal) of
						true ->
							visionDataCollector (n-1, AnimalType, {BestDir, BestVal}, {Direction, Value}, To);
						false ->
							visionDataCollector (n-1, AnimalType, {BestDir, BestVal}, {WorstDir, WorstVal}, To)
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
	
