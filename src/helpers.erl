-module(helpers).
-compile(export_all).

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

getAdjecentAt ({X,Y}, Nbr,Direction) ->
	case Direction of
		n  -> %% North
			%%Next = list_to_atom("x" ++ integer_to_list(X) ++ "y" ++ integer_to_list(Y-1));
			Next = lists:nth(2, Nbr);
		ne -> %% Northeast
			%%Next = ["x", X+1, y, Y-1];
			Next = lists:nth(3, Nbr);
		e  -> %% East
			%%Next = ["x", X+1, y, Y];
			Next = lists:nth(5, Nbr);
		se -> %% Southeast
			%%Next = ["x", X+1, y, Y+1];
			Next = lists:nth(8, Nbr);
		s  -> %% South
			%%Next = ["x", X, y, Y+1];
			Next = lists:nth(7, Nbr);
		sw -> %% Southwest
			%%Next = ["x", X-1, y, Y+1];
			Next = lists:nth(6, Nbr);
		w  -> %% West
			%%Next = ["x", X-1, y, Y];
			Next = lists:nth(4, Nbr);
		_ -> %% Northwest
			%%Next = ["x", X-1, y, Y-1];
			Next = lists:nth(1, Nbr)
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
	(lists:nth(1, Nbr)) ! {vision, AnimalType, nw, VisionRange, [], DataCollector},
	(lists:nth(2, Nbr)) ! {vision, AnimalType, n, VisionRange, [], DataCollector},
	(lists:nth(3, Nbr)) ! {vision, AnimalType, ne, VisionRange, [], DataCollector},
	(lists:nth(4, Nbr)) ! {vision, AnimalType, w, VisionRange, [], DataCollector},
	(lists:nth(5, Nbr)) ! {vision, AnimalType, e, VisionRange, [], DataCollector},
	(lists:nth(6, Nbr)) ! {vision, AnimalType, sw, VisionRange, [], DataCollector},
	(lists:nth(7, Nbr)) ! {vision, AnimalType, s, VisionRange, [], DataCollector},
	(lists:nth(8, Nbr)) ! {vision, AnimalType, se, VisionRange, [], DataCollector}.

visionDataCollector (8, AnimalType, {BestDir, BestVal}, {WorstDir, WorstVal}, To) ->
	case (WorstVal >= BestVal) of
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
					visionDataCollector (N-1, AnimalType, {Direction, Value}, {WorstDir, WorstVal}, To);
				false ->
					case (Value < WorstVal) of
						true ->
							visionDataCollector (N-1, AnimalType, {BestDir, BestVal}, {Direction, Value}, To);
						false ->
							visionDataCollector (N-1, AnimalType, {BestDir, BestVal}, {WorstDir, WorstVal}, To)
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