-module(test).
-compile(export_all).

construct_neigh({X,Y}, Max_X, Max_Y) ->
	L = [{Xn, Yn} || 
			Yn <- [Y-1, Y, Y+1], Xn <- [X-1, X, X+1], 
			Xn >= 0, Xn < Max_X, Yn >=0, Yn < Max_Y, {Xn, Yn} =/= {X,Y}],
	lists:map(fun({C,R}) -> 
			list_to_atom("x" ++ integer_to_list(C) ++ "y" ++ integer_to_list(R)) end
			, L).