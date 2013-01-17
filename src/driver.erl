-module(driver).
-import(entity).
-compile(export_all).
 
init() ->
        frame:init(),
        register(driver, spawn(?MODULE, loop, [[],[],0,0])).
 
 
parse_binary_inner(<<>>,Ack,_,_) -> Ack;
parse_binary_inner(<<Byte:8, Rest/binary>>, Ack, X, Y) ->
        case Byte of
                10 -> parse_binary_inner(Rest, Ack, X+1, Y);
                84 -> parse_binary_inner(Rest, [{X,Y,1}|Ack], X+1, Y);
                72 -> parse_binary_inner(Rest, [{X,Y,2}|Ack], X+1, Y);
                67 -> parse_binary_inner(Rest, [{X,Y,3}|Ack], X+1, Y);
                101 -> parse_binary_inner(Rest, [{X,Y,0}|Ack], X+1, Y);
                35 -> parse_binary_inner(Rest, [{X,Y,-1}|Ack], X+1, Y);
                _ -> parse_binary_inner(Rest, Ack, X+1, Y)
        end.
 
parse_binary_outer(<<>>, Ack, _, _) -> Ack;
parse_binary_outer(Board, Ack, Y, W) ->
        Board_width = 8*(W+1),
        <<Line:Board_width, Rest/binary>> = Board,
        parse_binary_outer(Rest, parse_binary_inner(binary:encode_unsigned(Line), Ack, 0, Y), Y+1, W).
 
parse_binary(Binary,W) ->
        parse_binary_outer(Binary,[],0,W).
 
spawn_game([], _W, _H) -> ok;
spawn_game([{Xe,Ye,C}|T], W, H) ->
        entity:start_cell({Xe,Ye},W,H,C),
        spawn_game(T,W,H).
 
to_pid(L) ->
        lists:map((fun({X,Y,_}) -> whereis(list_to_atom("x" ++ integer_to_list(X) ++ "y" ++ integer_to_list(Y))) end), L).
 
loop(P, E, W, H) ->
        receive
                {set_up, File, Wn, Hn} ->
                        frame:set_w(Wn),
                        frame:set_h(Hn),
                        {ok, Binary} = file:read_file(File),
                        Entities = parse_binary(Binary,Wn),
                        spawn_game(Entities, Wn, Hn),
                        driver:loop(to_pid(Entities), Entities, Wn, Hn);
                {restart} ->
                        spawn_game(E, W, H),
                        register(clock, spawn(?MODULE, tick, [P])),
                        driver:loop(to_pid(E),E,W,H);
                {reset} ->
                        driver:loop([],[],0,0);
                {start} ->
                        register(clock, spawn(?MODULE, tick, [P])),
                        driver:loop(P,E,W,H);
                {stop} ->
                        exit(whereis(clock), kill),
                        lists:map(fun(X) -> exit(X, kill) end, P),
                        driver:loop(P,E,W,H);
                _ ->
                        io:format("Driver: Undefined message~n",[])
        end,
        driver:loop(P,E,W,H).
 
%% ====================================================================
%% Simulation tic-toc clock timer
%% ====================================================================
 
tick(L) ->
        io:format("DING~n",[]),
        lists:map((fun (X) -> timer:send_after(1000, X, {tick}) end), L),
        timer:sleep(1000),
        tock(L).
 
 
tock(L) ->
        io:format("DONG~n",[]),
        lists:map((fun (X) -> timer:send_after(1000, X, {tock}) end), L),
        timer:sleep(1000),
        tick(L).