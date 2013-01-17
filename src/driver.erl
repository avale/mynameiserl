-module(driver).
-import(entity).
-compile(export_all). 

init() ->
	frame:init(),
	register(driver, spawn(?MODULE, loop, [[]])).


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

loop(L) ->
	receive 
		{set_up, File, W, H} -> 
			frame:set_w(W),
			frame:set_h(H),
			{ok, Binary} = file:read_file(File),
			Entities = parse_binary(Binary,W),
			spawn_game(Entities, W, H),
			driver:loop(Entities);
		{start} ->
			register(clock, spawn(?MODULE, tick, [L])),
			driver:loop(L);
		_ ->
			io:format("Driver: Undefined message~n",[])
	end,
	driver:loop(L).


%% ====================================================================
%% Functions to read the input file
%% ====================================================================

open_file(FileName, Mode) ->
    {ok, Device} = file:open(FileName, [Mode, binary]),
    Device.

close_file(Device) ->
    ok = file:close(Device).

read_lines(Device, L) ->
    case io:get_line(Device, "") of
        eof ->
            L;
        String ->
            read_lines(Device, [String|L])
    end.

read(InputFileName) ->
    Device = open_file(InputFileName, read),
    Data = read_lines(Device, ""),
    close_file(Device),
    lists:reverse(Data).


%% ====================================================================
%% Simulation tic-toc clock timer
%% ====================================================================

tick(L) ->
	io:format("DING~n",[]),
	lists:map((fun (X) -> timer:send_after(1000, X, tick) end), L),
	timer:sleep(1000),
	tock(L).


tock(L) ->
	io:format("DONG~n",[]),
	lists:map((fun (X) -> timer:send_after(1000, X, tick) end), L),
	timer:sleep(1000),
	tick(L).