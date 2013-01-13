-module(driver).
-compile(export_all). 

init() ->
	frame:init(),
	register(driver, spawn(?MODULE, loop, [])).


parse_binary_inner(<<>>,Ack,_,_) -> Ack;
parse_binary_inner(<<Byte:8, Rest/binary>>, Ack, C, Y) ->
	case Byte of
		10 -> parse_binary_aux(Rest, Ack, X+1, Y);
		84 -> parse_binary_aux(Rest, [{X,Y,1}|Ack], X+1, Y);
		72 -> parse_binary_aux(Rest, [{X,Y,2}|Ack], X+1, Y);
		67 -> parse_binary_aux(Rest, [{X,Y,3}|Ack], X+1, Y);
		101 -> parse_binary_aux(Rest, [{X,Y,0}|Ack], X+1, Y);
		66 -> parse_binary_aux(Rest, [{X,Y,-1}|Ack], X+1, Y)
	end.

parse_binary_outer(<<>>, Ack, _, _) -> Ack;
parse_binary_outer(<<Line:(8*W+8), Rest/binary>>, Ack, Y, W) ->
	parse_binary_outer(Rest, parse_binary_inner(Line, Ack, 0, Y), Y+1, W).

parse_binary(Binary,W) ->
	parse_binary_outer(Binary,[],0,W).

spawn_game_inner([{X,Y,C}|T], Size, X, Y) ->
	spawn_game_inner(T,Size-1,X-1,Y).

spawn_game_outer(Entities, Size, X, Y) ->
	spawn_game_inner(Entities, Size-1, X, Y).

spawn_game(Entities, W, H) ->
	spawn_game_outer(Entities, W*H, W, H);

loop() ->
	receive 
		{start, File, W, H} -> 
			{ok, Binary} = file:read_file(File),
			Entities = parse_binary(Binary,W),
			spawn_game(Entities, W, H);
		_ ->
			io:format("Driver: Undefined message~n",[])
	end,
	driver:loop().


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