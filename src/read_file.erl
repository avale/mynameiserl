-module(read_file).
-export([read/1]).

%taken from: http://stackoverflow.com/questions/2171312/the-most-efficient-way-to-read-a-file-into-a-list-of-strings

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