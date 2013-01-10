-module(driver).
-compile(export_all). 

init() ->
	frame:init(),
	register(driver, spawn(?MODULE, loop, [])).


loop() ->
	receive 
		_ -> 
			io:format("Jak ar en gnu ~n")
	end,
	driver:loop().