-module(entity).
-compile(export_all).

-behaviour(gen_server).


start_cell() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% ====================================================================
%% callback functions
%% ====================================================================

init([]) ->
	{ok, [[],[]]}.

handle_call(_, _From, State) ->
	{reply, ok, State}.

handle_cast(_, State) ->
	{noreply, State}.

handle_info(Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
