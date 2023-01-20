%%% @author c50 <joq62@c50>
%%% @copyright (C) 2023, c50
%%% @doc
%%%
%%% @end
%%% Created : 20 Jan 2023 by c50 <joq62@c50>

-module(pod).

-define(SERVER,pod_node_server).

-export([
	 ping/0
	
	]).

-export([
	 start/0,
	 stop/0
	]).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
	    
%% call
start()-> gen_server:start_link({local, ?SERVER}, ?SERVER, [], []).
stop()-> gen_server:call(?SERVER, {stop},infinity).



%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
ping() ->
    gen_server:call(?SERVER, {ping}).

