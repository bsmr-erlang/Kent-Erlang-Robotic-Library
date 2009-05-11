%%% File    : simple.erl
%%% Author  :  Thomas Lorentsen
%%% Description : Just a simple example module to init several robots
%%% Created : 12 Feb 2009 by  Thomas Lorentsen

-module(simple).


-export([start/0, start/1, loop/1]).

start() ->
	start("localhost").

start(Host) ->
	code:add_path("../module/"),code:add_path("../ebin/"),
	% init robots in parallel
	init(rih:plinit(mrh:start(), lists:seq(0,6,1), [{host, Host}])).

init([]) ->
	ok;

init([Rid|M]) ->
	spawn(?MODULE, loop, [Rid]),
	init(M).

loop(Rid) ->
	loop(Rid).
