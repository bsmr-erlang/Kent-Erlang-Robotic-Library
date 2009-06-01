%%%-------------------------------------------------------------------
%%% File    : ps.erl
%%% Author  : Sten Gruener
%%% Description : Test to try working with multiple robots
%%%
%%% Created :  10 Dec 2008 by Sten Gruener
%%%-------------------------------------------------------------------
-module(masterslave).
-export([master/3, slave/3, start/0]).

start() ->
	%add path to the modules
	code:add_path("../module/"),code:add_path("../ebin/"),
	%start driver
	Driver = player:start(),
	%start multi-robot module
	Pid = multi:start(),
	lists:map(fun(X) -> multi:create_robot(Pid, masterslave, slave, [Pid, Driver, X]) end, lists:seq(1,4,1)),
	multi:create_robot(Pid, masterslave, master, [Pid, Driver, 0]).


master(Dispatcherid, Driver, Playerid)->
		io:format("~p: master started~n",[self()]),
		player:init(Driver, Playerid),
		multi:barrier(Dispatcherid),
		masterLoop(Dispatcherid).

masterLoop(Dispatcherid)->
		%seed
		{A, B, C} = erlang:now(),
		random:seed(A, B, C),
		%0 or 1
		Choise = random:uniform(2),
		case Choise of
			1 ->
				%rotate
				Deg = random:uniform(360),
				multi:broadcast(Dispatcherid, {rotate, Deg}),
				%do rotate
				io:format("Master: rotating on ~p deg~n", [Deg]),
				player:rotate(degrees, Deg);
			_ ->
				%move
				Dist = 2*random:uniform(),
				%send message
				%lists:map(fun(X) -> X ! {self(), {move, Dist}} end, List),
				multi:broadcast(Dispatcherid, {move, Dist}),
				io:format("Master: moving for ~p~n", [Dist]),
				player:move(distance, Dist)
		end,

		multi:barrier(Dispatcherid),
		masterLoop(Dispatcherid).

slave(Dispatcherid, Driver, Playerid)->
		io:format("~p: slave started~n", [self()]),
		player:init(Driver, Playerid),
		multi:barrier(Dispatcherid),
		slaveLoop(Dispatcherid).

slaveLoop(Dispatcherid)->
		receive
			{_, {rotate, Deg}} ->
				io:format("Slave: rotating on ~p deg~n", [Deg]),
				player:rotate(degrees, Deg),
				multi:barrier(Dispatcherid),
				slaveLoop(Dispatcherid);
		  {_, {move, Dist}} ->
				io:format("Slave: moving for ~p~n", [Dist]),
				player:move(distance, Dist),
				multi:barrier(Dispatcherid),
				slaveLoop(Dispatcherid)
		end.

