%%%-------------------------------------------------------------------
%%% File    : multibouncer_diff.erl
%%% Author  : Sten Gruener
%%% Description : This is the second version of multibouncer which uses
%%%		differential positioning. Unfortunatley there are no
%%%		votings, so sometimes 2 robots pretend to be the master
%%%		and the system fails.
%%%
%%% Created :  13 Jan 2009 by Sten Gruener
%%%-------------------------------------------------------------------

-module(multibouncer_diff).
-export([start/0, thread/3]).

start() ->
	%add path to the modules
	code:add_path("../module/"),code:add_path("../ebin/"),
	%start driver
	Driver = player:start(),
	%start multi-robot module
	Pid = multi:start(),
	%create 4 bouncers
	lists:map(fun(X) -> multi:create_robot(Pid, ?MODULE, thread, [Pid, Driver, X]) end, lists:seq(0,3,1)),
	ok.

thread(Dispatcherid, Driver, Playerid) ->
	%init robot
	player:init(Driver, Playerid),
	%======EXAMPLE`S CONFIG======
	Speed = 0.35,
	%=======END OF CONFIG========
	loop(Dispatcherid, Speed).

loop(Dispatcherid, Speed) ->
	%barrier
	multi:barrier(Dispatcherid),
	%save the initial position
	memo:save("initial_position", player:get_position()),
	%run the main function
	travel(Dispatcherid, Speed),
	%infinite loop
	loop(Dispatcherid, Speed).

% keeps traveling until it finds a wall
travel(Dispatcherid, Speed) ->
	receive
		{_, {stop}} ->
				%stop
				player:move(speed, 0),
				%Do the slave part
				slave()
		%wait 40 ms
		after 40 ->
				{_, Results} = player:results(lasers),
				case bouncer:is_close(bouncer:sublasers(center, Results)) of
				%no obstacle found -> keep moving
				false ->
					player:move(speed, Speed),
					travel(Dispatcherid, Speed);
				%I see an obstacle!
				true ->
					%alert companions
					multi:broadcast(Dispatcherid, {stop}),
					%stop
					player:move(speed, 0),
					%I am master
					%Note: unfortunately more then one robot can be here at the same time
					%      in order to solve it we need to use votings -> masterslave.erl
					master(Dispatcherid, Speed)
				end
  end.

%I am a master for this turn
master(Dispatcherid, Speed) ->
	%I have located the obstacle - avoid_wall
	bouncer:avoid_wall(Speed),
	%get orientation
	{X, Y, A} = player:get_position(),
	%get initial position
	{InitX, InitY, InitA} = memo:read("initial_position"),
	%broadcast the difference
	multi:broadcast(Dispatcherid, {correction, {X-InitX, Y-InitY, A-InitA}}).

%I am a slave for this turn
slave() ->
	%correct the position
	receive
		{_, {correction, {MasterDiffX, MasterDiffY, MasterDiffA}}} ->
			%my position now
			{X, Y, A} = player:get_position(),
			%get initial position
			{InitX, InitY, InitA} = memo:read("initial_position"),
			%Compute own differences
			{DiffX, DiffY, DiffA} = {X-InitX, Y-InitY, A-InitA},
			%correct the 'difference' between differences
			player:move(difference, {MasterDiffX-DiffX, MasterDiffY-DiffY, MasterDiffA-DiffA})
	end.
