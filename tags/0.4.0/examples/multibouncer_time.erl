%%%-------------------------------------------------------------------
%%% File    : multibouncer_time.erl
%%% Author  : Sten Gruener
%%% Description : This is the first naive approach, to create a multibouncer.
%%%		Since we rely on time syncronisation the robots will not stay
%%%		in gorup and collide.
%%%
%%% Created :  13 Jan 2009 by Sten Gruener
%%%-------------------------------------------------------------------

-module(multibouncer_time).
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
				slave(Speed)
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
					master(Dispatcherid, Results, Speed)
				end
  end.

%I am a master for this turn
master(Dispatcherid, Results, Speed) ->
	%find out the side
	Side = bouncer:quickest_rotate(Results),
	%tell it to slaves
	multi:broadcast(Dispatcherid, {rotate, Side}),
	%rotate
	bouncer:avoid_wall(Side, Speed),
	%tell everybody to stop
	multi:broadcast(Dispatcherid, {stop}).	

%I am a slave for this turn
slave(Speed) ->
	%receive rotation
	receive
		{_, {rotate, Side}} ->
			%rotate like master
			bouncer:rotate(Side, Speed)
	end,
	%stop
	receive
		{_, {stop}} ->
			%rotate like master
			player:move(speed, 0)
	end.
