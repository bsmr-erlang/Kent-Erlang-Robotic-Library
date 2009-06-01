%%%-------------------------------------------------------------------
%%% File    : snake.erl
%%% Author  : Sten Gruener
%%% Description : Snake example
%%%
%%% Created :  11 Feb 2008 by Sten Gruener
%%%-------------------------------------------------------------------
-module(snake).
-export([tail/5, head/6, start/0]).

start() ->
	%======EXAMPLE`S CONFIG======
	Speed = 0.50, %movement speed in m/s
	Rotation = 14, %rotation speed of the head in deg/s ~ agility of the snake
	Timeout = 1800, %timeout before posible direction change in msec
	Distance = 0.7, %distance between snakes members
	%=======END OF CONFIG========
	%add path to the modules
	code:add_path("../module/"),code:add_path("../ebin/"),
	%start driver
	Driver = player:start(),
	%start multi-robot module
	Pid = multi:start(),
	%1st snake
	multi:create_robot(Pid, snake, head, [Pid, Driver, 0, Speed, Rotation, Timeout]),
	lists:map(fun(X) -> multi:create_robot(Pid, snake, tail, [Pid, Driver, X, Speed, Distance]) end, lists:seq(1,6,1)),
	%2nd snake
	multi:create_robot(Pid, snake, head, [Pid, Driver, 16, Speed, Rotation, Timeout]),
	lists:map(fun(X) -> multi:create_robot(Pid, snake, tail, [Pid, Driver, X, Speed, Distance]) end, lists:seq(10,15,1)),
	ok.

head(Dispatcherid, Driver, Playerid, Speed, Rotation, Timeout) ->
	%seed the random number generator
	{A, B, C} = erlang:now(),
	random:seed(A, B, C),
	%init robot
	player:init(Driver, Playerid),
	%sync
	multi:barrier(Dispatcherid),
	player:move(full, {Speed, 0}),
	headloop(Speed, Rotation, Timeout).

headloop(Speed, Rotation, Timeout) ->
	%read lasers
	{_, Lasers} = player:results(lasers),
	%localize the closest obbstacle
	Closest = lists:min(Lasers),
	%8.0 is not seeing anything
	case Closest > 7.92 of
		true ->
			%+1, 0 or -1
			Random = 2 - random:uniform(3),
			player:move(full, {Speed, Rotation/2 * Random}),
			%move a period
			timer:sleep(Timeout);
		_ ->
			Angle = trunc(dvh:find_value(lists:min(Lasers), Lasers)/2),
			case Angle < 90 of
				true -> 
					Random = 1;
				false ->
					Random = -1
			end,
			player:move(full, {Speed, Rotation * Random})
		end,
	headloop(Speed, Rotation, Timeout).

tail(Dispatcherid, Driver, Playerid, Speed, Distance) ->
	%init robot
	player:init(Driver, Playerid),
	%sync
	multi:barrier(Dispatcherid),
	%start at zero
	player:move(full, {0, 0}),
	tailloop(Speed, Distance).

tailloop(Speed, Distance) ->
	%read lasers
	{_, Lasers} = player:results(lasers),
	if Lasers =/= [] ->
		%localize the closest obbstacle
		Closest = lists:min(Lasers),
		Angle = trunc(dvh:find_value(lists:min(Lasers), Lasers)/2),
		%normalize Angle (NormAngle is \in [-90, +90]
		NormAngle = Angle - 90,
                %count rotation + some accelerated rotation if difference is big
                Rotation = NormAngle + erlang:abs(NormAngle) * 0.12,
                %normalize distance
                NormDistance = Closest - Distance,
                %some acceleration
                Acceleration = Speed * NormDistance * 1.05,
                %set the movement
                player:move(full, {Speed+Acceleration, Rotation})
	end,
	%busy loop
	tailloop(Speed, Distance).

