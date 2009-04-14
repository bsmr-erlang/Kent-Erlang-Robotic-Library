%%% File    : avoid.erl
%%% Author  :  Thomas Lorentsen
%%% Description : Moves around avoiding walls
%%%               This moves a robot around a map avoiding walls,
%%%               Make this work better.
%%% Created : 14 Apr 2009 by Thomas Lorentsen

-module(avoid).

-export([demo/0, travel/1, collision/2]).

%starts up a simple demo of 1 robot
demo() ->
	%adjust these to the correct path if needed
	code:add_path("../../module/"),code:add_path("../../ebin/"),
	init(rih:init( mrh:start(), 0)).


init({error, Error}) ->
	io:format("Error: ~p~n", [Error]);

%spawns the 2 required servers to run
init(Rid) ->
	Pid = spawn(?MODULE, travel, [Rid]),
	spawn(?MODULE, collision, [Pid, Rid]),
	Pid.


% Moves the robot until a wall is found
travel(Rid) ->
	
	receive
		{collision, true} ->
			rotate(Rid);
		{collision,false} ->
			mvh:move(Rid, speed, 0.5),
			travel(Rid)
	end.

% rotates the robot until there is no wall
rotate(Rid) ->
	receive
		{collision, true} ->
			mvh:rotate(Rid, speed, 10),
			rotate(Rid);
		{collision,false} ->
			travel(Rid)
	end.


% notifies the mover process if there are any walls or not
collision(Pid, Rid) ->
	{_, Res} = dvh:read_lasers(Rid),
	is_collision(Pid, lists:min(Res)),
	receive _ -> nothing after 1000 -> nothing end,
	collision(Pid, Rid).

% a simple collision detector
is_collision(Pid, Min) when Min < 2 ->
	Pid ! {collision, true};
is_collision(Pid, _) ->
	Pid ! {collision, false}.
