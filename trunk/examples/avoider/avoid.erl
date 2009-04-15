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
		{collision, true, _} ->
			rotate(Rid);
		{collision,false} ->
			mvh:move(Rid, speed, 0.5),
			travel(Rid)
	end.

% rotates the robot until there is no wall
rotate(Rid) ->
	receive
		{collision, true, Direction} ->
			mvh:rotate(Rid, speed, 10/Direction),
			rotate(Rid);
		{collision,false} ->
			travel(Rid)
	end.


% notifies the mover process if there are any walls or not
collision(Pid, Rid) ->
	{Left, Right} = split_lasers( dvh:read_lasers(Rid)),
	is_collision(Pid, lists:min(lists:sublist(lists:reverse(Left),90)), lists:min(lists:sublist(Right,90))),
	receive _ -> nothing after 500 -> nothing end,
	collision(Pid, Rid).

split_lasers({_, Res}) ->
	lists:split(erlang:trunc( string:len(Res)/2),Res).


% a simple collision detector
is_collision(Pid, LeftMin, _) when LeftMin < 1 ->
	Pid ! {collision, true, 1};
is_collision(Pid, _, RightMin) when RightMin < 1 ->
	Pid ! {collision, true, -1};
is_collision(Pid, _, _) ->
	Pid ! {collision, false}.
