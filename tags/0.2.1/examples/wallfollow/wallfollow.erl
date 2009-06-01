%%% File    : avoid.erl
%%% Author  :  Thomas Lorentsen
%%% Description : Moves around avoiding walls
%%%               This moves a robot around a map avoiding walls,
%%%               Make this work better.
%%% Created : 14 Apr 2009 by Thomas Lorentsen

-module(wallfollow).

-export([start/0, travel/1, collision/2]).

% runs simple wall avoider with a single robot
start() ->
    code:add_path("../../module/"),code:add_path("../../ebin/"),
    init(rih:init( mrh:start(), 0)).

init({error, Error}) ->
    io:format("Error: ~p~n", [Error]);

% Spawns the collision and movement processes
init(Rid) ->
    spawn(?MODULE, collision, [Pid = spawn(?MODULE, travel, [Rid]), Rid]),
    Pid.

% Controls the robots movements
travel(Rid) ->	
    receive
	{collision, true} ->
	    mvh:rotate(Rid, speed, 10);
	{collision,false} ->
	    mvh:move(Rid, speed, 0.4)			
    end,
    travel(Rid).


% Reads the lasers and checks for collisions
collision(Pid, Rid) ->
    {_, Results} = dvh:read_lasers(Rid),
    is_collision(Pid, lists:min(lists:sublist(Results, 100, 160))),
    timer:sleep(500),
    collision(Pid, Rid).

% A simple collision detector using the smallest laser result
is_collision(Pid, Min) when Min < 1.2 ->
    Pid ! {collision, true};
is_collision(Pid, _) ->
    Pid ! {collision, false}.
