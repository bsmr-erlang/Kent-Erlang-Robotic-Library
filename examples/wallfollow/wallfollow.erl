%%% File    : wallfollow.erl
%%% Author  :  Thomas Lorentsen, Sten Gruener
%%% Description : Moves around avoiding walls
%%%               This moves a robot around a map avoiding walls,
%%%               Make this work better.
%%% Created : 14 Apr 2009 by Thomas Lorentsen

-module(wallfollow).

-export([start/0, travel/1, collision/3]).

% runs simple wall avoider with a single robot
start() ->
    code:add_path("../../module/"),code:add_path("../../ebin/"),
    init(rih:init( mrh:start(), 0)).

init({error, Error}) ->
    io:format("Error: ~p~n", [Error]);

% Spawns the collision and movement processes
init(Rid) ->
    spawn(?MODULE, collision, [Pid = spawn(?MODULE, travel, [Rid]), Rid, stop]),
    Pid.

% Controls the robots movements
travel(Rid) ->	
    receive
	{collision, true} ->
	    mvh:rotate(Rid, speed, 10);
	{collision,false} ->
	    mvh:move(Rid, speed, 0.5)			
    end,
    travel(Rid).


% Reads the lasers and checks for collisions
collision(Pid, Rid, State) ->
    {_, Results} = dvh:results(Rid, lasers),
    NewState = is_collision(Pid, lists:min(lists:sublist(Results, 90, 180)), State),
    timer:sleep(50),
    collision(Pid, Rid, NewState).

% A simple collision detector using the smallest laser result
is_collision(Pid, Min, move) when Min < 1 ->
    Pid ! {collision, true},
	stop;
is_collision(Pid, _, stop) ->
    Pid ! {collision, false},
	move;
is_collision(_,_,State) ->
	State.
