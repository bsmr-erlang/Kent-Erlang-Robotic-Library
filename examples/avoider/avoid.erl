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
    code:add_path("../../module/"),code:add_path("../../ebin/"),
    init(rih:init( mrh:start(), 0)).

init({error, Error}) ->
    io:format("Error: ~p~n", [Error]);

%spawns the 2 required servers to run
init(Rid) ->
    spawn(?MODULE, collision, [Pid = spawn(?MODULE, travel, [Rid]), Rid]),
    Pid.

% Moves the robot until a wall is found where it will be rotated
travel(Rid) ->	
    receive
	{collision, true} ->
	    mvh:rotate(Rid, speed, 10);
	{collision,false} ->
	    mvh:move(Rid, speed, 0.5)			
    end,
    travel(Rid).


% notifies the mover process if there are any walls or not
collision(Pid, Rid) ->
    {_, Results} = dvh:read_lasers(Rid),
    is_collision(Pid, lists:min(Results)),
    timer:sleep(500),
    collision(Pid, Rid).

% a simple collision detector
is_collision(Pid, Min) when Min < 1 ->
    Pid ! {collision, true};
is_collision(Pid, _) ->
    Pid ! {collision, false}.
