%%% File    : traffic2.erl
%%% Author  :  <tom@fridge>
%%% Description : 
%%% Created :  1 Sep 2009 by  <tom@fridge>

-module(traffic2).


-export([start/0]).
-export([travel/2]).


start() ->
	init([1,2,3,4,5,6,7,8], [{10, 11}]).
	%init([1], [{10, 11}]).


init(Robots, Lights) ->
	TrafficServ = spawn(traffic, traffic_loop, []),
	start_crossroad(TrafficServ, Lights),
	start_robots(Robots, TrafficServ).


% spawns a new cross road
start_crossroad(_, []) -> [];
start_crossroad(TrafficServ, [{LightA, LightB}|Lights]) ->
	[spawn(traffic, crossroad, [TrafficServ, {LightA, red}, {LightB, green}])]++start_crossroad(TrafficServ, Lights).


start_robots(Robots, TrafficServ) ->
	[spawn(?MODULE, travel, [Robot, TrafficServ]) || Robot <- rih:plinit(mrh:start(), Robots, [])].
	%travel(rih:init(mrh:start(), 1), TrafficServ).
	

% initial collision detection
travel(Robot, TrafficServ) ->
	io:format("tick~n"),
	find_red_light(Robot, TrafficServ),
	collision_avoidance(Robot),
	travel(Robot, mvh:get_position(Robot), TrafficServ).

travel(Robot, LastPosition, TrafficServ) ->
	io:format("tick~n"),
	mvh:move(Robot, speed, 0.2),
	timer:sleep(50),
	case distance(Robot, LastPosition) of 
		% dont really need to do anything if the robot has not moved
		{stopped, _} ->
			travel(Robot, LastPosition, TrafficServ);
		% This would be a good time to update the laser results
		{reset, NewPosition} ->
			collision_avoidance(Robot),
			find_red_light(Robot, TrafficServ),
			% The angle of the robot is not needed so no need to update position at this point
			travel(Robot, NewPosition, TrafficServ);
		% Keep moving until robot has moved to destination
		{continue,_} ->
			travel(Robot, LastPosition, TrafficServ)
	end.
		
find_red_light(Robot, TrafficServ) ->
	case traffic:find_red_light(TrafficServ, dvh:read_fiducial(Robot)) of
		stop ->
			mvh:move(Robot, speed, 0),
			timer:sleep(150),
			find_red_light(Robot, TrafficServ);
		go ->
			skip
	end.


% This is used to decide if the sensors need updating
distance(Robot, {X, Y, A}) ->
	{NewX, NewY, NewA} = mvh:get_position(Robot),
	case calc_hyp(NewX - X, NewY - Y) of
		0 ->
			{stopped, {X, Y, A}};
		Any when Any > 1 ->
			{reset, {NewX, NewY, NewA}};
		_ ->
			{continue, {X, Y, A}}
	end.


% calculates the hypotenous from the two sides
calc_hyp(X, Y) ->
	math:sqrt((X*X)+(Y*Y)).


% Detect walls
collision_avoidance(Robot) ->
	case see_robot(dvh:read_fiducial(Robot)) of
		% avoid rotating when behind a robot
		true ->
			mvh:move(Robot, speed, 0),
			timer:sleep(50),
			collision_avoidance(Robot);
		false ->
			better_detection(Robot)
	end.


% determines if any robots are close
see_robot([]) ->
	false;
% see robot, calculate distance
see_robot([{1, {X, Y, _}, _, _, _}|Rest]) ->
	case calc_hyp(X, Y) of
		%  smaller numbers, fidicual results are not in meters
		D when D < 2 ->
			io:format("spotted it ~p~n", [D]),
			true;
		_ ->
			see_robot(Rest)
	end;
% ignore beacons
see_robot([_|Rest]) ->
	see_robot(Rest).


% Simple with only a single direction
dumb_detection(Robot) ->
	{_, Results} = dvh:read_lasers(Robot),
	case lists:min(lists:sublist(Results, 110, 140)) of
		D when D < 1 ->
			io:format("wall ~p~n", [D]),
			mvh:rotate(Robot, speed, 10),
			timer:sleep(25),
			collision_avoidance(Robot);
		_ ->
			skip
	end.
				   
better_detection(Robot) ->
	case lists:min(fix_results(dvh:read_lasers(Robot))) of
		{Distance, Angle} when Distance < 1.4 ->
			io:format("wall - ~p, > ~p~n", [Distance, Angle]),
			if 
				Angle > 0 ->
					mvh:rotate(Robot, speed, -10);
				true ->
					mvh:rotate(Robot, speed, 10)
			end,
			timer:sleep(25),
			collision_avoidance(Robot);
		_ ->
			skip
	end.		


% shifts the laser results slightly
fix_results({[],_}) ->
	[];
fix_results({[B|Bearings], [R|Results]}) ->
	[{R+erlang:abs(B), B}]++fix_results({Bearings, Results}).
