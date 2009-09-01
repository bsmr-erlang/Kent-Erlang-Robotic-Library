%%% File    : traffic.erl
%%% Author  :  <tom@fridge>
%%% Description : 
%%% Created : 14 Aug 2009 by  <tom@fridge>

-module(traffic).

-export([start/0]).
-export([crossroad/3, traffic_loop/0, robot/3]).

-export([find_red_light/2]).

start() ->
	% alias erl="erl +A24 -pa ../../ebin/" >> ~/.bashrc # SEE README.txt
	init([1,2,3,4,5,6,7,8], [{10, 11}]).


init(Robots, Lights) ->
	put(robot_serv, mrh:start()),
	put(traffic_serv, spawn(?MODULE, traffic_loop, [])),
	get(traffic_serv) ! {light, {1, red}},
	start_crossroad(Lights),
	start_robots(Robots).


start_robots([]) -> [];
start_robots([Robot| Robots]) ->
	T = get(traffic_serv),
	D = get(robot_serv),
	[spawn(?MODULE, robot, [T, D, Robot])] ++ start_robots(Robots).


robot(TrafficServ, RobotServ, Rid) ->
	io:format("starting robot ~b~n", [Rid]),
	robot(TrafficServ, rih:init(RobotServ, Rid)).

robot(TrafficServ, Rid) ->
	case find_red_light(TrafficServ, dvh:read_fiducial(Rid)) of
		stop ->
			mvh:move(Rid, speed, 0);
		go ->
			avoid_walls(Rid),
			mvh:move(Rid, speed, 0.2)
	end,
	timer:sleep(50),
	robot(TrafficServ, Rid).
	

avoid_walls(Rid) ->
	{_, Results} = dvh:read_lasers(Rid),
	case choose_direction(lists:min(lists:sublist(Results, 90, 90)),
						  lists:min(lists:sublist(Results, 180, 90)),
						 lists:max(lists:sublist(Results, 1, 180)),
						  lists:max(lists:sublist(Results, 180, 180))) of
		{true, Speed} ->
			mvh:rotate(Rid, speed, Speed),
			timer:sleep(50),
			avoid_walls(Rid);
		_ ->
			ok
	end.

choose_direction(Left, _, _, FarRight) when Left < 0.65, FarRight > 2 ->
	{true, 10};
choose_direction(_, Right, FarLeft, _) when Right < 0.65, FarLeft > 2 ->
	{true, -10};
choose_direction(_,_,_,_) ->
	false.

find_red_light(_,[]) ->
	go;

find_red_light(_, [{1, {X, Y, _},_,_,_}|_]) when abs(X) < 0.6, 
abs(Y) < 0.6 ->

	stop;
find_red_light(TrafficServ, [{Id, {X, Y, _},_,_,_}|More]) when 
abs(X) < 1.5, abs(Y) < 1.5, Id > 9 ->
	TrafficServ ! {colour, self(), Id},
	receive
		red ->
			stop;
		_ ->
			find_red_light(TrafficServ, More)
	end;
find_red_light(TrafficServ, [_|More]) ->
	find_red_light(TrafficServ, More).
					  


% stores traffic signal 
traffic_loop() ->
	receive 
		{light, {Name, Colour}} ->
			put(Name, Colour);
		{colour, Pid, Name} ->
			Pid ! get(Name)
	end,
	traffic_loop().


% spawns a new cross road
start_crossroad([]) -> [];
start_crossroad([{LightA, LightB}|Lights]) ->
	T = get(traffic_serv),
	[spawn(?MODULE, crossroad, [T, {LightA, red}, {LightB, green}])]++start_crossroad(Lights).

%swap, red light first
crossroad(TrafficServ, {LightA, green}, {LightB, red}) ->
	crossroad(TrafficServ, {LightB, red}, {LightA, green});
	
% controls a pair of traffic lights
crossroad(TrafficServ, LightA, LightB) ->
	TrafficServ ! {light, switch(LightA)},
	timer:sleep(3000), % amber (allow robots to finish crossing)
	TrafficServ ! {light, switch(LightB)},
	timer:sleep(20000),
	io:format("switched lightes~n"),
	crossroad(TrafficServ, switch(LightB), switch(LightA)).
		   

%switches the traffic light colours
switch({Name, green}) ->
	{Name, red};
switch({Name, red}) ->
	{Name, green}.
	
