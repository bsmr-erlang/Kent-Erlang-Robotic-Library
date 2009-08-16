%%%-------------------------------------------------------------------
%%% File    : bouncer.erl
%%% Author  : Sten Gruener, Tom Lorentsen
%%% Description : Example for a single bouncing robot
%%%
%%% Created :  09 Jan 2009 by Sten Gruener
%%%-------------------------------------------------------------------
-module(bouncer).
-export([start/0, is_close/1, avoid_wall/1, avoid_wall/2, sublasers/2, quickest_rotate/1, rotate/2]).

start() ->
	%add path to the modules
	code:add_path("../module/"),code:add_path("../ebin/"),
	%start server
	Server = player:start(),
	%init robot
	player:init(Server, "localhost", 6665, 1),
	loop().

loop() ->
	%CONFIG: speed in m/s
	Speed = 1,
   	move_to_wall(Speed),
	avoid_wall(Speed),
	loop().

% Rotates with a given direction and speed until there is no wall blocking
avoid_wall(Direction, Speed) ->	
	timer:sleep(100),
	{_, Lasers} = player:results(lasers),
	case is_close(sublasers(Direction,Lasers)) of
	true ->
		rotate(Direction,Speed),
		avoid_wall(Direction,Speed);
	false ->
		%speed
		player:move(speed, 0)
	end.

% Determines which direction to rotate and then rotates
% Speed is in pseudo-units we scale m/s up to deg/sec
avoid_wall(Speed) ->
	timer:sleep(100),
	{_, Lasers} = player:results(lasers),
	Side = quickest_rotate(Lasers),
	avoid_wall(Side,Speed).

% Just some shortcuts to make rotation more understandable
% We scale speed in m/s up to deg/sec
rotate(left, Speed) ->
	player:rotate(speed, Speed*100);

rotate(right,Speed) ->
	player:rotate(speed, -Speed*100).


% Determens where there are 'more' obstacles and chooses the 
%direction to rotate in the directino with 'less' obstacles
quickest_rotate(Lasers) ->
	Left = lists:sum(sublasers(left,Lasers)),
	Right = lists:sum(sublasers(right,Lasers)),
	if
	Left > Right ->
		right;
	true ->
		left
	end.

% Grabs a subset of the lasers 
sublasers(center, Lasers) ->
	%CONFIG: how tight is the zone of the central sensing - in degress
	Zone = 120,
	lists:sublist(Lasers,round(length(Lasers)/2)-Zone,Zone*2);
sublasers(left, Lasers) ->
	%CONFIG: how tight is the zone of left sensing - in degress
	Zone=90,
	lists:sublist(Lasers,1, Zone*2);
sublasers(right, Lasers) ->
	%CONFIG: how tight is the zone of left sensing - in degress
	Zone=90,
	Length = round(length(Lasers)),
	lists:sublist(Lasers, Length-Zone*2, Zone*2).

% keeps traveling until it finds a wall
move_to_wall(Speed) ->
	%Timeout ot avoid a busy-loop
	timer:sleep(100),
	{_, Results} = player:results(lasers),
	case is_close(sublasers(center, Results)) of
	false ->
		player:move(speed, Speed),
		move_to_wall(Speed);
	true ->
		%stop
		player:move(speed, 0)
	end.

% returns true if at least one laser hits a wall
is_close(List) ->
	%CONFIG: distance to the 'close' object
	Limit = 1.2,
	N = lists:min(List),
	if
	N < Limit ->
		true;
	N >= Limit ->
		false
	end.
