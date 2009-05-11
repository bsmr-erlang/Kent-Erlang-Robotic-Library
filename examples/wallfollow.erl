%%%
%%% File	: wallfollow.erl
%%% Author	: Matthew Champ, functions mostly stolen from Sten!
%%% Description : Example simple wall follower robot
%%%
%%% Created	: February 9th 2009
-module(wallfollow).
-export([start/0]).

start() ->
	%add path to modules
	code:add_path("../module/"),code:add_path("../ebin/"),
	%start server
	Server = ps:start(),
	%init robot
	ps:init(Server, "localhost", 6665, 0),
	loop().

loop() ->
	%test run
	%move_to_wall(1),
	side_check(),
	loop().

%test for wall safety
side_check() ->
	{_, Results} = ps:results(lasers),
	case is_close(sublasers(center, Results)) of
	false ->
		move_to_wall(1);
	true ->
		ps:rotate(speed, 15)
	end.

%move to in range of the wall
move_to_wall(Speed) ->
	timer:sleep(100),
	{_, Results} = ps:results(lasers),
	case is_close(sublasers(center, Results)) of
	false ->
		ps:move(speed, Speed),
		move_to_wall(Speed);
	true ->
		%stop
		ps:move(speed, 0)
	end.

%returns true if at least one laser hits the wall
is_close(List) ->
N = lists:min(List),
	if
	N < 1.2 ->
		true;
	N >= 1.2 ->
		false
	end.

%grabs a subset of the lasers
sublasers(center, Lasers) ->
	lists:sublist(Lasers,round(length(Lasers)/2)-20,40);
sublasers(left, Lasers) ->
	W=90,
	lists:sublist(Lasers,0+W,180-W);
sublasers(right, Lasers) ->
	W=90,
	lists:sublist(Lasers,180,180-W).
