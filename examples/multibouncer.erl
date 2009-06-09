%%%-------------------------------------------------------------------
%%% File    : multibouncer.erl
%%% Author  : Sten Gruener
%%% Description : This is the final version of multibouncer which uses
%%%		differential positioning and votings.
%%%
%%% Created :  13 Jan 2009 by Sten Gruener
%%%-------------------------------------------------------------------

-module(multibouncer).
-export([start/0, main/3]).

start() ->
	%add path to the modules
	code:add_path("../module/"),code:add_path("../ebin/"),
	%start driver
	Driver = player:start(),
	%start multi-robot module
	Pid = multi:start(),
	%create 4 bouncers
	lists:map(fun(X) -> multi:create_robot(Pid, ?MODULE, main, [Pid, Driver, X]) end, lists:seq(0,3,1)),
	ok.

main(Dispatcherid, Driver, Playerid) ->
	%init robot
	player:init(Driver, Playerid),
	%======EXAMPLE`S CONFIG======
	Speed = 0.35,
	%=======END OF CONFIG========
	loop(Dispatcherid, Speed).

loop(Dispatcherid, Speed) ->
	%barrier
	multi:barrier(Dispatcherid),
	%save the initial position
	memo:save("initial_position", player:get_position()),
	%run the main function
	travel(Dispatcherid, Speed),
	%infinite loop
	loop(Dispatcherid, Speed).

% keeps traveling until it finds a wall
travel(Dispatcherid, Speed) ->
    receive
		{_, {stop}} ->
			player:stop(),
			% We have to vote to determine a leader
			% I do not want to become one so I am not interested in result
			multi:vote(Dispatcherid, false),
			% Discard received duplicates
			comm:discard({stop}),
			%Do the slave part
			slave()
			%wait 40 ms
    after 40 ->
			{_, Results} = player:results(lasers),
			case lists:min(Results) < 1 of
				%no obstacle found -> keep moving
				false ->
					player:move(speed, Speed),
					travel(Dispatcherid, Speed);
				%I see an obstacle!
				true ->
					%alert companions
					multi:broadcast(Dispatcherid, {stop}),
					%stop
					player:stop(),
					%save my pid
					MyPid = self(),
					%Participate in a voting as leader-candidate
					io:format("[~p]: ~p candidates to become a leader~n",[erlang:time(),self()]),
					Leader = multi:vote(Dispatcherid, true),
					case Leader of
						MyPid ->
							io:format("[~p]: ~p has won the election~n",[erlang:time(),self()]),
							%I have won the election
							master(Dispatcherid, Speed);
						_ ->
							io:format("[~p]: ~p has lost the election THE OLD SYSTEM WOULD CRASH HERE!~n",[erlang:time(),self()]),
							%I have lost the election
							slave()
					end
			end
	end.

%I am a master for this turn
master(Dispatcherid, Speed) ->
	%Discard wrongly received messages
	comm:discard({stop}),
	%I have located the obstacle - avoid_wall
	bouncer:avoid_wall(Speed),
	%get orientation
	{X, Y, A} = player:get_position(),
	%get initial position
	{InitX, InitY, InitA} = memo:read("initial_position"),
	%broadcast the difference
	multi:broadcast(Dispatcherid, {correction, {X-InitX, Y-InitY, A-InitA}}).

%I am a slave for this turn
slave() ->
	%Discard wrongly received messages
	comm:discard({stop}),
	%correct the position
	receive
		{_, {correction, {MasterDiffX, MasterDiffY, MasterDiffA}}} ->
			%my position now
			{X, Y, A} = player:get_position(),
			%get initial position
			{InitX, InitY, InitA} = memo:read("initial_position"),
			%Compute own differences
			{DiffX, DiffY, DiffA} = {X-InitX, Y-InitY, A-InitA},
			%correct the 'difference' between differences
			player:move(difference, {MasterDiffX-DiffX, MasterDiffY-DiffY, MasterDiffA-DiffA})
	end.
