%%% KERL - Kent Erlang Robotics Library
%%% Copyright (C) 2009
%%%    Thomas Lorentsen, Sten Gruener
%%%
%%%
%%% This program is free software; you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation; either version 2 of the License, or
%%% (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
%%%

%%% File    : player.erl
%%% @author   Sten Gruener
%%% @doc	  The player module is a high-level Erlang abstraction module, 
%%%  providing basic robot's functions. This module should be suitable for
%%%  students, who want to start playing with KERL without worrying about 
%%%  server PIDs and KERL's architecture. 
%%%  It is supposed, that the low-level modules are going to be used after
%%%  the provided functionality of the player module is not longer satisfying
%%%  the user.
%%%  <br/>Originally the functionality was provided by module itself but, now it
%%%  uses the low-level modules.
%%%  This naive module can only control 1 robot per process.
%%%  Please note that most of the movement functions are artificially made 
%%%  blocking in the player module. 
%%%

-module(player).

-include("../include/common.hrl").

-export([start/0, init/2, init/4, init/5, destroy/0, rotate/2, move/2, stop/0, get_position/0, results/1, set_option/2, get_pid/0]).

%% @doc Starts the server. Usually the first commant to run.
%% Returns the pid to the server to allow communication to the driver.
%% @spec start() -> pid() | {error, atom()}
start() ->
	mrh:start().

%% @doc This is the more rich signature of the init function allowing
%% to specify both the hostname, ip and the port of the player server 
%% as well as the robot's nickname.
%% @spec init(pid(), string(), integer(), integer(), string()) -> ok | {error, atom()}
init(Driverpid, Hostname, Port, Id, Nickname) ->
	init_result(rih:init(Driverpid, Hostname, Port, Id, Nickname)).
	
%% @doc A sorter signature allowing not to specify the nickname.
%% @spec init(pid(), string(), integer(), integer()) -> ok | {error, atom()}
init(Driverpid, Hostname, Port, Id) ->
	init_result(rih:init(Driverpid, Hostname, Port, Id)).
	
%% @doc Shortest signature localhost and default port 6665 are used.
%% @spec init(pid(), integer()) -> ok | {error, atom()}
init(Driverpid, Id) ->
	init_result(rih:init(Driverpid, Id)).

% This is a private helper for the init functions
init_result(Atom) ->
	case erlang:is_pid(Atom) of
		true ->
			erlang:put(pid, Atom),
			ok;
		false ->
			Atom
	end.
	
%% @doc Destroys the initialized robot.
%% @spec destroy() -> ok | {error, atom()}
destroy() ->
			rih:destroy(erlang:get(pid), {destroy}).

%% @doc Returns the current odometrical (inner rotation sensor)
%% position of the robot. This position is local (every robot 
%% starts at (0,0,0)) and with some measurement errors due to 
%% the nature of the sensors. 
%%
%% Note: A is in radiants, from 0 to 2*Pi.
%% @spec get_position() -> {X, Y, A} | {error, atom()}
get_position() ->
			mvh:get_position(erlang:get(pid)).

% private
wait_for_stop(Same, Same) ->
	ok;
wait_for_stop(_, Newtuple) ->	
	timer:sleep(1500),
	{X,Y,A} = get_position(),
	%parameter for how long to block
	Precision = 100,
	wait_for_stop(Newtuple, {erlang:trunc(Precision*X),erlang:trunc(Precision*Y),erlang:trunc(Precision*A)}).
wait_for_stop(Oldtuple) ->
		timer:sleep(1500),
		wait_for_stop(Oldtuple, get_position()).
		
%% @doc Blocks robot until there is no position change.
%% @spec wait_for_stop() -> ok | {error, atom}
wait_for_stop() ->
		wait_for_stop(get_position()).

%% @doc The basic rotation function. Allows to rotate for a certain amount of 
%% degrees or just with a certain speed. Positive value means mathematically
%% positive rotation - counter clockwise. 
%% Rotating for degrees will block until robot stops. 
%% @spec rotate(rotationmode(), float()) -> ok | {error, atom}
%% @type rotationmode() = degrees | speed
rotate(Type, Value)->
			mvh:rotate(erlang:get(pid), Type, Value),
		  case Type of
				degrees ->
					wait_for_stop();
				_ ->
					ok
			end.	          

%% @doc The basic movement function. Supports a varriaty of arguments.
%% <br/>Arguments:
%% <ul>
%% 	<li>move(full, {Rotation, Speed}) - allows to move and rotate at the same time, nonblocking.</li>
%%	<li>move(position, {X, Y, A}) - goes to a specified position, blocking.</li>
%%	<li>move(position, {X, Y}) - A is extracted from actual position, blocing.</li>
%%	<li>move(difference, {X, Y, A}) - goes adds the difference to the actual position, blocking.</li>
%%	<li>move(distance, Distance) -> goes straight for a specified distance, blocking.</li>
%%	<li>move(speed, Speed) -> moves at a specified speed with no rotation, nonblocking.</li>
%% </ul>
%% @spec move(movemode(), movevalue()) -> ok | {error, atom}
%% @type movemode() = full | position | difference | distance | speed
%% @type movevalue() = {float(), float()} | {float(), float(), float()}  | float()
move(full, {Speed, Rotation})->
			mvh:move(erlang:get(pid), {move, full, Speed, Rotation});
move(position, {X, Y, A}) ->
			mvh:move(erlang:get(pid), {move, position, X, Y, A}),	
			wait_for_stop();
move(position, {X, Y}) ->
			{_, _, A} = get_position(),
			move(position, {X, Y, A});
move(difference, {X, Y, A}) ->
			mvh:move(erlang:get(pid), difference, {X, Y, A}),
			wait_for_stop();
move(Type, Value) ->
			mvh:move(erlang:get(pid), {move, Type, Value}),
			case Type of
				distance ->
					wait_for_stop();
				_ ->
					ok
			end.


%% @doc stops a moving robot.
%% @spec stop() -> ok
stop() ->
    mvh:move(erlang:get(pid), {move, speed, 0}).


%% @doc This function gets the results of selected device, for device list
%% please consult the documentation of the dvh module.
%% @spec results(atom()) -> {[float()],[float()]} | {error, atom}
results(Device) ->
	dvh:read(erlang:get(pid), Device).

%% @doc This function sets the options of the C driver.
%% @spec set_option(atom(), any()) -> ok | {error, atom}
set_option(Option, Value) ->
			mrh:call_port(erlang:get(pid), {set_option, Option, Value}).

%% @doc Extracts the cached pid in order to be used in low-level functions.
%% Should be only used if more functions are needed, then player module can
%% provide.
%% @spec get_pid() -> pid() | {errot, atom}			
get_pid() ->
			erlang:get(pid).
