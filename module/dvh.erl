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

%%% File    : dvh.erl
%%% @author   Thomas Lorentsen, Sten Gruener
%%% @doc      A Simple device reader.
%%% Provides functions for reading devices
%%% It also supports concurrent reading of devices from a list or robots.
%%% Note that it is possible to return empty results.


-module(dvh).

-import(mrh, [call_port/2]).
-export([read_lasers/1,read/2, results/2]).
-export([read_robots/2,read_devices/2]).
-export([pread_robots/2,pread_robots/3]).
-export([find_value/2]).


%% @doc duplication of read for naming consitancy with player
%% Devices supported: lasers
%% @spec (RobotId::pid(), Device::atom()) -> Results::tuple() | {error, no_such_device}
results(Rid, Device) ->
	read(Rid, Device).

%% @doc Read device of a robot.
%% Pass in the robot pid and an atom of the device name
%% Devices supported: lasers
%% @spec (RobotId::pid(), Device::atom()) -> Results::tuple() | {error, no_such_device}
read(Rid, lasers) ->
	read_lasers(Rid);
% Return a error without calling the driver
read(_,_) ->
	{error,no_such_device}.


%% @doc Reads the lasers and returns a list of results. *Duplication of results() for back-compatiblity reasions*
%% @spec (RobotID::pid()) -> {Lasers::list(), Bearings::list()}
read_lasers(Rid) ->
	call_port(Rid, {results, lasers}).



%% @doc Reads multiple devices from the robot
%% @spec (RobotID::pid(), Devices::list()) -> [{RobotID::pid(), Device::string(), Results::tuple()}]
read_devices(_, []) ->
	[];
% Match on list of devices
read_devices(Rid, [Device|Devices]) ->
	[{Rid, Device, read(Rid, Device)}]++read_devices(Rid, Devices);
% Match on device names
read_devices(Rid, Device) ->
	[{Rid, Device, read(Rid, Device)}].


%% @doc Reads a list of devices or a device with a list of robots
%% Robot pids are passed in a list with a list of device name atoms.
%% @spec (Robots::list(), Devices::list()) -> [{RobotID::pid(), Device::string(), Results::tuple()}]
read_robots([], _) ->
	[];
% Provice a list of devices or the device name
read_robots([Rid|Rids], Devices) ->
	read_devices(Rid, Devices) ++ read_robots(Rids, Devices).



%% @doc Reads robots in parrallel and returns the results.
%% The robot id is built up of a list of robot pids.
%% The devices is a list of atoms of the device names.
%% @spec (RobotIds::list(), Devices::list()) -> Results::list()
pread_robots(Robots, Devices) ->
	[ spawn(dvh, pread_robots, [self(), Rid, Devices]) || Rid <- Robots],
	[ reply() || _ <- Robots].

%% This returns a reply from this module
reply() ->
	receive
		{dvh, A} ->
			A
	end.


%% @doc Reads robots in parrallel and returns results to caller.
%% The robot id is built up of a list of robot pids.
%% The devices is a list of atoms of the device names.
%% If you use this you have to handle the message receiving yourself
%% @spec (pid(), RobotIds::list(), Devices::list()) -> {dvh, Results::list()}
pread_robots(Caller, Rid, Devices) ->
	Caller ! {dvh, read_devices(Rid, Devices)}.


%% A C-Style-Array value finder which returns the index of the value in the list
%% returns the last occurence of the item, useful to find a minimum inside of the
%% list of laser measurments.
%% input: Value to find, List to search in.
%% return: index if found (first element is 0), -1 if not found.
find_value(Value, List)->
	find_value(Value, List, 1, -1).
find_value(Value, [Value|[]], Index, _)->
	Index;
find_value(Value, [Value|Tail], Index, _)->
	find_value(Value, Tail, Index+1, Index);
find_value(_, [_|[]], _, Lastocc)->
	Lastocc;
find_value(Value, [_|Tail], Index, Lastocc)->
	find_value(Value, Tail, Index+1, Lastocc).

