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

%%% File    : mrh.erl
%%% @author   Thomas Lorentsen
%%% @doc      Multiple robot control server.
%%% Allow multiple robots to be controlled from one driver.
%%% Due to the async driver, it can only be loaded once.
%%% Use this to start the driver and allow multiple robot
%%% connections to be made.
%%% By passing the robot pid from this module into callport the
%%% messages will automatically be forwarded to the driver.

-module(mrh).

-include("../include/common.hrl").

-import(driver, [call_port/4]).
-export([start/0, loop/1, loop/2, call_port/2,info/1]).

%% @doc Starts the driver and the multirobot server.
%% @spec () -> RobotServer::pid() | {error, Error::atom()}
start() ->
	started(driver:start()).


%% Check if driver has been started and initialise
started({ok, Pid}) ->
	init(Pid);
started({error, permanent}) ->
	{error, stop_server_first};
% Return an error if it couldn't be started
started(Error) ->
	Error.


% any initialisation needed to be done
init(Pid) ->
	spawn(?MODULE, loop, [Pid]).


%% Receive commands being made to be relayed to the driver
%% @hidden
loop(DriverPid) ->
	?DBUG("call with driver id"),
	receive
		{Pid, RobotId, Message} ->
			?DBUG("calling driver port"),
			call_port(DriverPid, Pid, RobotId, Message),
			loop(DriverPid)

	end.


%% This caches the robot id and the driver id connection
%% @hidden
loop(DriverPid, RobotId) ->
	
	receive
		{Pid, stoprobot} ->
			Pid ! ok;
		{Pid, Message} ->
			DriverPid ! {Pid, RobotId, Message},
			loop(DriverPid, RobotId)		
	end.


	
%% @doc Pass the robot id and the message to send to the driver.
%% Allows communication to the driver.
%% This will send the message to the driver and return its reply.
%% Pass commands to the server
%% @spec(RobotId::pid(), Message::any()) -> any()
call_port(Pid, Message) ->
	Pid ! {self(), Message},
	receive
		{return, Return} ->
			 Return

	end.

%% @doc Returns information about the robot.
%% @spec info(RobotId::pid()) -> info()
%% @type info() = {Rid::pid(),{RobotName, Host, Port, Index}} | list()
info([]) ->
    [];
info([Rid|M]) ->
    [info(Rid)]++info(M);
info(Rid) ->
    {Rid, call_port(Rid, {info})}.
