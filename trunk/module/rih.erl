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

%%%-------------------------------------------------------------------
%%% File    : rih.erl
%%% Author  :  Thomas Lorentsen, Sten Gruener
%%% Description : Robot Initialisation Helper
%%%  This is a low level helper for initialising robots.
%%%  
%%% Created : 23 Jan 2009 by  Thomas Lorentsen
%%%-------------------------------------------------------------------
-module(rih).

-include("../include/common.hrl").

-export([init/2,init/4,init/5,linit/2,linit/3,destroy/1]).
-export([plinit/3,plinit/4]).

-import(mrh, [call_port/2]).

%% @doc Initiates a robot by its player index number.
%% The Robot ID is returned on successful initialisation.
%% @spec init(DriverID::pid(), Index::integer()) -> RobotID::pid()
init(Pid, Id) ->
	init(Pid, "localhost", 6665, Id).

%% @doc Initiates a robot by its hostname, port number and index number.
%% The Robot ID is returned on successful initialisation.
%% @spec init(DriverID::pid(), Hostname::string(), Port::integer(), Index::integer()) -> RobotID::pid()
init(Pid, Host, Port, Id) ->
	init(Pid, Host, Port, Id, genNickname(Host, Port, Id)).
	

%% @doc Initiates a robot by its hostname, port number and index number.
%% The Robot ID is returned on successful initialisation.
%% This function allows by passing the auto generation of the nickname used to identify the robot inside the driver.
%% This name must be unique otherwise an error will be returned.
%% @spec init(DriverID::pid(), Hostname::string(), Port::integer(), Index::integer(), Nickname::string()) -> RobotID::pid()
init(Pid, Hostname, Port, Id, Nickname) ->
	create(Pid, {Hostname, Port, Id, Nickname}).


%creates a robot from the initialised robot
create(Pid, {Hostname, Port, Id, Nickname}) ->
	MPid = spawn(mrh, loop, [Pid, Nickname]),
	connected(MPid, call_port(MPid, {create, Hostname, Port*1.0, Id*1.0, Nickname})).

%% Checks if the client was successfully connected 
% returns a pid
connected(Mipd, ok) ->
	Mipd;
% or an error
connected(_, Error) ->
	Error.

%% Generates a nickname from the information provided.
%% This should be unique for each robot
%% Names are only important at driver level so that the driver only
%% has to remember one string instead of 3 or 4 values to communicate 
%% to a robot.
%% Returns a nickname to be used
genNickname(Host, Port, Id) ->
	Host++":"++integer_to_list(Port)++":"++integer_to_list(Id)++":"++genPseudoRandomHexId().


%% @doc Initalise a list of robots.
%% Each item can consist of either an index number of a tuples of any of the following:
%% {Index} 
%% {Hostname, Port, Index} 
%% {Hostname, Port, Index, Nickname} 
%% A list of RobotIDs are returned.
%% @spec linit(DriverID::pid(), Configs::list()) -> RobotID::list()
linit(_,[]) ->
	[];
linit(Pid, [Conf|Robots]) ->
	[deconfig(Pid, Conf)]++linit(Pid, Robots).

%% @doc Initiates a list of robots using a common configuration.
%% A list of RobotIDs are returned.
linit(_, [], _) ->
    [];
% auto configs and then inits
linit(Pid, [Conf|Robots], AutoConf) ->
    [autoconfig(Pid, AutoConf, Conf)]++linit(Pid, Robots, AutoConf).

%% Reads a robot config
deconfig(Pid, {Id}) ->
	init(Pid, Id);
deconfig(Pid, {Host, Port, Id}) ->
	init(Pid, Host, Port, Id);
deconfig(Pid, {Host, Port, Id, Nickname}) ->
	init(Pid, Host, Port, Id, Nickname);
deconfig(Pid, Id) ->
	init(Pid, Id).

%% Configures robots using common settings
% it starts with defaults and replaces values
% inits robot with these new settings when all settings have been applied
autoconfig(Pid, [], {Host, Port, Id}) ->
    init(Pid, Host, Port, Id);
% Host config
autoconfig(Pid, [{host,Host}|M], {_, Port, Id}) ->
    autoconfig(Pid, M,{Host, Port, Id});
% port config
autoconfig(Pid, [{port,Port}|M], {Host, _, Id}) ->
    autoconfig(Pid, M,{Host, Port, Id});
% id config
autoconfig(Pid, [{id,Id}|M], {Host, Port, _}) ->
    autoconfig(Pid, M,{Host, Port, Id});
% Passed {Host}
autoconfig(Pid, AutoConf, {Host}) ->
    autoconfig(Pid, AutoConf, {Host, 6665, 0});
% Passed {Host, Id}
autoconfig(Pid, AutoConf, {Host, Id}) ->
    autoconfig(Pid, AutoConf, {Host, 6665, Id});
% Passed Id
autoconfig(Pid, AutoConf, Id) ->
    autoconfig(Pid, AutoConf, {"localhost", 6665, Id}).

%destroys the robot
destroy(Robotid) ->
	call_port(Robotid, {destroy}).


%% @doc initiates a list of robots in parallel.
%% Returns a list of robots.
%% @spec plinit(DriverID::pid(), Robots::list(), Config::list()) -> RobotID::pid()
plinit(Pid, Robots, AutoConf) ->
	% will spawn a initiator for each robot
	[ spawn(rih,plinit,[self(), Pid, [Robot], AutoConf]) || Robot <- Robots],
	% will iterate by number of robots
	[ reply() || _ <- Robots].

%% This returns a reply from this module
reply() ->
	receive
		{rih, [A]} ->
			A
	end.


%% @doc initiates a list of robots in parallel.
%% Passes the list of messages to the caller process.
%% @spec plinit(Caller::pid(), DriverID::pid(), Robots::list(), Config::list()) -> RobotID::pid()
plinit(Caller,_, [], _) ->
	Caller ! {Caller,[]};
plinit(Caller, Pid, [Conf|Robots], AutoConf) ->
	spawn(rih,plinit,[Caller, Pid, Robots, AutoConf]),
	Caller ! {rih, linit(Pid, [Conf], AutoConf)}.



%% This generates fixed length hex string that can be used as a id
%% This should allow us to create many connections to the same robot
%% Hopefully it is unlikely we will get 2 values twice
genPseudoRandomHexId() ->
	genPseudoRandomHexId(erlang:now()).
% same as above but you can pass your own seed
genPseudoRandomHexId({A, B, C}) ->
	% generate our seed to make it as random as possible
	random:seed(A, B, C),
	% Generates the hex strings where the list contains the possible ascii characters
	[ lists:nth(random:uniform(15), "0123456789abcedf") || _ <- lists:seq(1,10,1)].
