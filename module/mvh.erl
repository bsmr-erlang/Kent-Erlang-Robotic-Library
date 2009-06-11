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

%%% File    : mvh.erl
%%% @author   Thomas Lorentsen, Sten Gruener
%%% @doc      Simple robot movement functions
%%% Use this module for very simple robot movements.
%%% It provides moving the robot by speed or to a position.
%%% It also allows the position of the robot to be read and
%%% some other useful movement functions.
%%% This functions are non-blocking so they will return before the robot has reached its destination.
%%%


-module(mvh).

-import(mrh, [call_port/2]).
-export([move/2,move/3,stop/1,rotate/3]).
-export([get_position/1]).


%% @doc Passes the command to the driver.
%% @spec move(RobotId::pid(), Command::tuple()) -> Any
%% @deprecated using {@link mrh:call_port/2} will produce same results 
%% @hidden I want to avoid use of this function
move(RobotId, Command) ->
	call_port(RobotId, Command).


%% @doc Provides several ways of moving the robot.
%% Supports: distance, speed, full, difference, position.
%% It also supports a list of RobotIds.
%% @spec (RobotId::pid(), Mode::movemode(), Params::tuple()) -> ok
%% @type movemode() = distance | speed | full | difference | position.
%% <b>distance</b> 
%% <p>Will move until it has moved a specified distance.</p>
%% <b>speed</b>
%% <p>Will constantly keep moving at a specified speed.</p>
%% <b>full</b>
%% <p>As well as moving at a specified speed, rotation speed can be specified.</p>
%% <b>difference</b>
%% <p></p>
%% <b>position</b>
%% <p>Travels toward a specified position.</p>

move([], _, _) ->
	[];
move([RobotId|M], Type, Params) ->
	[move(RobotId, Type, Params)]++move(M, Type, Params);
%% move by distance
move(RobotId, distance, Distance) ->
	call_port(RobotId, {move, distance, Distance});
%% stops the robot when traveling by speed
move(RobotId, speed, stop) ->
	call_port(RobotId, {move, speed, 0});
%% move by speed
move(RobotId, speed, Speed) ->
	call_port(RobotId, {move, speed, Speed});
%% move by speed and rotation
move(RobotId, full, {Speed, Rotation}) ->
	call_port(RobotId, {move, speed, Speed, Rotation});
%% move by a difference to the actual position
move(RobotId, difference, {X, Y, A}) ->
	{PosX, PosY, PosA} = get_position(RobotId),
	move(RobotId, position, {X+PosX, Y+PosY, A+PosA});
%% move to position
move(RobotId, position, {X, Y, A}) ->
	call_port(RobotId, {move, position, X, Y, A}).


%% @doc Stops a moving robot.
%% @spec stop(RobotId::pid()) -> ok
stop(RobotId) ->
    mvh:move(RobotId, speed, 0).


%% @doc Rotate the robot by degrees or speed.
%% Rotating by degrees is relative.
%% It keep rotating until it has reached the direction it needs.
%% Rotating by speed will make the robot rotate at a fixed speed.
%% It will keep rotating until it is stopped by using another movement function.
%% rotate by amount of degrees
%% @spec rotate(RobotID::pid(), Mode::rotationmode(), float()) -> ok | {error, Reason::atom()}
%% @type rotationmode() = degrees | speed.
%% <b>degrees</b> 
%% <p>This tells the robot to rotate until it has reached the number of degrees specified.</p>
%% <b>speed</b>
%% <p>This tells the robot to constantly rotate at a specified speed.</p>
rotate(RobotId, degrees, Degrees) ->
	call_port(RobotId, {rotate, degrees, Degrees});
%% rotate at speed
rotate(RobotId, speed, Speed) ->
	call_port(RobotId, {rotate, speed, Speed}).


%% @doc Reads the robot odometer.
%% The inital odometer is set to {0,0,0} and will track the position of the 
%% robot from its starting position.
%% @spec get_position(RobotID::pid()) -> {X::float(), Y::float(), Radians::float()} | {error, Reason::atom()}
get_position(RobotId) ->
    call_port(RobotId, {get_position}).


