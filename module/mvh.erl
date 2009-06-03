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

-module(mvh).

-import(mrh, [call_port/2]).
-export([move/3,rotate/3]).
-export([get_position/1]).


%% lowest level move command
% Simply just fowards to callport
%move(RobotId, Command) ->
%	call_port(RobotId, Command).


%% @doc Provides several ways of moving the robot.
%% Supports: distance, speed, full, difference, position.
%% It also supports a list of RobotIds.
%% @spec (RobotId, Type::atom(), Params::tuple()) -> ok
move([], _, _) ->
	[];
move([RobotId|M], Type, Params) ->
	[move(RobotId, Type, Params)]++move(M, Type, Params);
%% move by distance
move(RobotId, distance, Distance) ->
	call_port(RobotId, {move, distance, Distance});
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

%% @doc Rotate the robot by degrees or speed.
%% Rotating by degrees is relative.
%% It keep rotating until it has reached the direction it needs.
%% Rotating by speed will make the robot rotate at a fixed speed.
%% It will keep rotating until it is stopped by using another movement function.
%% rotate by amount of degrees
%% @spec rotate(RobotID::pid(), rotationmode(), float()) -> ok | {error, atom}
%% @type rotationmode() = degrees | speed
rotate(RobotId, degrees, Degrees) ->
	call_port(RobotId, {rotate, degrees, Degrees});
%% rotate at speed
rotate(RobotId, speed, Speed) ->
	call_port(RobotId, {rotate, speed, Speed}).


%% @doc Gets the position of the robot.
%% @spec get_position(RobotID:pid()) -> {X::float(), Y::float(), A::float()}
get_position(RobotId) ->
    call_port(RobotId, {get_position}).


