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

%% @author  Thomas Lorentsen, Sten Gruener
%% @doc     Used to calculate triangles and other maths.
%% Provides useful functions for calcuating angles and triangles.
%% This module was written to help provide functions for determining positions
%% of objects using the devices.  Hopefully they are useful.


-module(trimaths).

-import(math, [pi/0,sin/1,cos/1]).

-export([solve_ah/2, deg2rad/1,rad2deg/1]).

-export([results_to_triangle/3]).


%% @doc Solves triangle sides from angle and hypotenuse.
%% This allows to calculate where a wall is on a map
%% @spec solve_ah(float(), float()) -> {Sine::float(), Cosine::float(), Hypotenuse::float()}
solve_ah(Angle, Hypotenuse) ->
	{sin(deg2rad(Angle))*Hypotenuse, cos(deg2rad(Angle))*Hypotenuse, Hypotenuse}.
	



%% @doc Converts each result into a triangle {X, Y, H}.
%% Results: list of laser results
%% Degrees: Starting degrees
%% Inc: Incriment by this amount of degrees
%% This allows you to find the relative position of a object using the lasers
%% @spec (Results::list(), Degrees, Inc) -> Triangles::list()
results_to_triangle([],_,_) ->
    [];
results_to_triangle([Laser|Lasers], Degrees, Inc) ->
    [solve_ah(Degrees, Laser)] ++ results_to_triangle(Lasers, Degrees+Inc, Inc).


%% @doc convert degrees to radians.
%% @spec deg2rad(Degrees::float()) -> Radians::float()
deg2rad(Deg) ->
	Deg * pi()/180.


%% @doc convert radians to degrees.
%% @spec rad2deg(Radians::float()) -> Degrees::float()
rad2deg(Rad) ->
	Rad * 180 / pi().
