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

%%% File    : comm.erl
%%% @author   Sten Gruener
%%% @doc	  The comm module is used to provide communication enhancements for a single robot,
%%		 for multiple robot communication use multi module. 
%%%
-module(comm).
-export([discard/1]).

%% @doc Scans the message queue, discarding messages which match {_, Pattern}.
%% Note: this a bit ugly construction has been selected, since Erlang wildcards are no first order objects
%% and can not be passed into a function.
%% @spec discard(any()) -> ok
discard(Pattern) -> 
	receive
		{_, Pattern} ->
			discard(Pattern)
	after 0 ->
		ok
	end.
