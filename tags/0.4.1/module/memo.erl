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

%%% File    : memo.erl
%%% @author   Sten Gruener
%%% @doc	The memo module is a high-level Erlang module, providing a simple 
%%%		possibility for a process to memorize some values, without having
%%%		to keep them as a function parameter. No dictionary is used,
%%		instead the use of message queue is made.
%%%

-module(memo).
-export([save/2, read/1]).

%% @doc Saves a memo. 
%% Note: the identifier is expected to be a String and may not be unique 
%% @spec save(string(), any()) -> ok | {error, atom()}
save(Identifier, Message) when erlang:is_list(Identifier) ->
	%add unique prefix
	NewIdentifier = erlang:list_to_atom("memo." ++ Identifier),
	%save the message
	self() ! {NewIdentifier, Message},
	ok.

%% @doc Reads a memo trying to pattern-match the identifier, non-blocking.
%% Note: if there multiple memos with same identifier the FIFO approach will be used.
%% Returns {error, no_memo} if no memo matched
%% @spec read(string()) -> any() | {error, atom()}
read(Identifier) when erlang:is_list(Identifier) -> 
	%add unique prefix
	NewIdentifier = erlang:list_to_atom("memo." ++ Identifier),
	receive
		{NewIdentifier, Memo} ->
			Memo
	after 0 ->
		{error, no_memo}
	end.
