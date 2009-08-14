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

%%% File    : multi.erl
%%% @author   Sten Gruener
%%% @doc	The module is responsible for handling multiple robots and providing interprocess
%%%		communication. Includes useful features like broadcast and group voting.
%%%		It was one of design goals to achieve independence of other modules: multi does not require thu usage
%%%		of any low- or high-level modules at all, this decision is completely up to the user.
%%%
%%%		<b>Working philosophy.</b>
%%%
%%%		Before working with multi it is important to understand some basic working principles. 
%%%		The process which is responsible for handling robots is a so called dispatcher, this process is the main part of the module.
%%%		Once started, the dispatcher will maintain a list of robots who are under his 'control'. All robots inside of this group are 
%%%		reached e.g. by a broadcast. It is rational to keep the dispatcher's PID inside of each robot belonging to the group, which will
%%%		allow it to call functions provided by the multi module.
%%%
%%%		Note that a new dispatcher can be spawned by a robot who is already a member of some group allowing to create hierarchical groups structure quickly. 

-module(multi).
-export([start/0, create_robot/4, get_list/1, broadcast/2, barrier/1, vote/2, add/2]).

%% @doc Starts the dispatcher
%% @spec start() -> pid() | {error, atom()}
start() ->
	%seed the random number generator
	{A, B, C} = erlang:now(),
	random:seed(A, B, C),
	%init loop with no robos, saved driver and empty voting lists
	spawn(fun() -> dispatcherLoop([], {[], 0}) end).

dispatcherLoop(Robotslist, {Candidatelist, Votercount})->
	receive
		{_, {multi.exit}} ->
			ok;
		{From, {multi.spawn, Module, Command, Args}} ->
			Pid = spawn(Module, Command, Args),
			%return pid
			From ! {self(), {multi.created, Pid}},
			%update list
			dispatcherLoop(Robotslist ++ [Pid], {Candidatelist, Votercount});
		{From, {multi.getlist}} ->
			From ! {self(), {multi.list, Robotslist}},
			dispatcherLoop(Robotslist, {Candidatelist, Votercount});
		{From, {multi.broadcast, Message}} ->
			%filter the sender of the message
			List = lists:filter(fun(X) -> X /= From end, Robotslist),
			%broadcast
			lists:map(fun(X) -> X ! {From, Message} end, List),
			From ! {self(), {multi.ok}},
			dispatcherLoop(Robotslist, {Candidatelist, Votercount});
		%adds robot to the Robotlist
		{From, {multi.add, List}} ->
			From ! {self(), {multi.ok}},
			dispatcherLoop(lists:append(Robotslist, List), {Candidatelist, Votercount});
		%adds candidates to a vote
		{_, {multi.vote, Pid, Participate}} ->
			case Participate of
				true ->
					NewCandidatelist = Candidatelist ++ [Pid];
				false ->
					NewCandidatelist = Candidatelist
			end,
			%one more voter registered
			NewVotercount = Votercount + 1,
			%extract the number of robots in the group
			N = length(Robotslist),
			if
				%has everyone registered?
				NewVotercount =:= N ->
					%nobody wants to be a winner?
					if		
						NewCandidatelist == [] ->
							%return dispatchers pid
							WinnerPid = self();
						true ->					
							%pick a random winner
							Winner = random:uniform(length(NewCandidatelist)),
							% extract winner`s pid
							WinnerPid = lists:nth(Winner, NewCandidatelist)
					end,
					% broadcast the winner
					lists:map(fun(X) -> X ! {self(), {multi.winner, WinnerPid}} end, Robotslist),
					% reset the vote
					dispatcherLoop(Robotslist, {[], 0});
				true ->
					dispatcherLoop(Robotslist, {NewCandidatelist, NewVotercount})	
			end
		end.	

%% @doc Spawns a robot-thread and adds the PID to the list including robot into the group.
%% <br/>A good design-philosophy would be to pass the Dispatcherid as the first argument.
%% <br/>A typical call initializing 2 robots could be:
%%
%% <tt>Driver = ps:start(),
%% <br/>Dispatcherid = multi:start(),
%% <br/>multi:createRobot(Dispatcherpid, multibouncer, thread, [Dispatcherid, Driver, 0]),
%% <br/>multi:createRobot(Dispatcherpid, multibouncer, thread, [Dispatcherid, Driver, 1]).</tt>
%%
%% with:
%%
%% <tt>thread(Dispatcherid, Driverid, Playerid) ->
%% <br/>ps:init(Driverid, Playerid).</tt>
%% @spec create_robot(pid(), atom(), atom(), [any()]) -> pid() | {error, atom()}
create_robot(Dispatcherid, Module, Function, Args) when is_list(Args) ->
		Dispatcherid ! {self(), {multi.spawn, Module, Function, Args}},
		receive
			{Dispatcherid, {multi.created, Pid}} ->
				 Pid
		end.

%% @doc Returns the list of the PID of robots in the group. 
%% @spec get_list(pid()) -> [pid()] | {error, atom()}
get_list(Dispatcherid) ->
		Dispatcherid ! {self(), {multi.getlist}},
		receive
			{Dispatcherid, {multi.list, List}} ->
				 List
		end.

%% @doc Broadcasts the message to the group except the sender.
%%
%% Note: the message {test} from PID X will be broadcasted as {X, {test}}. 
%% @spec broadcast(Pid, any()) -> ok | {error, atom()}
broadcast(Dispatcherid, Message) ->
		Dispatcherid ! {self(), {multi.broadcast, Message}},
		receive
			{Dispatcherid, {multi.ok}} ->
				ok
		end.

%% @doc Allows time-synchronization, unblocks at the same time in every thread. Useful for creating syncpoints e.g. when all robots
%% need to start at the same time. Please note that 100% sync is not possible, there will always be some slight time-variation during unlock.
%% 
%% Note: barrier needs to be called on every robot in the group, it will unblock only after every robot has called it. 
%% @spec barrier(pid()) ->  ok | {error, atom()}
barrier(Dispatcherid) ->
		% how many robots are around including me?
		N = length(get_list(Dispatcherid)),
		broadcast(Dispatcherid, {multi.barrier}),
		barrier_wait(N-1). %0 messages got, dont wait for yourself - N-1 needed to get

%Unblock if everybody is synced
barrier_wait(0) ->
		ok;
%Wait for a barrier message
barrier_wait(N) ->
		receive
			{_, {multi.barrier}} ->
				barrier_wait(N-1)
		end.


%% @doc Gives a functionality of a acknowledged-selecting one robot. Useful for sharing resources or distributing roles. 
%% Participate can be true or false indicating robots will to be elected. The winner is selected between participating robots
%% with uniform probability in order to ensure fair chances of being elected. 
%% Note: if no process sets Participate=true dispatchers pid will be returned
%% 
%% Note: like barrier needs to be called on every robot in the group, it will unblock only after every robot has called it. 
%% @spec vote(pid(), binary()) -> pid()
vote(Dispatcherid, Participate) ->
		%Let process vote
		Dispatcherid ! {self(), {multi.vote, self(), Participate}},
		%receive the result
		receive
			{Dispatcherid, {multi.winner, Winner}} ->
				%return winner
				Winner
		end.


%% @doc Adds the pid or list of pids into the Dispatchers list. In contrast to add_robot the process has to be already spawned by the user. 
%% @spec add(pid(), addtype()) -> ok | {error, atom()}
%% @type addtype() = pid() | [pid()]
add(Dispatcherid, [H|T]) ->
		Dispatcherid ! {self(), {multi.add, [H|T]}},
		receive
			{Dispatcherid, {multi.ok}} ->
				ok
		end;
add(Dispatcherid, Pid) ->
	add(Dispatcherid, [Pid]).
