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

%%% File    : driver.erl
%%% @author   Thomas Lorentsen, Sten Gruener
%%% @doc      This is the Erlang Port Driver
%%%  Upon start the driver will be loaded into the Erlang runtime enviroment.
%%%  Using call_port messages can be passed to and from the driver.
%%%  You can only start the driver once using an asynchronous driver.
%%%  So messages being passed into the driver are serialised and passed into
%%%  the driver.
%%%  Once the message has been passed into the driver it can continue to pass 
%%%  more messages into the driver.
%%%  Messages are passed back to the caller argument of callport.
%%%  To set the number of asynchronous processes inside the port driver
%%%  you need to tweak the +A option in erlang shell.
%%%

-module(driver).

-include("../include/common.hrl").

-export([start/0, stop/1, init/2]).
-export([call_port/4]).

%% @doc Starts the driver.
%% returns the pid to the server to allow communication to the driver.
%% @spec start() -> pid()
start() ->
	start("kerl").


%% locates and starts the driver
start(SharedLib) ->
	?DBUG("Loading driver"),
	driver_loaded(erl_ddll:load_driver(find_driver(["./lib/","../lib/", "../../lib/", "/lib/", "/usr/lib/", "/usr/local/lib/", "/usr/lib/erlang/lib/kerl/lib/", "~/lib/"], SharedLib),
									  SharedLib), SharedLib).

%% takes a list of directories and returns the directory where the driver was found
% Last attempt tries current directory
find_driver([], _) ->
	".";
% Finds the driver from a list of directories
% Once the driver has been found it will return the directory path
find_driver([Dir|Dirs], SharedLib) ->
	case file:read_file_info(Dir++SharedLib++".so") of
		{error,_} ->
			find_driver(Dirs, SharedLib);
		{ok,_} ->
			Dir
	end.


%% Determines if error occured when loading driver
% If ok it will initiate the driver
driver_loaded(ok, SharedLib) ->
	wait_for_port_ready(SharedLib, 9001);

% This error code is rubbish so I return a human friendly error message
driver_loaded({error, {open_error, -10}}, _) ->
	{error, {open_error, driver_not_found}};
% a catch all error
driver_loaded({error, Message}, _) ->
	{error, Message}.


%% Blocks until either the driver is ready or the timeout is reached.
%% This usually only takes around a second.
%% You can pass infinity to make it blocking
wait_for_port_ready(SharedLib, Timeout) ->
    S = self(),
    spawn(?MODULE, init, [S, SharedLib]),
    % block until port is running or timeouts
    receive
		{ok, Pid} ->
			{ok, Pid}
    after(Timeout) ->
	    {error, port_never_responded}
    end.


%% @doc Initiates the port server to allow messages passed to it.
%% It will return a pid to the driver allowing communication.
%% @spec init(pid(), string()) -> {ok, pid()} | {error, atom()}
%% @private
init(Pid, SharedLib) ->
    Port = open_port({spawn, SharedLib}, []),
    ?DBUG("Driver Loaded"),
    Pid ! {ok, self()},
    loop(Port).


%% call_port passes messages to here where the messages are relayed onto the port driver.
%% Messages received from the port driver are passed back to the caller
%% This is simplier than it looks and most of the bloat is just changing data types around
%% to work with Erlang Interface.
%% All messages to be passed to port must be in a tuple beginning with call atom.
%% Messages from the port driver begin with port process id in the atom and the caller
loop(Port) ->
    receive												
												% create a new robot and update the robot id
		{call, Caller, {create, Host, Port, ID, RobotId}} ->
			?DBUG("creating new robot"),
			Port ! encode_port_message(self(), RobotId, Caller, {create, Host, Port, ID, RobotId}),
			loop(Port);
		
												% all other messages to the robot
		{call, Caller, RobotId, Message} ->
			?DBUG("calling robot with this message"),
			?DBUG(Message),
			Port ! encode_port_message(self(), RobotId, Caller, Message),
			loop(Port);
			
		{Port, OriginalCaller, tuple_float_3, Return} ->
			list_to_pid(atom_to_list(OriginalCaller)) ! {return, tuple_decode(Return)},
			loop(Port);
		{Port, OriginalCaller, list_float, ListA, list_float, ListB} ->
			list_to_pid(atom_to_list(OriginalCaller)) ! {return, list_decode({ListA, ListB})},
			loop(Port);
		{Port, OriginalCaller, fiducials, Return} ->
			list_to_pid(atom_to_list(OriginalCaller)) ! {return, fid_decode(Return)},
			loop(Port);
		{Port, OriginalCaller, Return} ->
			list_to_pid(atom_to_list(OriginalCaller)) ! {return, Return},
			loop(Port);
	
		stop ->
			Port ! {self(), close},
			receive
				{Port, closed} ->
					exit(normal)
			end
    end.

%% encodes the message so it can be passed into the port driver
encode_port_message(Pid, Rid, Caller, Message) ->
	{Pid, {command, term_to_binary({Rid, pid_to_list(Caller), Message})}}.


%% fiducial tuples need to be converted back to doubles
fid_decode([]) ->
	[];
% [{Id, {Roll, Pitch, Yaw},{UX,UY,UZ},{URoll, UPitch, UYaw}} | _]
fid_decode([{Id, Position,Rotation, UPosition, URotation}|More]) ->
	[{Id, 
	  tuple_decode(Position),
	  tuple_decode(Rotation),
	  tuple_decode(UPosition),
	  tuple_decode(URotation)}]++fid_decode(More).

%% pass in a tuple and it will upscale the values
tuple_decode({X,Y,Z}) ->
	{X/1000000,Y/1000000,Z/1000000}.
	

%% pass a tuple of lists and it will upscale the values
list_decode({ListA, ListB}) ->
	{list_decode(ListA),list_decode(ListB)};

%% decodes a list back into doubles from ints
list_decode(List) ->
    lists:map(fun(X) -> X/1000000 end, List).


%% @doc Stops the server
%% @spec stop(pid()) -> ok
stop(Pid) ->
    Pid ! stop,
	ok.


%% @doc Passes a message and result is passed back to Caller.
%% @spec (pid(), pid(), string(), any()) -> {return, Message::string()} | {error, atom()}
call_port(DriverPid, Caller, RobotId, Message) ->
	?DBUG("sending message to driver"),
	DriverPid ! {call, Caller, RobotId, integer_to_float(Message)}.

	
%% Converts all integers inside of the tuple to floats because we can only send floats to EI
%% Will recursivly affect all the nested tuples, but not the lists
integer_to_float(Input) when is_tuple(Input) ->
	%not very good readble because we need not to process nested lists (otherwise String will be corrupted)
	%otherwise stings will be corrupted
	erlang:list_to_tuple(
		lists:map(fun(X) -> integer_to_float(X) end, erlang:tuple_to_list(Input))
	);

%% Converts a single integer to a float
integer_to_float(Input) when erlang:is_integer(Input) ->
	Input*1.0;

%% This is the case when the input is neither a tuple, nor a integer -> return the input
integer_to_float(Input) ->
	Input.
