-module(spacemachine).
-compile(export_all).

start(What, Options) ->
    ID = newid(),
    Pid = spawn(spacemachine, What, [Options, ID]),
    {ID, Pid}.

%% Models the world and all the objects in it.
world(Options, MyID, State) ->
    io:format("Loop~n"),
    receive
	{newobj, {ID, Pid}} ->
	    world(Options, MyID, dict:store(ID, Pid, State));

	{reload, Pid, all} ->
	    spacemachine:world(Options, MyID, State),
	    Pid ! {reloaded, done},
	    world(Options, MyID, restart_objects(State));

	{remove, ID} ->
	    world(Options, MyID, dict:erase(ID, State))
    end.

%% Helper method to restart objects and reload their code.
restart_objects(Objects) ->
    dict:map(
      fun(_, V) ->
	      V ! {reload, self()},
	      receive
		  {reloaded, done} ->
		      ok
	      after
		  10000 ->
		      error
	      end
      end, Objects).

%% Starts the world
start_world(Options) ->
    register(idbroker, spawn(spacemachine, idbroker, [0])),
    Dict = dict:new(),
    register(world, spawn(spacemachine, world, [Options, newid(), Dict])).

%% Creates a new object in the world, allocates it an ID and registers
%% the ID in the state of the World.
object(Options, MyID) ->
    io:format("Loop~n"),
    receive
	{world, Message} ->
	    object(Options, MyID);
	{reload, Pid} ->
	    Pid ! {reloaded, done},
	    spacemachine:object(Options, MyID)
    end.

register_object({ID, Pid}) ->
    world ! {newobj, {ID, Pid}}.

newobj(Options) ->
    {ID, Pid} = start(object, Options),
    register_object({ID, Pid}),
    Pid.

idbroker(N) ->
    receive
	{newid, Pid} ->
	    Pid ! {newid, N},
	    idbroker(N+1)
    end.

newid() ->
    idbroker ! {newid, self()},
    receive
	{newid, N} ->
	    ok
    end,
    N.
