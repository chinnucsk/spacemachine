-module(spacemachine).
-compile(export_all).

start(What, Options) ->
    ID = newid(),
    Pid = spawn(?MODULE, What, [Options, ID]),
    {ID, Pid}.

%% Models the world and all the objects in it.
world(Options, MyID, State) ->
    receive
	{newobj, {ID, Pid}} ->
	    world(Options, MyID, dict:store(ID, Pid, State));

	{reload, Pid, all} ->
	    Pid ! {reloaded, done},
	    ?MODULE:world(Options, MyID, restart_objects(State));

	{remove, ID} ->
	    case dict:find(ID, State) of
		{ok, Value} ->
		    Value ! {die, self()},
		    world(Options, MyID, dict:erase(ID, State));
		error ->
		    world(Options, MyID, State)
	    end;

	{running, Pid} ->
	    Pid ! {true, running},
	    world(Options, MyID, State)    
    end.

%% Checks if the world is running and returns a boolean respective of
%% that.
running(Who) ->
    try
	Who ! {running, self()},
    receive
	{true, running} ->
	    true
    after
	1000 ->
	    false
    end
	catch
	    error:badarg ->
		  false
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
		  100 ->
		      error
	      end
      end, Objects),
    Objects.

%% Supervises the world process and restarts the process if required.
world_supervisor(Options) ->

    %% trap dem exits
    process_flag(trap_exit, true),
    Dict = dict:new(),
    register(world, spawn_link(?MODULE, world, [Options, newid(), Dict])),
    
    receive
	_ ->
	    io:format("Caught exit~n"),
	    world_supervisor(Options)	    
    end.
    

%% Starts the world
start_world(Options) ->
    case running(idbroker) of
	true ->
	    ok;
	false ->
	    register(idbroker, spawn(?MODULE, idbroker, [0]))
    end,
    case running(world) of
	true ->
	    ok;
	false ->
	    spawn(?MODULE, world_supervisor, [Options])
    end,
    sleep(1),
    running(idbroker) and running(world).

%% Creates a new object in the world, allocates it an ID and registers
%% the ID in the state of the World.
object(Options, MyID) ->
    receive
	{world, Message} ->
	    object(Options, MyID);
	{reload, Pid} ->
	    Pid ! {reloaded, done},
	    ?MODULE:object(Options, MyID)
    end.

register_object({ID, Pid}) ->
    world ! {newobj, {ID, Pid}}.
unregister_object(ID) ->
    world ! {remove, ID}.

newobj(Options) ->
    {ID, Pid} = start(object, Options),
    register_object({ID, Pid}),
    Pid.

idbroker(N) ->
    receive
	{newid, Pid} ->
	    Pid ! {newid, N},
	    idbroker(N+1);
	{running, Pid} ->
	    Pid ! {true, running},
	    idbroker(N)
    end.

newid() ->
    idbroker ! {newid, self()},
    receive
	{newid, N} ->
	    ok
    end,
    N.

reset_world() ->
    world ! {reload, self(), all},
    receive
	{reloaded, done} ->
	    ok
    end.

sleep(N) ->
    receive
    after
	N ->
	    ok
    end.
