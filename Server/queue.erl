-module(queue).
-export([queueInit/0, queues/0, queues/1, join/2, leave/1]).

% -----------------------------------
%% queues
% -----------------------------------

queueInit() ->
    Queues = #{},
    queueLoop(Queues).

join(User, Level) ->
    Queue = maps:get(Level, ?MODULE:queues(), []),
    case length(Queue) of
        2 ->
            io:format("Queue for level ~p is full.~n", [Level]);
        1 ->
            {UN, Level,From, Server} = User,
            
            Server ! {start,From},
            UpdatedQueues = maps:remove(Level, ?MODULE:queues()),
            ?MODULE:queues(UpdatedQueues);
        _ ->
            NewQueue = [User | Queue],
            UpdatedQueues = maps:put(Level, NewQueue, ?MODULE:queues()),
            ?MODULE:queues(UpdatedQueues)
    end.

leave(User) ->
    Queues = ?MODULE:queues(),
    UpdatedQueues = maps:map(fun(Level, Queue) -> {Level, lists:delete(User, Queue)} end, Queues),
    ?MODULE:queues(UpdatedQueues).


queueLoop(Queues) ->
    receive
        {join, User, Level} ->
            join(User, Level),
            queueLoop(Queues);
        {leave, User} ->
            leave(User),
            queueLoop(Queues)
    end.


queues() ->
    maps:get(?MODULE, erlang:get(?MODULE), #{}).

queues(Queues) ->
    erlang:put(?MODULE, Queues).
