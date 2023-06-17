-module(server).
-import(files, [readAccounts/0, writeAccounts/1]).
-import(queue, [queueInit/0, join/2, leave/1]).
-import(game, [startGame/1, handleRemovePlayer/2, handleInput/2]).
-import(account, [createAccount/4, removeAccount/4, login/4, logout/4, auth/3]).
-export([start/1, queues/1, start/0, ca/2, ra/2, li/2, lo/1, jo/2, on/0, go/1, lb/0, sg/0]).



% -----------------------------------
% Test functions for server
% -----------------------------------

ca(A, B) -> ?MODULE ! {create_account, A, B, self()}, receive Res -> Res end.
ra(A, B) -> ?MODULE ! {remove_account, A, B, self()}, receive Res -> Res end.
li(A, B) -> ?MODULE ! {login, A, B, self()},          receive Res -> Res end.
jo(A, B) -> ?MODULE ! {join, A, B, self()},           receive Res -> Res end.
lo( A  ) -> ?MODULE ! {logout, A, self()},            receive Res -> Res end.
go( A  ) -> ?MODULE ! {gameover, A, self()},          receive Res -> Res end.
on(    ) -> ?MODULE ! {online, self()},               receive Map -> Map end.
lb(    ) -> ?MODULE ! {leaderboard, self()},          receive Map -> Map end.
sg(    ) -> ?MODULE ! {start, self()},                receive Res -> Res end.



% -----------------------------------
% Server
% -----------------------------------

start(Port) -> register(?MODULE, spawn(fun() -> server(Port) end)).
start() -> register(?MODULE, spawn(fun()-> server(8000) end)).

server(Port) ->
    ResTCP = gen_tcp:listen(Port, [binary, {packet, line}, {reuseaddr, false}]),
    case ResTCP of
        {error, Reason} -> io:fwrite("Error - Can't create tcp socket ~n\n"), Reason;
        {ok, LSock} ->
            spawn(fun() -> acceptor(LSock) end),
            Queues = spawn(fun() -> queues(#{}) end),
            serverLoop(files:readAccounts(), Queues, 0)
    end.


% -----------------------------------
%% Server Loop
% -----------------------------------

% serverLoop/2: The main server loop that handles incoming messages and manages the game server.
serverLoop(Users, Queues, OnGoing) ->
    receive
        {create_account, User, Password, From} ->
            NewUsers = account:createAccount(Users, User, Password, From),
            serverLoop(NewUsers, Queues, OnGoing);

        {remove_account, User, Password, From} ->
            NewUsers = account:removeAccount(Users, User, Password, From),
            serverLoop(NewUsers, Queues, OnGoing);

        {login, User, Password, From} ->
            NewUsers = account:login(Users, User, Password, From),
            serverLoop(NewUsers, Queues, OnGoing);

        {logout, User, From} ->
            NewUsers = account:logout(Users, User, From),
            serverLoop(NewUsers, Queues, OnGoing);

        {join, User, Password, From} ->
            case maps:find(User, Users) of
                {ok, {P, Level, _Victories, LoggedIn}} ->
                    if
                        OnGoing > 5 ->
                            From ! full_server;
                        (Password == P) and LoggedIn->
                            Queues ! {join, User, Level, From},
                            From ! {done, Level};
                        true->
                            From ! invalid_auth
                        end;                
                _ ->
                    From ! invalid_auth
            end,
            serverLoop(Users, Queues, OnGoing);
        {leave, User, From} ->
            case maps:find(User, Users) of
                {ok, {_P, Level, _Victories, _LoggedIn}} ->
                        Queues ! {leave, User, Level, From},
                        From ! leave_done;
                _->
                    From ! invalid_command
                end,
            serverLoop(Users, Queues, OnGoing);

        {start,_Level} ->
            io:fwrite("Game Started \n"),
            serverLoop(Users, Queues, OnGoing);
            
        {online, From} ->
            From ! [Username || {Username, {_Password, _Level, _Victories, LoggedIn}} <- maps:to_list(Users), LoggedIn],
            serverLoop(Users, Queues, OnGoing);

        {leaderboard, From} ->
            UserList = [{Username, (levelToVictories(Level) + _Victories)} || {Username, {_Password, Level, _Victories, _LoggedIn}} <- maps:to_list(Users)],
            SortedUserList = lists:sort(fun({_, V1}, {_, V2}) -> V1 > V2 end, UserList),
            SortedUsers = lists:sublist(SortedUserList, 10),
            From ! SortedUsers,
            serverLoop(Users, Queues, OnGoing);
        {gameoverL, Loser, From} ->io:format("Game Over Loser: ~p ~n", [Loser]), From ! loser;
        {gameoverW, Winner, From} ->
            From ! winner,
            io:format("Game Over Winner: ~p ~n", [Winner]),
            Res = maps:get(Winner, Users),
            case Res of
                {badkey, _} ->
                    serverLoop(Users, Queues , OnGoing - 1);
                {badmap, _} ->
                    serverLoop(Users, Queues , OnGoing - 1);
                {Password, Level, Victories, LoggedIn} ->
                    if
                        Victories + 1 == Level * 2 ->
                            NewUsers = maps:update(Winner, {Password, Level + 1, 0, LoggedIn}, Users),
                            files:writeAccounts(NewUsers),
                            serverLoop(NewUsers, Queues ,OnGoing - 1);
                        true ->
                            NewUsers = maps:update(Winner, {Password, Level, Victories + 1, LoggedIn}, Users),
                            files:writeAccounts(NewUsers),
                            serverLoop(NewUsers, Queues ,OnGoing - 1)
                    end
            end
    end.

% -----------------------------------
%% queues
% -----------------------------------

queues(Queues) ->
    % io:fwrite("queues users: "),
    % [io:format("~p ", [User]) || {User, _Pid} <- maps:to_list(Queues)],
    % io:fwrite("\n"),
    receive
        {join, User, Level, From} ->
            NewQueue = maps:get(Level, Queues, []),
            UpdatedQueue = [{User, From} | NewQueue],
            NewQueues = maps:put(Level, UpdatedQueue, Queues),
            
            io:fwrite("queues : ~p \n", [maps:to_list(NewQueues)]),
            case length(UpdatedQueue) of
                2 ->
                    ?MODULE! {start, Level},
                    spawn(fun() -> game:startGame(UpdatedQueue) end),
                    NQueues = maps:update(Level, [], NewQueues),
                    io:fwrite("queues : ~p \n", [maps:to_list(NQueues)]),
                    queues(NQueues);
                _ ->
                    queues(NewQueues)
            end;
        {leave, User, Level, _From} ->
            Queue = maps:get(Level, Queues, []),
            NewQueue = lists:keydelete(User, 1, Queue),
            NewQueues = maps:put(Level, NewQueue, Queues),
            queues(NewQueues)
    end.

% -----------------------------------
% Acceptor
% -----------------------------------

acceptor(LSock) ->
    ResTCP = gen_tcp:accept(LSock),
    spawn(fun() -> acceptor(LSock) end),
    case ResTCP of
        {error, Reason} -> io:fwrite("Error - Can't connect to tcp. ~p\n",[Reason]);
        {ok, Sock} -> client(Sock)
    end.


% -----------------------------------
% User data handler
% -----------------------------------

client(Sock) ->
    receive
        {tcp, _, Data} ->
            String = binary_to_list(string:trim(Data, trailing, "\n")),
            handleClientInput(String, Sock),
            client(Sock)
    end.

handleClientInput(String, Sock) ->
    case string:split(String, ":") of
        ["create_account", Info] ->
            [Username, Password] = string:split(Info, " "),
            ?MODULE ! {create_account, Username, Password, self()},
            receive
                done -> gen_tcp:send(Sock, "create_account:done\n");
                user_exists -> gen_tcp:send(Sock, "create_account:user_exists\n");
                invalid_password -> gen_tcp:send(Sock, "create_account:invalid_password\n")
            end;
        ["remove_account", Info] ->
            [Username, Password] = string:split(Info, " "),
            ?MODULE ! {remove_account, Username, Password, self()},
            receive
                done -> gen_tcp:send(Sock, "remove_account:done\n");
                invalid_account -> gen_tcp:send(Sock, "remove_account:invalid_account\n");
                invalid_password -> gen_tcp:send(Sock, "remove_account:invalid_password\n")
            end;
        ["logout", Username] ->
            ?MODULE ! {logout, Username, self()},
            receive
                done -> gen_tcp:send(Sock, "logout:done\n");
                invalid_account -> gen_tcp:send(Sock, "logout:invalid_account\n")
            end;
        ["login", Info] ->
            [Username, Password] = string:split(Info, " "),
            ?MODULE ! {login, Username, Password, self()},
            receive
                done -> gen_tcp:send(Sock, "login:done\n");
                user_logged_in-> gen_tcp:send(Sock, "login:invalid_account\n");
                invalid_account -> gen_tcp:send(Sock, "login:invalid_account\n");
                invalid_password -> gen_tcp:send(Sock, "login:invalid_password\n")
            end;
        ["leaderboard", _] ->
            ?MODULE ! {leaderboard, self()},
            receive
                Users -> 
                    UserList = [string:join([Username, integer_to_list(Victories)], " ") || {Username, Victories} <- Users],
                    % [io:format("~p ", [A]) || A <- Users],
                    Res = string:join(UserList, "|"),
                    io:format("~w ~p~n", [length(UserList), Res]),
                    gen_tcp:send(Sock, "leaderboard:"++string:join([Res, "\n"], ""))
            end;
        ["online", _] ->
            ?MODULE ! {online, self()},
            receive
                Users ->
                    Res = string:join(Users, " "),
                    io:format("~p~n", [Res]),
                    gen_tcp:send(Sock, string:join([Res, "\n"], ""))
            end;
        ["join", Info] ->
            io:fwrite("  in  request.\n"),
            [Username, Password] = string:split(Info, " "),
            ?MODULE ! {join, Username, Password, self()},
            receive
                {done, Level} ->
                    Message = "join:done " ++ integer_to_list(Level) ++ "\n",
                    gen_tcp:send(Sock, Message),
                    clientGame(Sock, Username);
                full_server ->
                    gen_tcp:send(Sock, "join:full_server x\n");
                invalid_auth ->
                    gen_tcp:send(Sock, "join:invalid_auth x\n")
            end;
        ["leave",Username] ->
            ?MODULE ! {leave, Username, self()},
            receive 
                done ->
                    gen_tcp:send(Sock, "leave:done\n"),
                    clientGame(Sock, Username);
                invalid_command ->
                    gen_tcp:send(Sock, "leave:invalid_command\n")
            end;
        _ -> io:fwrite("join:Incorrect syntax in tcp request.\n")
    end.

clientGame(Sock, Username) ->
    receive
        leave_done ->
            gen_tcp:send(Sock, "leave:done\n"),
            gen_tcp:send(Sock, "leave:done\n"),
            client(Sock);
        {tcp, _, Data} ->
            [DataString, _] = string:split(binary_to_list(Data), ":"),
            case DataString of
                "leave" ->
                    ?MODULE ! {leave, Username, self()}, 
                    clientGame(Sock, Username);
                _ -> io:fwrite("Incorrect syntax in tcp request.\n")
            end,
            clientGame(Sock, Username);
        {tcp_closed, _} ->
            ?MODULE ! {leave, Username, self()};
        {tcp_error, _} ->
            ?MODULE ! {leave, Username, self()};
        {start,Username, Game} ->
            io:fwrite("Game Started \n"),
            gen_tcp:send(Sock, "Play:start\n"),
            clientGameLoop(Sock, Username, Game);
        _ ->
            clientGame(Sock, Username)
    end.

clientGameLoop(Sock, Username, Game) ->
    receive
        winner ->
            ?MODULE ! {gameoverW, Username, self()},
            gen_tcp:send(Sock, "Game:winner\n"),
            client(Sock);
        defeat ->
            ?MODULE ! {gameoverL, Username, self()},
            gen_tcp:send(Sock, "Game:defeat\n"),
            client(Sock);
        {tcp, _, Data} ->
            DataString = string:trim(binary_to_list(Data), trailing, "\n"),
            case string:split(DataString, ":") of
                ["move", Info] ->
                    Game ! {move, Username, Info},
                    clientGameLoop(Sock, Username, Game);
                ["forfeit", _] -> game:handleRemovePlayer(Username, Game) , clientGameLoop(Sock, Username, Game)
            end,
            clientGameLoop(Sock, Username, Game);
        {tcp_closed, _} -> game:handleRemovePlayer(Username, Game), client(Sock);
        {tcp_error, _} -> game:handleRemovePlayer(Username, Game), client(Sock);
        Info ->
            GameInfo = "Game:" ++ Info,
            gen_tcp:send(Sock, GameInfo)
    end,
    clientGameLoop(Sock, Username, Game).



% -----------------------------------
% Auxiliary functions
% -----------------------------------

%Conversion of level to Number of Victories
levelToVictories(1) -> 0;
levelToVictories(N) when N > 1 -> 2 * N + levelToVictories(N - 1).







