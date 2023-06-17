-module(game).
-import(geometry, [dist/2, insideCircle/2, crystalInsideCircle/2]).
-export([ startGame/1, handleRemovePlayer/2]).

-define(BASE_ANGULAR_VELOCITY, 0.20).  % Base value for angular velocity
-define(BASE_LINEAR_ACCELERATION, 1.23).  % Base value for linear acceleration



% -----------------------------------
%% Game
% -----------------------------------


startGame(Queue) ->
    Players = lists:sublist(Queue, 2),
    io:format("Starting game with players: ~p~n", [Players]),
    io:fwrite("Starting game with players: ~p~n", [Players]),
    game(Players).
    
game(Players) ->
    [From ! {start, User, self()} || {User, From} <- Players],
    NewPlayers = initGame(Players, []),
    spawn(fun() -> receive after 120000 -> self() ! endGame end end),
    gameTimer(NewPlayers, []).

initGame([], _UsedPositions) -> #{};
initGame([{Player, From}| Players], UsedPositions) ->
    X = float(rand:uniform(400)),
    Y = float(rand:uniform(400)),

    case [{Player, From}| Players] of
        [{Player, From}| []] -> Direction = math:pi();
        _ -> Direction = 0.1
    end,
    case lists:member({X, Y}, UsedPositions) of
        false ->
            Pos = {X+0.1, Y+0.1},
            PlayerMap = initGame(Players, [Pos | UsedPositions]),
            maps:put(Player, {From, Pos, ?BASE_LINEAR_ACCELERATION, ?BASE_ANGULAR_VELOCITY, Direction, 0}, PlayerMap);
        true ->
            initGame([{Player, From} | Players], UsedPositions)
    end.


generateCrystals(Crystals) ->
    Number = rand:uniform(100),
    if
        Number == 38 ->
            CrystalsNumber = rand:uniform(5),
            generateCrystalsN(CrystalsNumber) ++ Crystals;
        true ->
            Crystals
    end.


generateCrystalsN(Number) ->
    case Number of 
        0 -> [];
        _ ->
        X = rand:uniform(790) + -0.1 ,
        Y = rand:uniform(790) + -0.1 ,
        case rand:uniform(3) of
            1 -> Color = red;
            2 -> Color = green;
            3 -> Color = blue
        end,
        [{X, Y, Color} | generateCrystalsN(Number-1)]
    end.

%Timer
gameTimer(Players, Crystals) ->
    Self = self(),
    spawn(fun() -> receive after 40 -> Self ! timeout end end), % tickrate
    {NewPlayers, TempCrystals} = handleGame(Players, Crystals),
    NewCrystals = generateCrystals(TempCrystals),
    PlayerInfo = parseGame(maps:to_list(NewPlayers), []),
    case NewCrystals of
        [] -> Info = string:concat(PlayerInfo, "\n");
        _ ->
            CrystalInfo = parseCrystals(NewCrystals, []),
            Info = string:join([PlayerInfo, CrystalInfo], "|")
    end,
    lists:foreach(fun({_Player, {From, _Pos, _Acceleration, _RotationSpeed, _Direction, _Score}}) -> From ! Info end, maps:to_list(NewPlayers)),
    gameLoop(NewPlayers, NewCrystals).


gameLoop(Players, Crystals) ->
    NPlayers = Players,
    receive
        endGame ->
            handleEndGame(maps:to_list(Players));
        timeout ->
            gameTimer(Players, Crystals);
        {leave, Username} ->
            handleEndGame(maps:to_list(NPlayers), Username);
        {move, Username, Info} ->
            NewPlayers = handleInput(Players, Username, Info),
            gameLoop(NewPlayers, Crystals)
    end.

handleEndGame(Players)->
    [{_UP1, {From, {_X, _Y}, _Acceleration, _RotationSpeed, _Direcction, Score1}} | P2] = Players,
    [{_UP2, {From2, {_X2, _Y2}, _Acceleration2, _RotationSpeed2, _Direcction2, Score2}} | _] = P2,
    if 
        Score1 > Score2 -> From ! winner, From2 ! defeat;
        Score1 < Score2 -> From2 ! winner, From ! defeat;
        true-> From ! winner, From2 ! defeat
    end,
    exit(kill).


handleEndGame(Players,Username)->
    [{UP1, {From, {_X, _Y}, _Acceleration, _RotationSpeed, _Direcction, _Score1}} | P2] = Players,
    [{_UP2, {From2, {_X2, _Y2}, _Acceleration2, _RotationSpeed2, _Direcction2, _Score2}} | _] = P2,
    if 
        Username == UP1 -> From ! winner, From2 ! defeat;
        true -> From2 ! winner, From ! defeat
    end,
    exit(kill).


handleRemovePlayer(Username, Game) -> Game ! {leave, Username}.




handleInput(Players, Username, Info) ->
    [Move,Rotation] = string:split(Info, " "), 
    Res = maps:get(Username, Players),
    case Res of
        {badmap, _} -> Players;
        {badkey, _} -> Players;
        {From, {OldX, OldY}, Acceleration, RotationSpeed, Direction, Score} ->
            case Rotation of
                "a" ->
                    NewDirection =  Direction + RotationSpeed;

                "d" ->
                    NewDirection = Direction - RotationSpeed;
    
                _ ->
                    NewDirection = Direction
            end,
            {DirX, DirY} = angle_to_direction(NewDirection),
        
            if
                Move =="w" ->
                    {X, Y} = {OldX + (DirX * Acceleration), OldY + (DirY * Acceleration)};
                true ->
                    {X, Y} = {OldX , OldY }
                end,

            if
                X<0 orelse X>800 orelse Y<0 orelse Y>800->
                    Self = self(),
                    Self ! {leave, Username};
                true ->
                maps:update(Username, {From, {X, Y}, Acceleration, RotationSpeed, NewDirection , Score}, Players)
            end
    end.

handleGame(Players, Crystals) ->
    CrystalCollisions = findCrystalCollisions(Crystals, maps:to_list(Players)),
    PlayerCollisions  = findColisions(maps:to_list(Players)),
    {NewPlayers, FinalCrystals} = handleCrystalCollisions(CrystalCollisions, Players, Crystals),
    FinalPlayers = handlePlayerCollisions(PlayerCollisions, NewPlayers),
    {FinalPlayers, FinalCrystals}.

% -----------------------------------
%% Collisions
% -----------------------------------


handleCrystalCollisions([], Players, Crystals) -> {Players, Crystals};
handleCrystalCollisions([{Player, X, Y, Color} | T], Players, Crystals) ->
    {NewPlayers, NewCrystals} = handleCrystalCollisions(T, Players, Crystals),
    {From, Pos, Acceleration, RotationSpeed, Direction, _Score} = maps:get(Player, NewPlayers),
    case Color of
        red-> 
            NewAcceleration = ?BASE_LINEAR_ACCELERATION,
            NewRotationSpeed = ?BASE_ANGULAR_VELOCITY;
        green->
            NewAcceleration = Acceleration,
            if 
                RotationSpeed >= 5* ?BASE_ANGULAR_VELOCITY -> NewRotationSpeed = RotationSpeed;
                true -> NewRotationSpeed = RotationSpeed + ?BASE_ANGULAR_VELOCITY
            end;
        blue->
            if 
                Acceleration >= 5* ?BASE_ANGULAR_VELOCITY -> NewAcceleration = Acceleration;
                true -> NewAcceleration = Acceleration + ?BASE_ANGULAR_VELOCITY
            end,
            NewRotationSpeed = RotationSpeed
        end,

    {maps:update(Player, {From, Pos, NewAcceleration, NewRotationSpeed, Direction, _Score}, NewPlayers), lists:delete({X, Y, Color},NewCrystals)}.

handlePlayerCollisions({}, Players) -> Players;
handlePlayerCollisions({P1, P2}, Players) ->
    {_From1, {X1, Y1}, _Acceleration1, _RotationSpeed1, Direction1, Score1} = maps:get(P1, Players),
    {_From2, _Pos2, _Acceleration2, _RotationSpeed2, _Direction2, Score2} = maps:get(P2, Players),
    X = rand:uniform(700) +20.1,
    Y = rand:uniform(700) + 20.1,
    NPlayers = maps:update(P2, {_From2, {X , Y}, ?BASE_LINEAR_ACCELERATION, ?BASE_ANGULAR_VELOCITY, 0.1, Score2}, Players),
    NewPlayers = maps:update(P1, {_From1, { X1, Y1}, ?BASE_LINEAR_ACCELERATION, ?BASE_ANGULAR_VELOCITY, Direction1, Score1 + 1}, NPlayers),
    NewPlayers.
    



% -----------------------------------
%% Find Collision
% -----------------------------------

findCrystalCollisions([], _Players) -> [];
findCrystalCollisions([H | T], Players) -> findCrystalCollisionsAux(H, Players) ++ findCrystalCollisions(T, Players).

findCrystalCollisionsAux(_C, []) -> [];
findCrystalCollisionsAux(C, [H | T]) ->
    {CX, CY, CColor} = C,
    {Player, {_From, HPos, _Acceleration, _RotationSpeed, _Direction, _Score}} = H,
    case geometry:crystalInsideCircle({{CX, CY}, 15}, {HPos, 60}) of
        true -> [{Player, CX, CY, CColor} | findCrystalCollisionsAux(C, T)];
        _ -> findCrystalCollisionsAux(C, T)
    end.


%findColisions([]) -> [];
%findColisions(_P, []) -> [];

findColisions([H|T]) -> findColisions(H, T).
findColisions(P, [H | _]) ->

    {CPlayer, {_CFrom, CPos, _CAcceleration, _CRotationSpeed, CDir, _CScore}} = P,
    {HPlayer, {_HFrom, HPos, _HAcceleration, _HRotationSpeed, HDir, _HScore}} = H,
    NCDir = dimensionality(CDir),
    NHDir = dimensionality(HDir),
    case geometry:insideCircle({CPos, 30}, {HPos, 30}) of
        true -> 
            Angle = (NCDir * NHDir) / (math:sqrt(NCDir * NCDir) * math:sqrt(NHDir * NHDir)),
            case Angle < math:pi() / 2 of
                true ->
                    {CPlayer, HPlayer};  % Player 1 wins - Return Player 1's coordinates
                false ->
                    {HPlayer, CPlayer}   % Player 2 wins - Return Player 2's coordinates
                end;
        _ -> {}
    end.





parseGame([], List) -> string:join(List, "|");
parseGame([{Player, {_From, {X, Y}, _Acceleration, _RotationSpeed, Direction, Score}} | Tail], List) ->
    InfoPlayer = string:join([Player, float_to_list(X), float_to_list(Y), float_to_list(Direction), integer_to_list(Score)], " "),
    parseGame(Tail, [InfoPlayer | List]).

parseCrystals([], List) -> string:concat(string:join(List, "|"), "\n");
parseCrystals([{X, Y, Color} | Tail], List) ->
    InfoCrystal = string:join(["<>", atom_to_list(Color), float_to_list(X), float_to_list(Y)], " "),
    parseCrystals(Tail, [InfoCrystal | List]).



% -----------------------------------
%% AUX
% -----------------------------------


angle_to_direction(Angle) ->
    X = round(math:cos(Angle)),
    Y = round(math:sin(Angle)),
    {X, Y}.

dimensionality(Dir) ->
    Radians = math:pi() * 2 * Dir,
    case Radians >= 0 of
        true -> NRadians = Radians;
        false -> NRadians = Radians + (math:pi() * 2)
    end,
    math:fmod(NRadians, math:pi()*2).


%direction_to_angle({X, Y}) ->
%    Angle = math:atan2(Y, X),
%    Angle.