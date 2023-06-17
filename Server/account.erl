-module(account).
-import(files,[readAccounts/0, writeAccounts/1]).
-export([createAccount/4, removeAccount/4, login/4, logout/3, auth/3]).




createAccount(Users, User, Password, From) ->
    case maps:is_key(User, Users) of
        true->
            NewUsers = Users,
            io:format("Error - User ~p already exists.~n", [User]),
            From ! user_exists;
        false ->
            if
                Password == "" -> 
                    NewUsers = Users,
                    io:format("Error - Cannot create user with empty password.~n"),
                    From ! invalid_password;
                true ->
                    NewUsers = maps:put(User, {Password, 1, 0, false}, Users),
                    io:format("Created an account with username ~p.~n", [User]),
                    files:writeAccounts(NewUsers),
                    From ! done
            end
    end,
    NewUsers.

removeAccount(Users, User, Password, From) ->
    case maps:find(User, Users) of
        {ok, {Pass, _Level, _Victories, _LoggedIn}} ->
            if
                Pass == Password ->
                    NewUsers = maps:remove(User, Users),
                    io:format("Removed the account with username ~p.~n", [User]),
                    files:writeAccounts(NewUsers),
                    From ! done;
                true ->
                    NewUsers = Users,
                    io:format("Error - Incorrect password for user ~p.~n", [User]),
                    From ! invalid_password
            end;
        _ ->
            io:format("Error - Cannot remove user ~p~n.", [User]),
            NewUsers = Users,
            From ! invalid_account
    end,
    NewUsers.

login(Users, U, P, From) ->
    case maps:find(U, Users) of
       {ok, {Password, Level, _Victories, LoggedIn}} ->
            if
                Password == P ->
                    if 
                        LoggedIn ->
                            From ! user_logged_in,
                            Users;
                        true ->
                        From ! done,
                        maps:update(U, {Password, Level, _Victories, true}, Users)
                    end;
                true ->
                    From ! invalid_password,
                    Users
            end;
        _ ->
           From ! invalid_account,
           Users
    end.

logout(Users, U, From) ->
    case maps:find(U, Users) of
        {ok, {Password, Level, _Victories, _LoggedIn}} ->
            From ! done,
            maps:update(U, {Password, Level, _Victories, false}, Users);

        _ ->
            From ! invalid_account,
            Users
    end.

auth(Users, U, P) ->
    case maps:find(U, Users) of
        {ok, {Password, _Level, _Victories, LoggedIn}} ->
            (Password == P) and LoggedIn;
        _ -> false
    end.
