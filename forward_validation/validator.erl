-module(validator).
-export([start/0]).

start() ->
    spawn_link(fun() -> init() end).

init()->
    validator().

validator() ->
    receive
        {validate, Ref, Reads, Writes, Client, From} ->
            Tag = make_ref(),
            lists:foreach(fun({_, Entry, _}) -> Entry ! lock end, Writes),
            lists:foreach(fun({_, Entry, _}) -> Entry ! {unread, From} end, Writes),
            send_write_check(Writes, From, Tag),  %% TODO: COMPLETE
            case check_writes(length(Writes), Tag) of  %% TODO: COMPLETE
                ok ->
                    update(Writes),  %% TODO: COMPLETE
                    lists:foreach(fun(Entry) -> Entry ! {unread, From} end, Reads),
                    Client ! {Ref, ok};
                abort ->
                    lists:foreach(fun(Entry) -> Entry ! {unread, From} end, Reads),
                    %% TODO: ADD SOME CODE
                    Client ! {Ref, abort}
            end,
            lists:foreach(fun({_, Entry, _}) -> Entry ! unlock end, Writes),
            validator();
        stop ->
            ok;
        _Old ->
            validator()
    end.

update(Writes) ->
    lists:foreach(fun({_, Entry, Value}) ->
                  %% TODO: ADD SOME CODE
                  Entry ! {write, Value}
                  end,
                  Writes).

send_write_check(Writes, From, Tag) ->
    Self = self(),
    lists:foreach(fun({_, Entry, _}) ->
                  %% TODO: ADD SOME CODE
                  Entry ! {check, Tag, From, Self}
                  end,
                  Writes).

check_writes(0, _) ->
    ok;
check_writes(N, Tag) ->
    receive
        {Tag, ok} ->
            check_writes(N-1, Tag);
        {Tag, abort} ->
            abort
    end.
