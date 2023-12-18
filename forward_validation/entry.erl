-module(entry).
-export([new/1]).

new(Value) ->
    spawn_link(fun() -> init(Value) end).

init(Value) ->
    entry(Value, [], true).

entry(Value, ReadsList, Locked) ->
    receive
	    lock when Locked ->
	        entry(Value, ReadsList, false);
	    unlock when not Locked->
	        entry(Value, ReadsList, true);
        {read, Ref, From} when Locked ->
            L = lists:append(ReadsList, [From]),
            From ! {readack, Ref, self(), Value},
            entry(Value, L, Locked);
        {write, New} when Locked ->
            entry(New, [], Locked);
        {check, Ref, _, From} ->
            if
                length(ReadsList) == 0 ->
				    From ! {Ref, ok};
		        true ->
		            From ! {Ref, abort}
	        end,
            entry(Value, ReadsList, Locked);
	    {unread, From} ->
	        L = unread(From, ReadsList),
	        entry(Value, L, Locked);
        stop ->
            ok
    end.

unread(From, ReadsList) ->
    lists:filter(fun(X) -> X /= From end, ReadsList).
