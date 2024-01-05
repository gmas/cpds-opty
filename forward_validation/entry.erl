-module(entry).
-export([new/1]).

new(Value) ->
    spawn_link(fun() -> init(Value) end).

init(Value) ->
    entry(Value, [], true).

entry(Value, ReadsList, Unlocked) ->
    receive
	    lock when Unlocked ->
	        entry(Value, ReadsList, false);
	    unlock when not Unlocked->
	        entry(Value, ReadsList, true);
        {read, Ref, From} when Unlocked ->
            L = lists:append(ReadsList, [From]),
            From ! {readack, Ref, self(), Value},
            entry(Value, L, Unlocked);
        {write, New} when Unlocked ->
            entry(New, [], Unlocked);
        {check, Ref, _, From} ->
            if
                length(ReadsList) == 0 ->
				    From ! {Ref, ok};
		        true ->
		            From ! {Ref, abort}
	        end,
            entry(Value, ReadsList, Unlocked);
	    {unread, From} ->
	        L = unread(From, ReadsList),
	        entry(Value, L, Unlocked);
        stop ->
            ok
    end.

unread(From, ReadsList) ->
    lists:filter(fun(X) -> X /= From end, ReadsList).
