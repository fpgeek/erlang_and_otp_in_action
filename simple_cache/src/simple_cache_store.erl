-module(simple_cache_store).
-define(TABLE_ID, ?MODULE).

-export([
    init/0,
    insert/2,
    delete/1,
    lookup/1
]).

init() ->
    ets:new(?TABLE_ID, [public, named_table]),
    ok.

insert(Key, Pid) ->
    ets:insert(?TABLE_ID, {Key, Pid}).

delete(Key) ->
    ets:match_delete(?TABLE_ID, {'_', Key}).

lookup(Key) ->
    case ets:lookup(?TABLE_ID, Key) of
        [{Key, Pid}]    -> {ok, Pid};
        []              -> {error, not_found}
    end.