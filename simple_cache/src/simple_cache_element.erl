-module(simple_cache_element).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-define(DEFAULT_LEASE_TIME, 60 * 60 * 24).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
    start_link/2,
    create/1,
    create/2,
    fetch/1,
    replace/2,
    delete/1
]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {value, lease_time, start_time}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Value, LeaseTime) ->
    gen_server:start_link(?MODULE, [Value, LeaseTime], []).

create(Value, LeaseTime) ->
    simple_cache_element_sup:start_child(Value, LeaseTime).

create(Value) ->
    create(Value, ?DEFAULT_LEASE_TIME).

replace(Pid, Value) ->
    gen_server:cast(Pid, {replace, Value}).

fetch(Pid) ->
    gen_server:call(Pid, fetch).

delete(Pid) ->
    gen_server:cast(Pid, delete).


%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([Value, LeaseTime]) ->
    StartTime = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
    {ok, #state{value = Value, lease_time = LeaseTime, start_time = StartTime}, time_left(StartTime, LeaseTime)}.

handle_call(fetch, _From, State) ->
    #state{value = Value, lease_time = LeaseTime, start_time = StartTime} = State,
    TimeLeft = time_left(StartTime, LeaseTime),
    {reply, {ok, Value}, State, TimeLeft}.

handle_cast({replace, Value}, State) ->
    #state{lease_time = LeaseTime, start_time = StartTime} = State,
    TimeLeft = time_left(StartTime, LeaseTime),
    {noreply, State#state{value = Value}, TimeLeft};
handle_cast(delete, State) ->
    {stop, normal, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    simple_cache_store:delete(self()),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

time_left(_StartTime, infinity) ->
    infinity;
time_left(StartTime, LeaseTime) ->
    CurrnetTime = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
    TimeElapsed = CurrnetTime - StartTime,
    case LeaseTime - TimeElapsed of
        Time when Time =< 0 -> 0;
        Time                -> Time * 1000
    end.
