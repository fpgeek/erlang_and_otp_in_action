-module(simple_cache_element_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_child/2]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, temporary, brutal_kill, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(Key, Value) ->
    supervisor:start_child(?MODULE, [Key, Value]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {simple_one_for_one, 0, 1}, [?CHILD(simple_cache_element, worker)]} }.
