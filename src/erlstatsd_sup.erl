-module(erlstatsd_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Opts), {I, {I, start_link, [Opts]}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    {Host, Port} = get_hostport(),
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Host, Port]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([Host, Port]) ->
    {ok, { {one_for_one, 5, 10}, [?CHILD(erlstatsd, worker, [Host, Port])]} }.

%% ===================================================================
%% Internal
%% ===================================================================
get_hostport() ->
    Opts = application:get_all_env(),
    {proplists:get_value(statsd_host, Opts, "localhost"),
     proplists:get_value(statsd_port, Opts, 8125)}.

