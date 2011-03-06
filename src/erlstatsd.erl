%%%----------------------------------------------------------------
%%% @author  Wil Tan <wil@dready.org>
%%%----------------------------------------------------------------
-module(erlstatsd).
-behaviour(gen_server).

%% External APIs
-export([start_link/1, timing/2, increment/1, decrement/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).




%% ===================================================================
%% API functions
%% ===================================================================
%%--------------------------------------------------------------------
%% @spec (Opts::[Host, Port]) -> {ok, Pid} | {error, Reason}
%
%% @doc Called by a supervisor to start the pool
%% @end
%%----------------------------------------------------------------------
start_link(Opts) when is_list(Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Opts, []).


timing(Stat, Time) ->
    gen_server:cast(?MODULE, {timing, Stat, Time}).

increment(Stat) ->
    gen_server:cast(?MODULE, {increment, Stat}).

decrement(Stat) ->
    gen_server:cast(?MODULE, {decrement, Stat}).



%%----------------------------------------------------------------------
%% @spec (Opts::[Host, Port]) -> {ok, State}           |
%%                            {ok, State, Timeout}  |
%%                            ignore                |
%%                            {stop, Reason}
%%
%% @doc Called by gen_server framework at process startup.
%% @end
%%----------------------------------------------------------------------
init([Host, Port]) ->
    process_flag(trap_exit, true),
    % create a socket
    {ok, Sock} = gen_udp:open(0),
    % return our state (which is just a triplet of sock-host-port)
    {ok, {Sock, Host, Port}}.


%%-------------------------------------------------------------------------
%% @spec (Request, From, State) -> {reply, Reply, State}          |
%%                                 {reply, Reply, State, Timeout} |
%%                                 {noreply, State}               |
%%                                 {noreply, State, Timeout}      |
%%                                 {stop, Reason, Reply, State}   |
%%                                 {stop, Reason, State}
%% @doc Callback for synchronous server calls.  If `{stop, ...}' tuple
%%      is returned, the server is stopped and `terminate/2' is called.
%% @end
%% @private
%%-------------------------------------------------------------------------
handle_call(Request, _From, State) ->
    Answer = do_request(Request, State),
    {reply, Answer, State}.


%%-------------------------------------------------------------------------
%% @spec (Msg, State) ->{noreply, State}          |
%%                      {noreply, State, Timeout} |
%%                      {stop, Reason, State}
%% @doc Callback for asyncrous server calls.  If `{stop, ...}' tuple
%%      is returned, the server is stopped and `terminate/2' is called.
%% @end
%% @private
%%-------------------------------------------------------------------------
handle_cast(Request, State) ->
    _Answer = do_request(Request, State),
    {noreply, State}.


%%-------------------------------------------------------------------------
%% @spec (Msg, State) ->{noreply, State}          |
%%                      {noreply, State, Timeout} |
%%                      {stop, Reason, State}
%% @doc Callback for messages sent directly to server's mailbox.
%%      If `{stop, ...}' tuple is returned, the server is stopped and
%%      `terminate/2' is called.
%% @end
%% @private
%%-------------------------------------------------------------------------
handle_info(Info, State) ->
    error_logger:error_msg("erlstatsd:handle_info got strange message ~p", [Info]),
    {noreply, State}.



%%-------------------------------------------------------------------------
%% @spec (Reason, State) -> any
%% @doc  Callback executed on server shutdown. It is only invoked if
%%       `process_flag(trap_exit, true)' is set by the server process.
%%       The return value is ignored.
%% @end
%% @private
%%-------------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.


%%-------------------------------------------------------------------------
%% @spec (OldVsn, State, Extra) -> {ok, NewState}
%% @doc  Convert process state when code is changed.
%% @end
%% @private
%%-------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%%------------------------------------------------------------------------
%%% Internal functions
%%%------------------------------------------------------------------------

do_request({timing, Stat, Time}, {Sock, Host, Port}) ->
    gen_udp:send(Sock, Host, Port,
                 io_lib:format("~s:~p|ms", [Stat, Time]));

do_request({increment, Stat}, {Sock, Host, Port}) ->
    gen_udp:send(Sock, Host, Port,
                 Stat ++ ":1|c");

do_request({decrement, Stat}, {Sock, Host, Port}) ->
    gen_udp:send(Sock, Host, Port,
                 Stat ++ ":-1|c").
