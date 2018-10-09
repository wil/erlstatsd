-module(erlstatsd).
-behaviour(gen_server).

%% External APIs
-export([start_link/1, increment/3, decrement/3, timing/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {socket, host, port}).

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

increment(Key, Magnitude, SampleRate) ->
    gen_server:cast(?MODULE, {sample, {increment, Key, Magnitude, SampleRate}}).

decrement(Key, Magnitude, SampleRate) ->
    gen_server:cast(?MODULE, {sample, {decrement, Key, Magnitude, SampleRate}}).

timing(Key, Value, SampleRate) ->
    gen_server:cast(?MODULE, {sample, {timing, Key, Value, SampleRate}}).

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
    {ok, Socket} = gen_udp:open(0),
    {ok, #state{socket=Socket, host=Host, port=Port}}.

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
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%-------------------------------------------------------------------------
%% @spec (Msg, State) ->{noreply, State}          |
%%                      {noreply, State, Timeout} |
%%                      {stop, Reason, State}
%% @doc Callback for asyncrous server calls.  If `{stop, ...}' tuple
%%      is returned, the server is stopped and `terminate/2' is called.
%% @end
%% @private
%%-------------------------------------------------------------------------
handle_cast({sample, {Operation, Key, Value, SampleRate}}, State) ->
    case rand:uniform() =< SampleRate of
        true ->
            handle_cast({Operation, Key, Value, SampleRate}, State);
        false ->
            {noreply, State}
    end;
handle_cast({increment, Key, Magnitude, SampleRate}, State) ->
    handle_cast({send, io_lib:format("~s:~B|c|@~f", [Key, Magnitude, SampleRate])}, State);
handle_cast({decrement, Key, Magnitude, SampleRate}, State) ->
    handle_cast({send, io_lib:format("~s:-~B|c|@~f", [Key, Magnitude, SampleRate])}, State);
handle_cast({timing, Key, Value, SampleRate}, State) ->
    handle_cast({send, io_lib:format("~s:~B|ms|@~f", [Key, Value, SampleRate])}, State);
handle_cast({send, Stats}, #state{socket=Socket, host=Host, port=Port}=State) ->
    gen_udp:send(Socket, Host, Port, Stats),
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
handle_info(_Info, State) ->
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
