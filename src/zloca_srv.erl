%%% @doc
%%% Main activity.

%%% @author Aleksey Morarash <aleksey.morarash@gmail.com>
%%% @since 13 Dec 2014
%%% @copyright 2014, Aleksey Morarash <aleksey.morarash@gmail.com>

-module(zloca_srv).

-behaviour(gen_server).

%% API exports
-export(
   [start_link/2,
    get/2,
    get_only_if_cached/2
   ]).

%% gen_server callback exports
-export(
   [init/1, handle_call/3, handle_info/2, handle_cast/2,
    terminate/2, code_change/3]).

-include("zloca.hrl").

%% --------------------------------------------------------------------
%% Data type definitions
%% --------------------------------------------------------------------

-export_type(
   [canonic_backend_fun/0
   ]).

-record(
   state,
   {monitor :: reference(),
    backend :: zloca:backend_fun(),
    updator :: pid()}).

-type canonic_backend_fun() ::
        fun((zloca:key()) -> #item{}).

-define(CALL_GET_BACKEND, get_backend).
-define(
   CAST_SCHEDULE_UPDATE(Key, LastUpdated, TTL),
   {schedule_update, Key, LastUpdated, TTL}).

%% --------------------------------------------------------------------
%% API functions
%% --------------------------------------------------------------------

%% @doc Start a Zloca Cache server as a named linked process.
-spec start_link(ServerName :: zloca:server_name(),
                 BackendFun :: zloca:backend_fun()) ->
                        {ok, Pid :: pid()} |
                        {error, Reason :: any()}.
start_link(ServerName, BackendFun) ->
    OwnerPid = self(),
    gen_server:start_link(
      {local, ServerName}, ?MODULE,
      _Args = {OwnerPid, ServerName, BackendFun},
      _Options = []).

%% @doc Query the cache for a keyed value.
-spec get(ServerName :: zloca:server_name(), Key :: zloca:key()) ->
                 Value :: zloca:value().
get(ServerName, Key) ->
    case ets:lookup(ServerName, Key) of
        [R] ->
            R#item.value;
        [] ->
            %% the item is not known for the cache
            BackendFun = gen_server:call(ServerName, ?CALL_GET_BACKEND),
            true = ets:insert(ServerName, Item = BackendFun(Key)),
            if Item#item.ttl /= infinity ->
                    ok = gen_server:cast(
                           ServerName,
                           ?CAST_SCHEDULE_UPDATE(
                              Key, Item#item.updated, Item#item.ttl));
               true ->
                    ok
            end,
            Item#item.value
    end.

%% @doc Query the cache for a keyed value. Do not real query if
%% the value does not cached yet but immediately return 'undefined'.
-spec get_only_if_cached(ServerName :: zloca:server_name(),
                         Key :: zloca:key()) ->
                                {ok, Value :: zloca:value(),
                                 TTL :: zloca:ttl()} |
                                undefined.
get_only_if_cached(ServerName, Key) ->
    case ets:lookup(ServerName, Key) of
        [R] ->
            {ok, R#item.value, R#item.ttl};
        [] ->
            undefined
    end.

%% --------------------------------------------------------------------
%% gen_server callback functions
%% --------------------------------------------------------------------

%% @hidden
-spec init({OwnerPid :: pid(),
            ServerName :: zloca:server_name(),
            BackendFun :: zloca:backend_fun()}) ->
                  {ok, InitialState :: #state{}}.
init({OwnerPid, ServerName, BackendFun}) ->
    MonitorRef = monitor(process, OwnerPid),
    ServerName = ets:new(ServerName, [named_table, public, {keypos, 2}]),
    CanonicBackendFun = gen_canonic_backend_fun(BackendFun),
    UpdatorPid = zloca_updator:start_link(ServerName, CanonicBackendFun),
    {ok, #state{monitor = MonitorRef,
                backend = CanonicBackendFun,
                updator = UpdatorPid}}.

%% @hidden
-spec handle_info(Request :: any(), State :: #state{}) ->
                         {noreply, State :: #state{}}.
handle_info({'DOWN', MonitorRef, process, _OwnerPid, _Reason}, State)
  when MonitorRef == State#state.monitor ->
    {stop, normal, State};
handle_info(_Request, State) ->
    {noreply, State}.

%% @hidden
-spec handle_cast(Request :: any(), State :: #state{}) ->
                         {noreply, NewState :: #state{}}.
handle_cast(?CAST_SCHEDULE_UPDATE(Key, LastUpdated, TTL), State) ->
    ok = zloca_updator:add(State#state.updator, Key, LastUpdated, TTL),
    {noreply, State};
handle_cast(_Request, State) ->
    {noreply, State}.

%% @hidden
-spec handle_call(Request :: any(), From :: any(), State :: #state{}) ->
                         {noreply, NewState :: #state{}}.
handle_call(?CALL_GET_BACKEND, _From, State) ->
    {reply, State#state.backend, State};
handle_call(_Request, _From, State) ->
    {noreply, State}.

%% @hidden
-spec terminate(Reason :: any(), State :: #state{}) -> ok.
terminate(_Reason, _State) ->
    ok.

%% @hidden
-spec code_change(OldVersion :: any(), State :: #state{}, Extra :: any()) ->
                         {ok, NewState :: #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ----------------------------------------------------------------------
%% Internal functions
%% ----------------------------------------------------------------------

%% @doc Generates a functional object which takes a key, applies
%% the users backend function and creates an #item{} record, ready
%% to be inserted in the cache table.
-spec gen_canonic_backend_fun(zloca:backend_fun()) ->
                                     canonic_backend_fun().
gen_canonic_backend_fun(BackendFun) ->
    fun(Key) ->
            {Value, TTL} =
                case BackendFun(Key) of
                    {ok, Value0, TTL0} ->
                        {Value0, TTL0};
                    {ok, Value0} ->
                        {Value0, infinity}
                end,
            #item{key = Key, value = Value, ttl = TTL}
    end.
