%%% @doc
%%% Zero Latency On Update Cache Library.

%%% @author Aleksey Morarash <aleksey.morarash@gmail.com>
%%% @since 13 Dec 2014
%%% @copyright 2014, Aleksey Morarash <aleksey.morarash@gmail.com>

-module(zloca).

%% API exports
-export(
   [start_link/2,
    get/2,
    get_only_if_cached/2,
    get_only_if_cached_with_ttl/2,
    inject/4,
    update/1
   ]).

-include("zloca.hrl").

%% --------------------------------------------------------------------
%% Data Type Definitions
%% --------------------------------------------------------------------

-export_type(
   [backend_fun/0,
    key/0,
    value/0,
    ttl/0,
    server_name/0
   ]).

-type backend_fun() ::
        fun((key()) -> {ok, value()} |
                       {ok, value(), ttl()}).

-type key() :: any().

-type value() :: any().

-type ttl() :: timeout().

-type server_name() :: atom().

%% --------------------------------------------------------------------
%% API functions
%% --------------------------------------------------------------------

%% @doc Start a Zloca Cache server as a named linked process.
-spec start_link(ServerName :: server_name(),
                 BackendFun :: backend_fun()) ->
                        {ok, Pid :: pid()} |
                        {error, Reason :: any()}.
start_link(ServerName, BackendFun)
  when is_atom(ServerName), is_function(BackendFun, 1) ->
    zloca_srv:start_link(ServerName, BackendFun).

%% @doc Query the cache for a keyed value.
-spec get(ServerName :: server_name(), Key :: key()) ->
                 Value :: value().
get(ServerName, Key) ->
    zloca_srv:get(ServerName, Key).

%% @doc Query the cache for a keyed value. Do not real query if
%% the value does not cached yet but immediately return 'undefined'.
-spec get_only_if_cached(ServerName :: server_name(),
                         Key :: key()) ->
                                {ok, Value :: value()} |
                                undefined.
get_only_if_cached(ServerName, Key) ->
    case get_only_if_cached_with_ttl(ServerName, Key) of
        {ok, Value, _TTL} ->
            {ok, Value};
        undefined ->
            undefined
    end.

%% @doc Query the cache for a keyed value. Do not real query if
%% the value does not cached yet but immediately return 'undefined'.
-spec get_only_if_cached_with_ttl(ServerName :: server_name(),
                                  Key :: key()) ->
                                         {ok,
                                          Value :: value(),
                                          TTL :: ttl()} |
                                         undefined.
get_only_if_cached_with_ttl(ServerName, Key) ->
    zloca_srv:get_only_if_cached(ServerName, Key).

%% @doc Explicitly populate zloca cache with new item (or replace
%% existing one).
-spec inject(ServerName :: server_name(),
             Key :: key(), Value :: value(), TTL :: ttl()) -> ok.
inject(ServerName, Key, Value, TTL) ->
    zloca_srv:inject(ServerName, Key, Value, TTL).

%% @doc Update all accumulated values immediately disregard their TTL.
%% Note this will only start update process and function will return
%% earlier than all values will be updated.
-spec update(ServerName :: server_name()) -> ok.
update(ServerName) ->
    zloca_srv:update(ServerName).

%% ----------------------------------------------------------------------
%% Internal functions
%% ----------------------------------------------------------------------

%% ----------------------------------------------------------------------
%% EUnit tests
%% ----------------------------------------------------------------------

-ifdef(TEST).

-define(flatformat(F, A), lists:flatten(io_lib:format(F, A))).

-define(
   _assertLong(Server, Key, Expectation),
   {?flatformat("Assert long from ~9999p: ~9999p => ~9999p",
                [Server, Key, Expectation]),
    ?_test(
       begin
           {Time, Result} = timer:tc(?MODULE, get, [Server, Key]),
           ?assert(Time >= 10000),
           ?assert(Result == Expectation)
       end)}).

-define(
   _assertFast(Server, Key, Expectation),
   {?flatformat("Assert fast from ~9999p: ~9999p => ~9999p",
                [Server, Key, Expectation]),
    ?_test(
       begin
           {Time, Result} = timer:tc(?MODULE, get, [Server, Key]),
           ?assert(Time =< 1000),
           ?assert(Result == Expectation)
       end)}).

-define(
   _feedBackend(ETS, Tuples),
   {?flatformat("Feeding backend `~9999p` with ~9999p", [ETS, Tuples]),
    ?_assert(ets:insert(ETS, Tuples))}).

-define(
   _sleep(Seconds),
   {?flatformat("Sleep for ~w seconds...", [Seconds]),
    ?_assert(ok == timer:sleep(round(Seconds * 1000)))}).

main_test_() ->
    {setup,
     _SetupFun =
         fun() ->
                 s = ets:new(s, [named_table, public]),
                 BackendFun =
                     fun(k3) ->
                             {ok, v4, 1};
                        (Key) ->
                             ok = timer:sleep(100),
                             case ets:lookup(s, Key) of
                                 [{_, V}] ->
                                     {ok, V, 1};
                                 [] ->
                                     {ok, undefined, 1}
                             end
                     end,
                 {ok, _} = start_link(z, BackendFun)
         end,
     _CleanUpFun =
         fun(_) ->
                 ok
         end,
     {inorder,
      [
       ?_feedBackend(s, {k1, v1}),
       ?_assertMatch(undefined, get_only_if_cached(z, k1)),
       ?_assertLong(z, k1, v1),
       ?_assertFast(z, k1, v1),
       ?_assertMatch({ok, v1}, get_only_if_cached(z, k1)),
       ?_assertLong(z, k2, undefined),
       ?_assertFast(z, k2, undefined),
       ?_feedBackend(s, [{k1, v2}, {k2, v3}]),
       ?_assertFast(z, k1, v1),
       ?_assertFast(z, k2, undefined),
       ?_sleep(1.2),
       ?_assertFast(z, k1, v2),
       ?_assertFast(z, k2, v3),
       %% injection test
       ?_assertMatch(undefined, get_only_if_cached(z, k3)),
       ?_assertMatch(ok, inject(z, k3, v3, 1)),
       ?_assertFast(z, k3, v3),
       ?_sleep(1.2),
       ?_assertFast(z, k3, v4)
      ]}}.

-endif.
