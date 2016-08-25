%%% @doc
%%% Updates the contents of the cache's ETS table.

%%% @author Aleksey Morarash <aleksey.morarash@gmail.com>
%%% @since 13 Dec 2014
%%% @copyright 2014, Aleksey Morarash <aleksey.morarash@gmail.com>

-module(zloca_updator).

%% API exports
-export(
   [start_link/2,
    add/4,
    awake/1
   ]).

-include("zloca.hrl").

%% --------------------------------------------------------------------
%% Data type definitions
%% --------------------------------------------------------------------

-define(
   ADD(Key, LastUpdated, TTL),
   {add, Key, LastUpdated, TTL}).

-define(AWAKE, awake).

-record(
   state,
   {monitor :: reference(),
    server_name :: zloca:server_name(),
    backend_fun :: zloca_srv:canonic_backend_fun(),
    scheduled :: ordsets:ordset(zloca:key()),
    schedule :: ets:tab(),
    awaker :: timer:tref() | undefined
   }).

%% --------------------------------------------------------------------
%% API functions
%% --------------------------------------------------------------------

%% @doc Start the updator as linked process.
-spec start_link(ServerName :: zloca:server_name(),
                 BackendFun :: zloca_srv:canonic_backend_fun()) ->
                        pid().
start_link(ServerName, BackendFun) ->
    Owner = self(),
    spawn_link(
      fun() ->
              State = #state{
                monitor = monitor(process, Owner),
                server_name = ServerName,
                backend_fun = BackendFun,
                scheduled = ordsets:new(),
                schedule = ets:new(?MODULE, [ordered_set])
               },
              loop(State)
      end).

%% @doc Add a new item to the schedule.
-spec add(UpdatorPid :: pid(),
          Key :: zloca:key(),
          LastUpdated :: zloca_lib:micros(),
          TTL :: zloca:ttl()) ->
                 ok.
add(UpdatorPid, Key, LastUpdated, TTL) ->
    _Sent = UpdatorPid ! ?ADD(Key, LastUpdated, TTL),
    ok.

%% @doc Immediately start values update procedure.
-spec awake(UpdatorPid :: pid()) -> ok.
awake(UpdatorPid) ->
    _Sent = UpdatorPid ! ?AWAKE,
    ok.

%% ----------------------------------------------------------------------
%% Internal functions
%% ----------------------------------------------------------------------

%% @doc
-spec loop(#state{}) -> ok.
loop(State) ->
    receive
        {'DOWN', MonitorRef, process, _MasterPid, _Reason}
          when MonitorRef == State#state.monitor ->
            %% master process terminated, break the loop
            ok;
        ?ADD(Key, LastUpdated, TTL) ->
            loop(handle_add(State, Key, LastUpdated, TTL));
        ?AWAKE ->
            loop(handle_awake(State));
        _Other ->
            loop(State)
    end.

%% @doc
-spec handle_add(State :: #state{},
                 Key :: zloca:key(),
                 LastUpdated :: zloca_lib:micros(),
                 TTL :: zloca:ttl()) ->
                        NewState :: #state{}.
handle_add(State, Key, LastUpdated, TTL) ->
    case ordsets:is_element(Key, State#state.scheduled) of
        true ->
            %% already scheduled for update => nothing to do
            State;
        false ->
            %% Scheduling for update the new object
            NewScheduled =
                ordsets:add_element(Key, State#state.scheduled),
            Elapsed = micros_elapsed_to_update(State#state.schedule),
            DeadLine = LastUpdated + TTL * 1000000,
            true = ets:insert(State#state.schedule,
                              {DeadLine, Key}),
            case Elapsed of
                {ok, N} when N > DeadLine ->
                    %% Update time for the new object is
                    %% closer than previous update time =>
                    %% need to reschedule awake signal
                    schedule_awake(
                      State#state{scheduled = NewScheduled},
                      DeadLine - zloca_lib:micros());
                {ok, _N} ->
                    %% awake time is not changed
                    State#state{scheduled = NewScheduled};
                undefined ->
                    %% the very first object
                    schedule_awake(
                      State#state{scheduled = NewScheduled},
                      DeadLine - zloca_lib:micros())
            end
    end.

%% @doc
-spec handle_awake(State :: #state{}) -> NewState :: #state{}.
handle_awake(State) ->
    %% It is time to update!
    %% Collect the expired records from the schedule
    Now = zloca_lib:micros(),
    Expired = collect_expired(Now, State#state.schedule),
    %% Do the update
    NewState = update(State, Expired),
    %% Schedule next update
    case micros_elapsed_to_update(NewState#state.schedule) of
        {ok, Elapsed} ->
            schedule_awake(NewState, Elapsed);
        undefined ->
            discard_awake(NewState)
    end.

%% @doc Collect expired items from the schedule starting
%% from the first one, deleting them from the schedule.
-spec collect_expired(Now :: zloca_lib:micros(),
                      ETS :: ets:tab()) -> list().
collect_expired(Now, ETS) ->
    case ets:first(ETS) of
        '$end_of_table' ->
            [];
        DeadLine when DeadLine =< Now ->
            Key = ets:lookup_element(ETS, DeadLine, 2),
            true = ets:delete(ETS, DeadLine),
            [Key | collect_expired(Now, ETS)];
        _NotExpired ->
            []
    end.

%% @doc Update the values for expired keys, save the updates
%% into the cache table, reschedule updates for the new values
%% with TTL &lt; infinity.
-spec update(State :: #state{},
             Expired :: [zloca:key()]) ->
                    NewState :: #state{}.
update(State, Expired) ->
    ServerName = State#state.server_name,
    BackendFun = State#state.backend_fun,
    MasterPid = self(),
    %% Do the updates in parallel.
    TaskResults =
        [receive
             {Pid, TaskResult} ->
                 TaskResult
         end ||
            Pid <-
                [spawn_link(
                   fun() ->
                           %% Update the value
                           Item = BackendFun(Key),
                           %% Store the value in the cache
                           true = ets:insert(ServerName, Item),
                           %% Notify updator process
                           TaskResult =
                               {Key, Item#item.updated, Item#item.ttl},
                           _Sent = MasterPid ! {self(), TaskResult}
                   end) || Key <- Expired]],
    %% Filter out the items with TTL = infinity
    {ToBeScheduled, ToBeUnscheduled} =
        lists:foldl(
          fun({Key, _LastUpdated, infinity},
              {ToBeScheduled, ToBeUnscheduled}) ->
                  %% the new value for the key will never expire.
                  {ToBeScheduled, [Key | ToBeUnscheduled]};
             ({Key, LastUpdated, TTL},
              {ToBeScheduled, ToBeUnscheduled}) ->
                  DeadLine = LastUpdated + TTL * 1000000,
                  {[{DeadLine, Key} | ToBeScheduled],
                   ToBeUnscheduled}
          end, {[], []}, TaskResults),
    %% Add keys with mean TTL to the schedule
    true = ets:insert(State#state.schedule, ToBeScheduled),
    %% Unregister keys with TTL = infinity
    NewScheduled =
        lists:foldl(
          fun ordsets:del_element/2,
          State#state.scheduled,
          ToBeUnscheduled),
    State#state{scheduled = NewScheduled}.

%% @doc Micros elapsed to next update.
-spec micros_elapsed_to_update(ETS :: ets:tab()) ->
                            {ok, Micros :: integer()} |
                            undefined.
micros_elapsed_to_update(ETS) ->
    case ets:first(ETS) of
        '$end_of_table' ->
            undefined;
        DeadLine ->
            {ok, DeadLine - zloca_lib:micros()}
    end.

%% @doc Schedule awake signal after given amount of micros.
-spec schedule_awake(State :: #state{},
                     Micros :: integer()) ->
                            NewState :: #state{}.
schedule_awake(State, TooSmall) when TooSmall < 1000 ->
    NewState = discard_awake(State),
    %% time period is too small for timer:* functions,
    %% send the signal immediately:
    _Sent = self() ! ?AWAKE,
    NewState;
schedule_awake(State, Micros) ->
    NewState = discard_awake(State),
    Millis = round(Micros / 1000),
    {ok, TRef} = timer:send_after(Millis, ?AWAKE),
    NewState#state{awaker = TRef}.

%% @doc Discard awake signal, if scheduled.
-spec discard_awake(State :: #state{}) -> NewState :: #state{}.
discard_awake(State) when State#state.awaker /= undefined ->
    {ok, cancel} = timer:cancel(State#state.awaker),
    State#state{awaker = undefined};
discard_awake(State) ->
    %% not scheduled
    State.
