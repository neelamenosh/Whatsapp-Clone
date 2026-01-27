%%%-------------------------------------------------------------------
%%% @doc User Connection Process
%%% gen_server managing a single user's WebSocket connection
%%% One process per connected device
%%% @end
%%%-------------------------------------------------------------------
-module(user_connection).
-behaviour(gen_server).

-include("records.hrl").

%% API
-export([start_link/2, send_message/2, get_state/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
         terminate/2, code_change/3]).

-define(HEARTBEAT_INTERVAL, 30000).  % 30 seconds

-record(state, {
    user_id :: binary(),
    socket :: any(),
    presence = online :: online | away | offline,
    active_calls = [] :: [binary()],
    device_info = #{} :: map(),
    heartbeat_ref :: reference() | undefined
}).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

-spec start_link(UserId :: binary(), Socket :: any()) -> {ok, pid()}.
start_link(UserId, Socket) ->
    gen_server:start_link(?MODULE, [UserId, Socket], []).

%% Send a message to this user's connection
-spec send_message(Pid :: pid(), Message :: map()) -> ok.
send_message(Pid, Message) ->
    gen_server:cast(Pid, {send_message, Message}).

%% Get current state of the connection
-spec get_state(Pid :: pid()) -> {ok, map()}.
get_state(Pid) ->
    gen_server:call(Pid, get_state).

%%--------------------------------------------------------------------
%% gen_server callbacks
%%--------------------------------------------------------------------

init([UserId, Socket]) ->
    process_flag(trap_exit, true),
    
    %% Register this connection in Mnesia
    ok = mnesia_store:register_user(UserId, self(), Socket),
    
    %% Start heartbeat timer
    HeartbeatRef = erlang:send_after(?HEARTBEAT_INTERVAL, self(), heartbeat),
    
    {ok, #state{
        user_id = UserId,
        socket = Socket,
        heartbeat_ref = HeartbeatRef
    }}.

handle_call(get_state, _From, State) ->
    Reply = #{
        user_id => State#state.user_id,
        presence => State#state.presence,
        active_calls => State#state.active_calls
    },
    {reply, {ok, Reply}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% Send message to client via WebSocket
handle_cast({send_message, Message}, State) ->
    send_to_socket(State#state.socket, Message),
    {noreply, State};

%% Handle incoming XMPP stanza
handle_cast({xmpp_stanza, Stanza}, State) ->
    NewState = handle_xmpp(Stanza, State),
    {noreply, NewState};

%% Update presence
handle_cast({set_presence, Presence}, State) ->
    ok = mnesia_store:update_presence(State#state.user_id, Presence),
    {noreply, State#state{presence = Presence}};

handle_cast(_Msg, State) ->
    {noreply, State}.

%% Heartbeat to check connection liveness
handle_info(heartbeat, State) ->
    %% Send ping to client
    send_to_socket(State#state.socket, #{type => <<"ping">>}),
    
    %% Schedule next heartbeat
    HeartbeatRef = erlang:send_after(?HEARTBEAT_INTERVAL, self(), heartbeat),
    
    %% Update last seen
    mnesia_store:update_last_seen(State#state.user_id),
    
    {noreply, State#state{heartbeat_ref = HeartbeatRef}};

%% Handle incoming WebSocket data
handle_info({ws_message, Data}, State) ->
    case parse_xmpp(Data) of
        {ok, Stanza} ->
            NewState = handle_xmpp(Stanza, State),
            {noreply, NewState};
        {error, _Reason} ->
            {noreply, State}
    end;

%% Handle socket close
handle_info({ws_closed, _Reason}, State) ->
    {stop, normal, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    %% Unregister from Mnesia
    mnesia_store:unregister_user(State#state.user_id),
    
    %% Cancel heartbeat
    case State#state.heartbeat_ref of
        undefined -> ok;
        Ref -> erlang:cancel_timer(Ref)
    end,
    
    %% End any active calls
    lists:foreach(fun(CallId) ->
        call_process:end_call(CallId, <<"user_disconnected">>)
    end, State#state.active_calls),
    
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

handle_xmpp(#xmpp_stanza{type = call_initiate} = Stanza, State) ->
    %% User initiating a call
    CallId = generate_call_id(),
    CalleeId = Stanza#xmpp_stanza.to,
    
    %% Start call process
    {ok, CallPid} = call_sup:start_call(CallId, State#state.user_id, CalleeId),
    
    %% Forward call initiation to callee
    case mnesia_store:get_user_pid(CalleeId) of
        {ok, CalleePid} ->
            call_process:set_caller_pid(CallPid, self()),
            user_connection:send_message(CalleePid, #{
                type => <<"call_initiate">>,
                call_id => CallId,
                from => State#state.user_id,
                call_type => maps:get(call_type, Stanza#xmpp_stanza.payload, <<"video">>)
            });
        {error, not_found} ->
            %% Callee not online - notify caller
            send_to_socket(State#state.socket, #{
                type => <<"call_failed">>,
                call_id => CallId,
                reason => <<"user_offline">>
            })
    end,
    
    State#state{active_calls = [CallId | State#state.active_calls]};

handle_xmpp(#xmpp_stanza{type = call_accept} = Stanza, State) ->
    CallId = Stanza#xmpp_stanza.call_id,
    
    %% Notify call process
    case mnesia_store:get_call(CallId) of
        {ok, CallSession} ->
            call_process:accept(CallId, self()),
            
            %% Notify caller
            case mnesia_store:get_user_pid(CallSession#call_session.caller_id) of
                {ok, CallerPid} ->
                    user_connection:send_message(CallerPid, #{
                        type => <<"call_accepted">>,
                        call_id => CallId
                    });
                _ -> ok
            end;
        _ -> ok
    end,
    
    State#state{active_calls = [CallId | State#state.active_calls]};

handle_xmpp(#xmpp_stanza{type = call_reject} = Stanza, State) ->
    CallId = Stanza#xmpp_stanza.call_id,
    call_process:reject(CallId),
    State;

handle_xmpp(#xmpp_stanza{type = call_end} = Stanza, State) ->
    CallId = Stanza#xmpp_stanza.call_id,
    call_process:end_call(CallId, <<"user_ended">>),
    State#state{active_calls = lists:delete(CallId, State#state.active_calls)};

handle_xmpp(#xmpp_stanza{type = ice_candidate} = Stanza, State) ->
    CallId = Stanza#xmpp_stanza.call_id,
    Candidate = maps:get(candidate, Stanza#xmpp_stanza.payload),
    call_process:add_ice_candidate(CallId, State#state.user_id, Candidate),
    State;

handle_xmpp(#xmpp_stanza{type = sdp_offer} = Stanza, State) ->
    CallId = Stanza#xmpp_stanza.call_id,
    Sdp = maps:get(sdp, Stanza#xmpp_stanza.payload),
    call_process:set_sdp_offer(CallId, Sdp),
    State;

handle_xmpp(#xmpp_stanza{type = sdp_answer} = Stanza, State) ->
    CallId = Stanza#xmpp_stanza.call_id,
    Sdp = maps:get(sdp, Stanza#xmpp_stanza.payload),
    call_process:set_sdp_answer(CallId, Sdp),
    State;

handle_xmpp(_Stanza, State) ->
    State.

send_to_socket(Socket, Message) ->
    %% Encode message as JSON and send via WebSocket
    Json = jsx:encode(Message),
    %% Socket send implementation depends on WebSocket library used
    catch websocket_handler:send(Socket, Json),
    ok.

parse_xmpp(Data) ->
    try
        Json = jsx:decode(Data, [return_maps]),
        Type = binary_to_atom(maps:get(<<"type">>, Json, <<"unknown">>), utf8),
        {ok, #xmpp_stanza{
            type = Type,
            from = maps:get(<<"from">>, Json, undefined),
            to = maps:get(<<"to">>, Json, undefined),
            call_id = maps:get(<<"call_id">>, Json, undefined),
            payload = Json
        }}
    catch
        _:_ -> {error, invalid_json}
    end.

generate_call_id() ->
    list_to_binary(uuid:uuid_to_string(uuid:get_v4())).
