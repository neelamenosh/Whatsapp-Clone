%%%-------------------------------------------------------------------
%%% @doc Call Process
%%% gen_server managing a single video/audio call
%%% One process per active call
%%% Handles complete call lifecycle
%%% @end
%%%-------------------------------------------------------------------
-module(call_process).
-behaviour(gen_server).

-include("records.hrl").

%% API
-export([start_link/3, accept/2, reject/1, end_call/2,
         add_ice_candidate/3, set_sdp_offer/2, set_sdp_answer/2,
         set_caller_pid/2, set_callee_pid/2, get_state/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(RING_TIMEOUT, 60000).      % 60 seconds to answer
-define(CALL_TIMEOUT, 3600000).    % 1 hour max call duration

-record(state, {
    call_id :: binary(),
    caller_id :: binary(),
    callee_id :: binary(),
    caller_pid :: pid() | undefined,
    callee_pid :: pid() | undefined,
    sdp_offer :: binary() | undefined,
    sdp_answer :: binary() | undefined,
    ice_candidates = [] :: [map()],
    call_type = video :: video | audio,
    call_state = initiating :: initiating | ringing | connecting | connected | ended,
    created_at :: integer(),
    ring_timer :: reference() | undefined,
    call_timer :: reference() | undefined
}).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

-spec start_link(CallId :: binary(), CallerId :: binary(), CalleeId :: binary()) -> 
    {ok, pid()}.
start_link(CallId, CallerId, CalleeId) ->
    gen_server:start_link(?MODULE, [CallId, CallerId, CalleeId], []).

-spec accept(CallId :: binary(), CalleePid :: pid()) -> ok.
accept(CallId, CalleePid) ->
    case get_call_pid(CallId) of
        {ok, Pid} -> gen_server:cast(Pid, {accept, CalleePid});
        _ -> ok
    end.

-spec reject(CallId :: binary()) -> ok.
reject(CallId) ->
    case get_call_pid(CallId) of
        {ok, Pid} -> gen_server:cast(Pid, reject);
        _ -> ok
    end.

-spec end_call(CallId :: binary(), Reason :: binary()) -> ok.
end_call(CallId, Reason) ->
    case get_call_pid(CallId) of
        {ok, Pid} -> gen_server:cast(Pid, {end_call, Reason});
        _ -> ok
    end.

-spec add_ice_candidate(CallId :: binary(), UserId :: binary(), Candidate :: map()) -> ok.
add_ice_candidate(CallId, UserId, Candidate) ->
    case get_call_pid(CallId) of
        {ok, Pid} -> gen_server:cast(Pid, {ice_candidate, UserId, Candidate});
        _ -> ok
    end.

-spec set_sdp_offer(CallId :: binary(), Sdp :: binary()) -> ok.
set_sdp_offer(CallId, Sdp) ->
    case get_call_pid(CallId) of
        {ok, Pid} -> gen_server:cast(Pid, {sdp_offer, Sdp});
        _ -> ok
    end.

-spec set_sdp_answer(CallId :: binary(), Sdp :: binary()) -> ok.
set_sdp_answer(CallId, Sdp) ->
    case get_call_pid(CallId) of
        {ok, Pid} -> gen_server:cast(Pid, {sdp_answer, Sdp});
        _ -> ok
    end.

-spec set_caller_pid(Pid :: pid(), CallerPid :: pid()) -> ok.
set_caller_pid(Pid, CallerPid) ->
    gen_server:cast(Pid, {set_caller_pid, CallerPid}).

-spec set_callee_pid(Pid :: pid(), CalleePid :: pid()) -> ok.
set_callee_pid(Pid, CalleePid) ->
    gen_server:cast(Pid, {set_callee_pid, CalleePid}).

-spec get_state(CallId :: binary()) -> {ok, map()} | {error, not_found}.
get_state(CallId) ->
    case get_call_pid(CallId) of
        {ok, Pid} -> gen_server:call(Pid, get_state);
        Error -> Error
    end.

%%--------------------------------------------------------------------
%% gen_server callbacks
%%--------------------------------------------------------------------

init([CallId, CallerId, CalleeId]) ->
    process_flag(trap_exit, true),
    
    Now = erlang:system_time(millisecond),
    
    %% Register call in Mnesia
    ok = mnesia_store:register_call(CallId, CallerId, CalleeId, self()),
    
    %% Start ring timeout timer
    RingTimer = erlang:send_after(?RING_TIMEOUT, self(), ring_timeout),
    
    State = #state{
        call_id = CallId,
        caller_id = CallerId,
        callee_id = CalleeId,
        call_state = ringing,
        created_at = Now,
        ring_timer = RingTimer
    },
    
    {ok, State}.

handle_call(get_state, _From, State) ->
    Reply = #{
        call_id => State#state.call_id,
        caller_id => State#state.caller_id,
        callee_id => State#state.callee_id,
        call_state => State#state.call_state,
        call_type => State#state.call_type
    },
    {reply, {ok, Reply}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% Call accepted by callee
handle_cast({accept, CalleePid}, State) ->
    %% Cancel ring timer
    cancel_timer(State#state.ring_timer),
    
    %% Start call duration timer
    CallTimer = erlang:send_after(?CALL_TIMEOUT, self(), call_timeout),
    
    %% Update state
    NewState = State#state{
        callee_pid = CalleePid,
        call_state = connecting,
        ring_timer = undefined,
        call_timer = CallTimer
    },
    
    %% Update Mnesia
    mnesia_store:update_call_state(State#state.call_id, connecting),
    
    %% Notify caller that call was accepted
    notify_user(State#state.caller_pid, #{
        type => <<"call_accepted">>,
        call_id => State#state.call_id
    }),
    
    {noreply, NewState};

%% Call rejected by callee
handle_cast(reject, State) ->
    cancel_timer(State#state.ring_timer),
    
    %% Notify caller
    notify_user(State#state.caller_pid, #{
        type => <<"call_rejected">>,
        call_id => State#state.call_id
    }),
    
    {stop, normal, State#state{call_state = ended}};

%% End call
handle_cast({end_call, Reason}, State) ->
    cancel_timer(State#state.ring_timer),
    cancel_timer(State#state.call_timer),
    
    %% Notify both parties
    EndMsg = #{
        type => <<"call_ended">>,
        call_id => State#state.call_id,
        reason => Reason
    },
    notify_user(State#state.caller_pid, EndMsg),
    notify_user(State#state.callee_pid, EndMsg),
    
    {stop, normal, State#state{call_state = ended}};

%% ICE candidate received
handle_cast({ice_candidate, FromUserId, Candidate}, State) ->
    %% Forward to the other party
    ToPid = if
        FromUserId =:= State#state.caller_id -> State#state.callee_pid;
        true -> State#state.caller_pid
    end,
    
    notify_user(ToPid, #{
        type => <<"ice_candidate">>,
        call_id => State#state.call_id,
        candidate => Candidate
    }),
    
    %% Store candidate
    NewCandidates = [Candidate | State#state.ice_candidates],
    {noreply, State#state{ice_candidates = NewCandidates}};

%% SDP offer received
handle_cast({sdp_offer, Sdp}, State) ->
    %% Forward to callee
    notify_user(State#state.callee_pid, #{
        type => <<"sdp_offer">>,
        call_id => State#state.call_id,
        sdp => Sdp
    }),
    
    {noreply, State#state{sdp_offer = Sdp}};

%% SDP answer received
handle_cast({sdp_answer, Sdp}, State) ->
    %% Forward to caller
    notify_user(State#state.caller_pid, #{
        type => <<"sdp_answer">>,
        call_id => State#state.call_id,
        sdp => Sdp
    }),
    
    %% Call is now connected
    mnesia_store:update_call_state(State#state.call_id, connected),
    
    {noreply, State#state{sdp_answer = Sdp, call_state = connected}};

handle_cast({set_caller_pid, Pid}, State) ->
    {noreply, State#state{caller_pid = Pid}};

handle_cast({set_callee_pid, Pid}, State) ->
    {noreply, State#state{callee_pid = Pid}};

handle_cast(_Msg, State) ->
    {noreply, State}.

%% Ring timeout - callee didn't answer
handle_info(ring_timeout, State) ->
    notify_user(State#state.caller_pid, #{
        type => <<"call_failed">>,
        call_id => State#state.call_id,
        reason => <<"no_answer">>
    }),
    {stop, normal, State#state{call_state = ended}};

%% Call duration timeout
handle_info(call_timeout, State) ->
    end_call(State#state.call_id, <<"timeout">>),
    {stop, normal, State#state{call_state = ended}};

%% Handle participant disconnect
handle_info({'EXIT', Pid, _Reason}, State) when 
        Pid =:= State#state.caller_pid; 
        Pid =:= State#state.callee_pid ->
    end_call(State#state.call_id, <<"participant_disconnected">>),
    {stop, normal, State#state{call_state = ended}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    mnesia_store:unregister_call(State#state.call_id),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

get_call_pid(CallId) ->
    mnesia_store:get_call_pid(CallId).

notify_user(undefined, _Msg) -> ok;
notify_user(Pid, Msg) when is_pid(Pid) ->
    user_connection:send_message(Pid, Msg).

cancel_timer(undefined) -> ok;
cancel_timer(Ref) -> erlang:cancel_timer(Ref).
