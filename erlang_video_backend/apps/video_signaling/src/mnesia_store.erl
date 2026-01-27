%%%-------------------------------------------------------------------
%%% @doc Mnesia Store
%%% gen_server for Mnesia database operations
%%% Handles user session and call session storage
%%% @end
%%%-------------------------------------------------------------------
-module(mnesia_store).
-behaviour(gen_server).

-include("records.hrl").

%% API
-export([start_link/0]).
-export([register_user/3, unregister_user/1, get_user_pid/1, 
         update_presence/2, update_last_seen/1]).
-export([register_call/4, unregister_call/1, get_call/1, get_call_pid/1,
         update_call_state/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% User operations
-spec register_user(UserId :: binary(), Pid :: pid(), Socket :: any()) -> ok.
register_user(UserId, Pid, Socket) ->
    gen_server:call(?SERVER, {register_user, UserId, Pid, Socket}).

-spec unregister_user(UserId :: binary()) -> ok.
unregister_user(UserId) ->
    gen_server:call(?SERVER, {unregister_user, UserId}).

-spec get_user_pid(UserId :: binary()) -> {ok, pid()} | {error, not_found}.
get_user_pid(UserId) ->
    gen_server:call(?SERVER, {get_user_pid, UserId}).

-spec update_presence(UserId :: binary(), Presence :: atom()) -> ok.
update_presence(UserId, Presence) ->
    gen_server:cast(?SERVER, {update_presence, UserId, Presence}).

-spec update_last_seen(UserId :: binary()) -> ok.
update_last_seen(UserId) ->
    gen_server:cast(?SERVER, {update_last_seen, UserId}).

%% Call operations
-spec register_call(CallId :: binary(), CallerId :: binary(), 
                    CalleeId :: binary(), Pid :: pid()) -> ok.
register_call(CallId, CallerId, CalleeId, Pid) ->
    gen_server:call(?SERVER, {register_call, CallId, CallerId, CalleeId, Pid}).

-spec unregister_call(CallId :: binary()) -> ok.
unregister_call(CallId) ->
    gen_server:call(?SERVER, {unregister_call, CallId}).

-spec get_call(CallId :: binary()) -> {ok, #call_session{}} | {error, not_found}.
get_call(CallId) ->
    gen_server:call(?SERVER, {get_call, CallId}).

-spec get_call_pid(CallId :: binary()) -> {ok, pid()} | {error, not_found}.
get_call_pid(CallId) ->
    gen_server:call(?SERVER, {get_call_pid, CallId}).

-spec update_call_state(CallId :: binary(), State :: atom()) -> ok.
update_call_state(CallId, State) ->
    gen_server:cast(?SERVER, {update_call_state, CallId, State}).

%%--------------------------------------------------------------------
%% gen_server callbacks
%%--------------------------------------------------------------------

init([]) ->
    {ok, #{}}.

%% Register user session
handle_call({register_user, UserId, Pid, Socket}, _From, State) ->
    Now = erlang:system_time(millisecond),
    Session = #user_session{
        user_id = UserId,
        node = node(),
        pid = Pid,
        socket = Socket,
        presence = online,
        active_calls = [],
        device_info = #{},
        last_seen = Now
    },
    
    Trans = fun() ->
        mnesia:write(user_session, Session, write)
    end,
    
    case mnesia:transaction(Trans) of
        {atomic, ok} -> {reply, ok, State};
        {aborted, Reason} -> {reply, {error, Reason}, State}
    end;

%% Unregister user session
handle_call({unregister_user, UserId}, _From, State) ->
    Trans = fun() ->
        mnesia:delete({user_session, UserId})
    end,
    mnesia:transaction(Trans),
    {reply, ok, State};

%% Get user PID
handle_call({get_user_pid, UserId}, _From, State) ->
    Trans = fun() ->
        case mnesia:read(user_session, UserId) of
            [#user_session{pid = Pid}] -> {ok, Pid};
            [] -> {error, not_found}
        end
    end,
    
    case mnesia:transaction(Trans) of
        {atomic, Result} -> {reply, Result, State};
        {aborted, _} -> {reply, {error, not_found}, State}
    end;

%% Register call session
handle_call({register_call, CallId, CallerId, CalleeId, Pid}, _From, State) ->
    Now = erlang:system_time(millisecond),
    Session = #call_session{
        call_id = CallId,
        caller_id = CallerId,
        callee_id = CalleeId,
        caller_pid = Pid,
        callee_pid = undefined,
        sdp_offer = undefined,
        sdp_answer = undefined,
        ice_candidates = [],
        call_type = video,
        state = initiating,
        created_at = Now,
        ended_at = undefined
    },
    
    %% Store call session and PID mapping
    Trans = fun() ->
        mnesia:write(call_session, Session, write),
        %% Also store PID lookup
        put({call_pid, CallId}, Pid)
    end,
    
    case mnesia:transaction(Trans) of
        {atomic, _} -> 
            %% Store PID in process dictionary for fast lookup
            erlang:put({call_pid, CallId}, Pid),
            {reply, ok, State};
        {aborted, Reason} -> 
            {reply, {error, Reason}, State}
    end;

%% Unregister call session
handle_call({unregister_call, CallId}, _From, State) ->
    Trans = fun() ->
        mnesia:delete({call_session, CallId})
    end,
    mnesia:transaction(Trans),
    erlang:erase({call_pid, CallId}),
    {reply, ok, State};

%% Get call session
handle_call({get_call, CallId}, _From, State) ->
    Trans = fun() ->
        case mnesia:read(call_session, CallId) of
            [Session] -> {ok, Session};
            [] -> {error, not_found}
        end
    end,
    
    case mnesia:transaction(Trans) of
        {atomic, Result} -> {reply, Result, State};
        {aborted, _} -> {reply, {error, not_found}, State}
    end;

%% Get call PID
handle_call({get_call_pid, CallId}, _From, State) ->
    case erlang:get({call_pid, CallId}) of
        undefined -> {reply, {error, not_found}, State};
        Pid -> {reply, {ok, Pid}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% Update presence
handle_cast({update_presence, UserId, Presence}, State) ->
    Trans = fun() ->
        case mnesia:read(user_session, UserId) of
            [Session] ->
                mnesia:write(user_session, Session#user_session{presence = Presence}, write);
            [] -> ok
        end
    end,
    mnesia:transaction(Trans),
    {noreply, State};

%% Update last seen
handle_cast({update_last_seen, UserId}, State) ->
    Now = erlang:system_time(millisecond),
    Trans = fun() ->
        case mnesia:read(user_session, UserId) of
            [Session] ->
                mnesia:write(user_session, Session#user_session{last_seen = Now}, write);
            [] -> ok
        end
    end,
    mnesia:transaction(Trans),
    {noreply, State};

%% Update call state
handle_cast({update_call_state, CallId, CallState}, State) ->
    Trans = fun() ->
        case mnesia:read(call_session, CallId) of
            [Session] ->
                mnesia:write(call_session, Session#call_session{state = CallState}, write);
            [] -> ok
        end
    end,
    mnesia:transaction(Trans),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
