%% Record definitions for video call system

%% User session - tracks connected users
-record(user_session, {
    user_id :: binary(),
    node :: node(),
    pid :: pid(),
    socket :: any(),
    presence = online :: online | away | offline,
    active_calls = [] :: [binary()],
    device_info = #{} :: map(),
    last_seen :: integer()
}).

%% Call session - tracks active calls  
-record(call_session, {
    call_id :: binary(),
    caller_id :: binary(),
    callee_id :: binary(),
    caller_pid :: pid() | undefined,
    callee_pid :: pid() | undefined,
    sdp_offer :: binary() | undefined,
    sdp_answer :: binary() | undefined,
    ice_candidates = [] :: [map()],
    call_type = video :: video | audio,
    state = initiating :: initiating | ringing | connecting | connected | ended | failed,
    created_at :: integer(),
    ended_at :: integer() | undefined
}).

%% XMPP stanza representation
-record(xmpp_stanza, {
    type :: call_initiate | call_accept | call_reject | call_end | 
            ice_candidate | sdp_offer | sdp_answer | presence,
    from :: binary(),
    to :: binary(),
    call_id :: binary() | undefined,
    payload :: map()
}).
