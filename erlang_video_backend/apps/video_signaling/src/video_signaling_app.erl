%%%-------------------------------------------------------------------
%%% @doc Video Signaling Application
%%% Main OTP application module for video call signaling
%%% @end
%%%-------------------------------------------------------------------
-module(video_signaling_app).
-behaviour(application).

-export([start/2, stop/1]).

%%--------------------------------------------------------------------
%% Application callbacks
%%--------------------------------------------------------------------

start(_StartType, _StartArgs) ->
    %% Initialize Mnesia tables
    ok = init_mnesia(),
    
    %% Start the root supervisor
    video_signaling_sup:start_link().

stop(_State) ->
    ok.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

init_mnesia() ->
    %% Create schema if not exists
    case mnesia:create_schema([node()]) of
        ok -> ok;
        {error, {_, {already_exists, _}}} -> ok
    end,
    
    %% Start Mnesia
    ok = application:ensure_started(mnesia),
    
    %% Create tables
    create_tables(),
    
    %% Wait for tables to be ready
    mnesia:wait_for_tables([user_session, call_session], 30000),
    ok.

create_tables() ->
    %% User session table - RAM copies for speed
    mnesia:create_table(user_session, [
        {attributes, [user_id, node, pid, socket, presence, active_calls, device_info, last_seen]},
        {ram_copies, [node()]},
        {type, set}
    ]),
    
    %% Call session table - RAM copies for speed
    mnesia:create_table(call_session, [
        {attributes, [call_id, caller_id, callee_id, caller_pid, callee_pid, 
                      sdp_offer, sdp_answer, ice_candidates, call_type, 
                      state, created_at, ended_at]},
        {ram_copies, [node()]},
        {type, set}
    ]),
    
    ok.
