%%%-------------------------------------------------------------------
%%% @doc Video Web Application
%%% Starts Cowboy HTTP/WebSocket server
%%% @end
%%%-------------------------------------------------------------------
-module(video_web_app).
-behaviour(application).

-export([start/2, stop/1]).

%%--------------------------------------------------------------------
%% Application callbacks
%%--------------------------------------------------------------------

start(_StartType, _StartArgs) ->
    %% Get port from config
    {ok, Port} = application:get_env(video_web, http_port),
    
    %% Define routes
    Dispatch = cowboy_router:compile([
        {'_', [
            %% WebSocket endpoint for signaling
            {"/ws", websocket_handler, []},
            
            %% REST API endpoints
            {"/api/auth/login", auth_handler, [login]},
            {"/api/auth/token", auth_handler, [token]},
            {"/api/turn/credentials", turn_credentials, []},
            
            %% Health check
            {"/health", health_handler, []}
        ]}
    ]),
    
    %% Start HTTP listener
    {ok, _} = cowboy:start_clear(http_listener,
        [{port, Port}],
        #{env => #{dispatch => Dispatch}}
    ),
    
    io:format("Video call server started on port ~p~n", [Port]),
    
    video_web_sup:start_link().

stop(_State) ->
    ok = cowboy:stop_listener(http_listener),
    ok.
