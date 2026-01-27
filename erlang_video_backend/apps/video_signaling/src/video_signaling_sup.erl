%%%-------------------------------------------------------------------
%%% @doc Root Supervisor for Video Signaling
%%% Supervises all child supervisors using one_for_one strategy
%%% @end
%%%-------------------------------------------------------------------
-module(video_signaling_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%--------------------------------------------------------------------
%% Supervisor callbacks
%%--------------------------------------------------------------------

init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 10,
        period => 60
    },
    
    ChildSpecs = [
        %% Connection Supervisor - manages user connection processes
        #{
            id => connection_sup,
            start => {connection_sup, start_link, []},
            restart => permanent,
            shutdown => infinity,
            type => supervisor,
            modules => [connection_sup]
        },
        
        %% Call Supervisor - manages call processes
        #{
            id => call_sup,
            start => {call_sup, start_link, []},
            restart => permanent,
            shutdown => infinity,
            type => supervisor,
            modules => [call_sup]
        },
        
        %% Mnesia Store - handles database operations
        #{
            id => mnesia_store,
            start => {mnesia_store, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [mnesia_store]
        }
    ],
    
    {ok, {SupFlags, ChildSpecs}}.
