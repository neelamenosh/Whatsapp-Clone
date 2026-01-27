%%%-------------------------------------------------------------------
%%% @doc Call Supervisor
%%% Dynamic supervisor for call processes (one per active call)
%%% Uses simple_one_for_one for efficient dynamic child spawning
%%% @end
%%%-------------------------------------------------------------------
-module(call_sup).
-behaviour(supervisor).

-export([start_link/0, start_call/3, stop_call/1]).
-export([init/1]).

-define(SERVER, ?MODULE).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% Start a new call process
-spec start_call(CallId :: binary(), CallerId :: binary(), CalleeId :: binary()) -> 
    {ok, pid()} | {error, term()}.
start_call(CallId, CallerId, CalleeId) ->
    supervisor:start_child(?SERVER, [CallId, CallerId, CalleeId]).

%% Stop a call process
-spec stop_call(Pid :: pid()) -> ok.
stop_call(Pid) ->
    supervisor:terminate_child(?SERVER, Pid),
    ok.

%%--------------------------------------------------------------------
%% Supervisor callbacks
%%--------------------------------------------------------------------

init([]) ->
    SupFlags = #{
        strategy => simple_one_for_one,
        intensity => 100,
        period => 60
    },
    
    ChildSpecs = [
        #{
            id => call_process,
            start => {call_process, start_link, []},
            restart => temporary,  % Don't restart crashed calls
            shutdown => 5000,
            type => worker,
            modules => [call_process]
        }
    ],
    
    {ok, {SupFlags, ChildSpecs}}.
