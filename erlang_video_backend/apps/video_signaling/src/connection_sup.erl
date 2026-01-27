%%%-------------------------------------------------------------------
%%% @doc Connection Supervisor
%%% Dynamic supervisor for user connection processes (one per user)
%%% Uses simple_one_for_one for efficient dynamic child spawning
%%% @end
%%%-------------------------------------------------------------------
-module(connection_sup).
-behaviour(supervisor).

-export([start_link/0, start_connection/2, stop_connection/1]).
-export([init/1]).

-define(SERVER, ?MODULE).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% Start a new user connection process
-spec start_connection(UserId :: binary(), Socket :: any()) -> 
    {ok, pid()} | {error, term()}.
start_connection(UserId, Socket) ->
    supervisor:start_child(?SERVER, [UserId, Socket]).

%% Stop a user connection process
-spec stop_connection(Pid :: pid()) -> ok.
stop_connection(Pid) ->
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
            id => user_connection,
            start => {user_connection, start_link, []},
            restart => temporary,  % Don't restart crashed user connections
            shutdown => 5000,
            type => worker,
            modules => [user_connection]
        }
    ],
    
    {ok, {SupFlags, ChildSpecs}}.
