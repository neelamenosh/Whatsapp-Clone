%%%-------------------------------------------------------------------
%%% @doc Health Check Handler
%%% Simple health check endpoint for load balancers
%%% @end
%%%-------------------------------------------------------------------
-module(health_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, _Opts) ->
    %% Check if all required applications are running
    Apps = [video_signaling, video_web, mnesia],
    AllRunning = lists:all(fun(App) ->
        case application:get_application(App) of
            {ok, _} -> true;
            undefined -> 
                %% Check if it's at least loaded
                lists:keymember(App, 1, application:which_applications())
        end
    end, Apps),
    
    {Status, Body} = if
        AllRunning ->
            {200, #{
                status => <<"healthy">>,
                node => atom_to_binary(node(), utf8),
                timestamp => erlang:system_time(millisecond)
            }};
        true ->
            {503, #{
                status => <<"unhealthy">>,
                node => atom_to_binary(node(), utf8)
            }}
    end,
    
    Req = cowboy_req:reply(Status,
        #{<<"content-type">> => <<"application/json">>},
        jsx:encode(Body),
        Req0
    ),
    
    {ok, Req, []}.
