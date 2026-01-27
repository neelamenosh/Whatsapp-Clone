%%%-------------------------------------------------------------------
%%% @doc TURN Credentials Handler
%%% Generates time-limited TURN server credentials using HMAC
%%% @end
%%%-------------------------------------------------------------------
-module(turn_credentials).
-behaviour(cowboy_handler).

-export([init/2, generate_credentials/1]).

-define(TURN_SECRET, <<"your-turn-secret-change-in-production">>).
-define(TURN_SERVER, <<"turn:turn.yourdomain.com:3478">>).
-define(STUN_SERVER, <<"stun:stun.yourdomain.com:3478">>).
-define(CREDENTIAL_TTL, 86400). % 24 hours

%%--------------------------------------------------------------------
%% Cowboy handler callback
%%--------------------------------------------------------------------

init(Req0, _Opts) ->
    %% Get authorization header
    case cowboy_req:header(<<"authorization">>, Req0) of
        <<"Bearer ", Token/binary>> ->
            case auth_handler:verify_token(Token) of
                {ok, UserId} ->
                    Credentials = generate_credentials(UserId),
                    Req = cowboy_req:reply(200,
                        #{<<"content-type">> => <<"application/json">>},
                        jsx:encode(Credentials),
                        Req0
                    ),
                    {ok, Req, []};
                {error, _} ->
                    unauthorized_response(Req0)
            end;
        _ ->
            unauthorized_response(Req0)
    end.

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

%% Generate TURN credentials using HMAC-based authentication
-spec generate_credentials(UserId :: binary()) -> map().
generate_credentials(UserId) ->
    %% TURN uses timestamp:username format
    Timestamp = erlang:system_time(second) + ?CREDENTIAL_TTL,
    Username = <<(integer_to_binary(Timestamp))/binary, ":", UserId/binary>>,
    
    %% Generate HMAC-SHA1 credential (RFC 5389)
    Credential = base64:encode(crypto:mac(hmac, sha1, ?TURN_SECRET, Username)),
    
    #{
        iceServers => [
            #{
                urls => ?STUN_SERVER
            },
            #{
                urls => ?TURN_SERVER,
                username => Username,
                credential => Credential
            },
            #{
                urls => <<"turn:turn.yourdomain.com:443?transport=tcp">>,
                username => Username,
                credential => Credential
            }
        ],
        ttl => ?CREDENTIAL_TTL
    }.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

unauthorized_response(Req0) ->
    Req = cowboy_req:reply(401,
        #{<<"content-type">> => <<"application/json">>},
        jsx:encode(#{error => <<"Unauthorized">>}),
        Req0
    ),
    {ok, Req, []}.
