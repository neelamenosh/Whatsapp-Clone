%%%-------------------------------------------------------------------
%%% @doc Auth Handler
%%% Handles authentication and JWT token operations
%%% @end
%%%-------------------------------------------------------------------
-module(auth_handler).
-behaviour(cowboy_handler).

-export([init/2, verify_token/1, generate_token/1]).

-define(JWT_SECRET, <<"your-secret-key-change-in-production">>).
-define(JWT_EXPIRY, 86400). % 24 hours

%%--------------------------------------------------------------------
%% Cowboy handler callback
%%--------------------------------------------------------------------

init(Req0, [Action]) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    
    Response = case Action of
        login -> handle_login(Body);
        token -> handle_token_refresh(Body)
    end,
    
    Req = cowboy_req:reply(200,
        #{<<"content-type">> => <<"application/json">>},
        jsx:encode(Response),
        Req1
    ),
    
    {ok, Req, []}.

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

%% Generate JWT token for user
-spec generate_token(UserId :: binary()) -> binary().
generate_token(UserId) ->
    Now = erlang:system_time(second),
    Expiry = Now + ?JWT_EXPIRY,
    
    %% Simple JWT structure (header.payload.signature)
    Header = base64:encode(jsx:encode(#{alg => <<"HS256">>, typ => <<"JWT">>})),
    Payload = base64:encode(jsx:encode(#{
        sub => UserId,
        iat => Now,
        exp => Expiry
    })),
    
    Message = <<Header/binary, ".", Payload/binary>>,
    Signature = base64:encode(crypto:mac(hmac, sha256, ?JWT_SECRET, Message)),
    
    <<Message/binary, ".", Signature/binary>>.

%% Verify JWT token
-spec verify_token(Token :: binary()) -> {ok, binary()} | {error, atom()}.
verify_token(Token) ->
    case binary:split(Token, <<".">>, [global]) of
        [Header, Payload, Signature] ->
            Message = <<Header/binary, ".", Payload/binary>>,
            ExpectedSig = base64:encode(crypto:mac(hmac, sha256, ?JWT_SECRET, Message)),
            
            if
                Signature =:= ExpectedSig ->
                    %% Verify expiry
                    case jsx:decode(base64:decode(Payload), [return_maps]) of
                        #{<<"sub">> := UserId, <<"exp">> := Exp} ->
                            Now = erlang:system_time(second),
                            if
                                Exp > Now -> {ok, UserId};
                                true -> {error, token_expired}
                            end;
                        _ ->
                            {error, invalid_payload}
                    end;
                true ->
                    {error, invalid_signature}
            end;
        _ ->
            {error, malformed_token}
    end.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

handle_login(Body) ->
    try
        #{<<"email">> := Email, <<"password">> := _Password} = jsx:decode(Body, [return_maps]),
        
        %% In production, verify credentials against database
        %% For now, generate a user ID from email
        UserId = crypto:hash(sha256, Email),
        UserIdHex = binary:encode_hex(binary:part(UserId, 0, 8)),
        
        Token = generate_token(UserIdHex),
        
        #{
            success => true,
            token => Token,
            user_id => UserIdHex
        }
    catch
        _:_ ->
            #{success => false, error => <<"Invalid request">>}
    end.

handle_token_refresh(Body) ->
    try
        #{<<"refresh_token">> := Token} = jsx:decode(Body, [return_maps]),
        
        case verify_token(Token) of
            {ok, UserId} ->
                NewToken = generate_token(UserId),
                #{success => true, token => NewToken};
            {error, Reason} ->
                #{success => false, error => atom_to_binary(Reason, utf8)}
        end
    catch
        _:_ ->
            #{success => false, error => <<"Invalid request">>}
    end.
