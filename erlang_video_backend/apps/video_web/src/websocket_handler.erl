%%%-------------------------------------------------------------------
%%% @doc WebSocket Handler
%%% Handles WebSocket connections for XMPP signaling
%%% Implements Cowboy WebSocket behavior
%%% @end
%%%-------------------------------------------------------------------
-module(websocket_handler).
-behaviour(cowboy_websocket).

%% Cowboy WebSocket callbacks
-export([init/2, websocket_init/1, websocket_handle/2, 
         websocket_info/2, terminate/3]).

%% API for sending messages
-export([send/2]).

-record(state, {
    user_id :: binary() | undefined,
    user_pid :: pid() | undefined,
    authenticated = false :: boolean()
}).

%%--------------------------------------------------------------------
%% Cowboy callbacks
%%--------------------------------------------------------------------

init(Req, _Opts) ->
    %% Upgrade to WebSocket
    {cowboy_websocket, Req, #state{}, #{
        idle_timeout => 60000,
        compress => true
    }}.

websocket_init(State) ->
    {ok, State}.

%% Handle incoming WebSocket frames
websocket_handle({text, Data}, State) ->
    case jsx:decode(Data, [return_maps]) of
        #{<<"type">> := <<"auth">>} = Msg ->
            handle_auth(Msg, State);
        Msg when State#state.authenticated ->
            handle_message(Msg, State);
        _ ->
            %% Not authenticated, reject
            Reply = jsx:encode(#{
                type => <<"error">>,
                message => <<"Authentication required">>
            }),
            {reply, {text, Reply}, State}
    end;

websocket_handle({ping, _Data}, State) ->
    {reply, pong, State};

websocket_handle(_Frame, State) ->
    {ok, State}.

%% Handle messages from Erlang processes
websocket_info({send, Message}, State) ->
    Json = jsx:encode(Message),
    {reply, {text, Json}, State};

websocket_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _Req, State) ->
    %% Cleanup user connection
    case State#state.user_pid of
        undefined -> ok;
        Pid -> 
            connection_sup:stop_connection(Pid)
    end,
    ok.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

handle_auth(#{<<"token">> := Token}, State) ->
    case auth_handler:verify_token(Token) of
        {ok, UserId} ->
            %% Start user connection process
            {ok, UserPid} = connection_sup:start_connection(UserId, self()),
            
            %% Link to user process
            link(UserPid),
            
            Reply = jsx:encode(#{
                type => <<"auth_success">>,
                user_id => UserId
            }),
            
            NewState = State#state{
                user_id = UserId,
                user_pid = UserPid,
                authenticated = true
            },
            
            {reply, {text, Reply}, NewState};
        
        {error, Reason} ->
            Reply = jsx:encode(#{
                type => <<"auth_error">>,
                message => atom_to_binary(Reason, utf8)
            }),
            {reply, {text, Reply}, State}
    end;

handle_auth(_, State) ->
    Reply = jsx:encode(#{
        type => <<"error">>,
        message => <<"Missing token">>
    }),
    {reply, {text, Reply}, State}.

handle_message(Msg, State) ->
    %% Forward message to user connection process
    gen_server:cast(State#state.user_pid, {ws_message, Msg}),
    {ok, State}.

%% API: Send message to WebSocket client
send(Socket, Message) when is_pid(Socket) ->
    Socket ! {send, Message},
    ok;
send(_, _) ->
    ok.
