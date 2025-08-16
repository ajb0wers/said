-module(said_chat).
-include_lib("kernel/include/logger.hrl").

-export([init/2]).
-export([
	websocket_init/1, 
	websocket_handle/2, 
	websocket_info/2,
	terminate/3
]).


init(Req, State) ->
  {cowboy_websocket, Req, State, #{
     idle_timeout => 900000}}.

websocket_init(State) ->
  said_srv:enter(),
	{[chat(text, <<"Hello!">>)], State}.

websocket_handle({text, Msg0}, State) ->
  {ok, #{<<"message">> := Msg}} = thoas:decode(Msg0),
  said_srv:chat(Msg),
	{[chat(text, Msg)], State};
websocket_handle(_Data, State) ->
	{[], State}.

websocket_info({timeout, _Ref, Msg}, State) ->
	{[toast(text, Msg)], State};
websocket_info({text, Msg}, State) ->
	{[chat(text, Msg)], State};
websocket_info({text, From, Msg}, State) ->
	{[chat(text, From, Msg)], State};
websocket_info(_Info, State) ->
	{[], State}.

terminate(Reason, _PartialReq, _State) ->
  ?LOG_INFO(#{what=>terminate, reason=>Reason}),
  said_srv:leave().


%% chat({ok, #{<<"message">> := Msg}}) ->  chat(text, Msg).

chat(text, Msg) -> 
  You = list_to_binary(io_lib:format("~w", [self()])),
	{text, <<
    """
    <div id="chat" hx-swap-oob="beforeend">
    <p><strong>
    """, You/binary, "</strong> ", Msg/binary,
    "</p></div>" >>}.

chat(text, From, Msg) -> 
  Other = list_to_binary(io_lib:format("~w", [From])),
	{text, <<
    """
    <div id="chat" hx-swap-oob="beforeend">
    <p>
    """, Other/binary, " ", Msg/binary,
    "</p></div>" >>}.

toast(text, Msg) -> 
	{text, <<
    """
    <div id="notifications">
    """, Msg/binary, "</div>">>}.

