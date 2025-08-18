-module(said_chat).
-include_lib("kernel/include/logger.hrl").

-export([init/2]).
-export([
	websocket_init/1, 
	websocket_handle/2, 
	websocket_info/2,
	terminate/3
]).
-export([escape/1]).

init(Req, State) ->
  {cowboy_websocket, Req, State, #{
     idle_timeout => 120000}}.

websocket_init(State) ->
  said_srv:enter(),
	{chat(text, <<"Hello!">>), State}.

websocket_handle({text, Msg0}, State) ->
  #{<<"message">> := Msg} = json:decode(Msg0),
  said_srv:chat(escape(Msg)),
	{chat(text, Msg), State};
websocket_handle(Frame, State) ->
	{[], State}.

websocket_info({timeout, _Ref, Msg}, State) ->
	{toast(text, Msg), State};
websocket_info({text, Msg}, State) ->
	{chat(text, Msg), State};
websocket_info({text, From, Msg}, State) ->
	{chat(text, From, Msg), State};
websocket_info(Info, State) ->
	{[], State}.

terminate(Reason, _PartialReq, _State) ->
  ?LOG_INFO(#{what=>terminate, reason=>Reason}),
  said_srv:leave().


chat(text, Msg) -> 
	[{text, [<<"""
    <div id="chat" hx-swap-oob="beforeend"><p><strong>
    """>>, you(), ~"</strong> ", Msg, ~"</p></div>"]}].

chat(text, From, Msg) -> 
	[{text, [<<"""
    <div id="chat" hx-swap-oob="beforeend"><p>
    """>>, other(From), ~" ", Msg, ~"</p></div>"]}].

toast(text, Msg) -> 
	[{text, [<<"""
    <div id="notifications">
    """>>, Msg, ~"</div>"]}].

you() -> format_pid(self()).
other(Pid) -> format_pid(Pid).
format_pid(Pid) ->
  Chars = io_lib:format("~w", [Pid]),
  list_to_binary(Chars).

-spec escape(Text) -> Result when
    Text :: binary(),
    Result :: [unicode:chardata()].
escape(Text) ->
  Text1 = string:replace(Text, "&", "&amp;", all),
  Text2 = string:replace(Text1, "<", "&lt;", all),
  Text3 = string:replace(Text2, ">", "&gt;", all),
  Text4 = string:replace(Text3, "\"", "&quot;", all),
  string:replace(Text4, "'", "&#x27;", all).
