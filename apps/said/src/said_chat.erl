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
     idle_timeout => 900000}}.

websocket_init(State) ->
  said_srv:enter(),
	{chat(text, <<"Hello!">>), State}.

websocket_handle({text, Msg0}, State) ->
  #{<<"message">> := Msg} = json:decode(Msg0),
  said_srv:chat(Msg),
	{chat(text, Msg), State};
websocket_handle(_Data, State) ->
	{[], State}.

websocket_info({timeout, _Ref, Msg}, State) ->
	{toast(text, Msg), State};
websocket_info({text, Msg}, State) ->
	{chat(text, Msg), State};
websocket_info({text, From, Msg}, State) ->
	{chat(text, From, Msg), State};
websocket_info(_Info, State) ->
	{[], State}.

terminate(Reason, _PartialReq, _State) ->
  ?LOG_INFO(#{what=>terminate, reason=>Reason}),
  said_srv:leave().


chat(text, Msg) -> 
  You = list_to_binary(io_lib:format("~w", [self()])),
  Text = iolist_to_binary(escape(Msg)),
	[{text, <<"""
    <div id="chat" hx-swap-oob="beforeend">
    <p><strong>
    """, You/binary, "</strong> ", Text/binary,
    "</p></div>" >>}].

chat(text, From, Msg) -> 
  Other = list_to_binary(io_lib:format("~w", [From])),
  Text = iolist_to_binary(escape(Msg)),
	[{text, <<"""
    <div id="chat" hx-swap-oob="beforeend">
    <p>
    """, Other/binary, " ", Text/binary,
    "</p></div>" >>}].

toast(text, Msg) -> 
  Text = iolist_to_binary(escape(Msg)),
	[{text, <<"""
    <div id="notifications">
    """, Text/binary, "</div>">>}].

escape(Msg) when is_binary(Msg) ->
  Chars = [
    {"&",  "&amp;"},
    {"<",  "&gt;"},
    {">",  "&lt;"},
    {"'",  "&#x27;"},
    {"\"", "&quot;"}
  ],
  Fun = fun({Char, Esc}, String) ->
    string:replace(String, Char, Esc, all)
  end,
  lists:foldl(Fun, Msg, Chars).
