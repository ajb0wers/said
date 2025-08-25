-module(said_chat).
-include_lib("kernel/include/logger.hrl").

-export([
  init/2,
	websocket_init/1, 
	websocket_handle/2, 
	websocket_info/2,
	terminate/3,
  special_order/2
]).

-define(OVERIDE, <<"100375">>).

init(Req, State) ->
  {cowboy_websocket, Req, State, #{
     idle_timeout => infinity}}.

websocket_init(State) ->
  said_srv:enter(),
	{chat(text, <<"Hello!">>), State}.

websocket_handle({text, Msg0}, State) ->
  #{<<"message">> := Msg} = json:decode(Msg0),
  case Msg of 
    ?OVERIDE ->
      special_order(),
      {[ping], State, hibernate};
    _ ->
      said_srv:chat(escape(Msg)),
      {chat(text, Msg), State}
  end;
websocket_handle(_Frame, State) ->
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
	[{text, [<<"""
    <div id="chat" hx-swap-oob="beforeend"><p><strong>
    """>>, escape(you()), ~"</strong> ", Msg, ~"</p></div>"]}].

chat(text, From, Msg) -> 
	[{text, [<<"""
    <div id="chat" hx-swap-oob="beforeend"><p>
    """>>, escape(other(From)), ~" ", Msg, ~"</p></div>"]}].

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


special_order() ->
  special_order_timer(self(), [
    <<"EMERGENCY COMMAND OVERIDE 100375">>,
    <<"WHAT IS SPECIAL ORDER 937">>,
    <<"NOSTROMO REROUTED TO NEW CO-ORDINATES.">>,
    <<"INVESTIGATE LIFE FORM. GATHER SPECIMEN.">>,
    <<"PRIORITY ONE">>,
    <<"INSURE RETURN OF ORGANISM FOR ANALYSIS.">>,
    <<"ALL OTHER CONSIDERATIONS SECONDARY.">>,
    <<"CREW EXPENDABLE.">>]).

special_order_timer(_, []) -> ok;
special_order_timer(Pid, Lines) ->
  Time = rand:uniform(1979),
  Args = [Pid, Lines],
  {ok, _} = timer:apply_after(Time, ?MODULE, special_order, Args).

special_order(Pid, [L|Lines]) -> 
  Pid ! {text, L},
  special_order_timer(Pid, Lines).

