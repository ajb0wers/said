-module(said_srv).
-behaviour(gen_server).


-include_lib("kernel/include/logger.hrl").

-export([enter/0, leave/0, chat/1]).
-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2,
         handle_continue/2, terminate/2]).

-record(state, {clients=[]}).

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

enter() -> 
  gen_server:cast(?MODULE, {enter, self()}).

leave() ->
  gen_server:cast(?MODULE, {leave, self()}).

chat(Msg) -> 
  gen_server:cast(?MODULE, {chat, self(), Msg}).

init(_Args) ->
  {ok, #state{}}.

handle_call(Msg, _From, State) ->
  ?LOG_INFO(#{what => Msg}),
	{noreply, State}.

handle_cast({enter, From} = Msg, #state{clients=Clients} = State) ->
  ?LOG_INFO(#{what => Msg, state => State}),
	NewState = State#state{clients=[From|Clients]},
  ?LOG_INFO(#{clients => NewState}),
	{noreply, NewState};
handle_cast({leave, From} = Msg, #state{clients=Clients} = State) ->
  ?LOG_INFO(#{what => Msg}),
  NewState = State#state{clients=lists:delete(From, Clients)},
	{noreply, NewState};
handle_cast({chat, From, Text} = Msg, #state{clients=Clients} = State) ->
  ?LOG_INFO(#{what => Msg}),
  ok = notify(From, Text, Clients),
	{noreply, State};
handle_cast(Msg, State) -> 
  ?LOG_INFO(#{wtf => Msg}),
	{noreply, State}.

handle_info(_Msg, State) ->
  {noreply, State}.

handle_continue(_Msg, State) ->
  {noreply, State}.

terminate(_Reason, _State) -> ok.


notify(_From, _Text, []) ->
  ok;
notify(From, Text, [H|T]) when From =:= H ->
  notify(From, Text, T);
notify(From, Text, [H|T])  ->
  H ! {text, From, Text},
  notify(From, Text, T).


