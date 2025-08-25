%%%-------------------------------------------------------------------
%% @doc said public API
%% @end
%%%-------------------------------------------------------------------

-module(said_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_', [
				  {"/", cowboy_static, {priv_file, said, "index.html"}},
					{"/static/[...]", cowboy_static, {priv_dir, said, "static"}},
				  {"/app", cowboy_static, {priv_file, said, "chat.html"}},
					{"/chat", said_chat, []}
				]}
    ]),
   {ok, _} = cowboy:start_clear(said_http_listener,
       [{port, 10000}],
       #{env => #{dispatch => Dispatch}
	 		}
   ),
   said_sup:start_link().

stop(_State) ->
    ok.


%% internal functions
