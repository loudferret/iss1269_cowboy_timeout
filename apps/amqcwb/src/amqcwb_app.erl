%%%-------------------------------------------------------------------
%% @doc amqcwb public API
%% @end
%%%-------------------------------------------------------------------

-module(amqcwb_app).

-behaviour(application).

-include("amqcwb_priv.hrl").

-export([start/0]).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start() ->
    ?L_INFO("DEV: Starting amqpCowboy."),
    application:ensure_all_started(amqcwb),
    application:start(amqcwb).

start(_StartType, _StartArgs) ->
    ?L_INFO(">>> Starting amqpCowboy."),
    amqcwb_cwb:initialization(),
    amqcwb_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ?L_INFO("<<< Stopping amqpCowboy."),
    ok.

%%====================================================================
%% Internal functions
%%====================================================================


%% -- end of file --

