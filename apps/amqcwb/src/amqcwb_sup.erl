%%%-------------------------------------------------------------------
%%% @author Marek Cermak
%%% @copyright (C) $year, EZE
%%% @doc amqcwb top level supervisor.
%%%
%%% @end
%%% Created : $fulldate
%%%-------------------------------------------------------------------

-module(amqcwb_sup).

-behaviour(supervisor).

-include("amqcwb_priv.hrl").

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    ?L_INFO(">>> Starting amqpCowboy supervisor"),
    SupFlags = #{strategy => one_for_one,
                 intensity => 2,
                 period => 30},

    {ok, { SupFlags, child_specs()} }.

%%====================================================================
%% Internal functions
%%====================================================================

child_specs() ->

%    Sup = #{ id => a_sup,
%                   start => {a_sup, start_link, []},
%                   restart => transient,
%                   type => supervisor,
%                   modules => [a_sup]
%                 },

    Publish = #{ id => amqcwb_publish_handler,
                 start => {amqcwb_publish, start_link, []},
                 restart => transient,
                 type => worker,
                 modules => [amqcwb_publish]
               },
    [Publish].

%% End of file

