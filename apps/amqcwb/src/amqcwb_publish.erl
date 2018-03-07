%%%-------------------------------------------------------------------
%%% @author Marek Cermak
%%% @copyright (C) 2018, EZE
%%% @doc
%%%
%%% @end
%%% Created : 2018-03-06
%%%-------------------------------------------------------------------

-module(amqcwb_publish).

-behaviour(gen_server).

-include("amqcwb_priv.hrl").

%% API
-export([start/0, start_link/0, stop/0]).
-export([init/2]).

%% gen_server callbacks
-export([init/1]).
-export([handle_call/3, handle_cast/2, handle_info/2]).
-export([terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(HTTPRQ, handle_http_request).

-record(pubst, {}).

%%====================================================================
%% API functions
%%====================================================================

start() ->
    ?L_DEBUG("start()"),
    gen_server:start({local, ?SERVER}, ?MODULE, {}, []).

start_link() ->
    ?L_DEBUG("start_link()"),
    gen_server:start_link({local, ?SERVER}, ?MODULE, {}, []).

stop() ->
    ?L_DEBUG("die()"),
    gen_server:cast(?SERVER, die).

%% Cowboy Handler %%
init(InReq, HttpState) ->
    gen_server:call(?SERVER, {?HTTPRQ, InReq, HttpState}, 20*1000).

%%====================================================================
%% gen_server Callback functions
%%====================================================================

init(_Args) ->
    ?L_DEBUG("init()"),
    {ok, #pubst{}}.

%% handle_call %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_call({?HTTPRQ, InReq, HttpState}, _From, #pubst{} = State) ->
    ?L_DEBUG("Incoming HTTP request."),
    Resp = cowboy_req:reply(200,
                           #{<<"content-type">> => <<"text/plain">>},
                           build_test_body(InReq), InReq),
    {reply, {ok, Resp, HttpState}, State}.

%% handle_cast %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_cast(die, #pubst{} = State) ->
    {stop, {die, "Asked to die."}, State}.

%% handle_info %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_info(_Info, #pubst{} = State) ->
    {noreply, State}.

%% rest of callbacks %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

terminate(_Reason, #pubst{} = _State) ->
    ok.

code_change(_OldVsn, #pubst{} = State, _Extra) ->
    {ok, State}.


%%====================================================================
%% Internal functions
%%====================================================================

build_test_body(InReq) ->
    #{method := Method, path := Path, qs := Qs, has_body := HB} = InReq,
    URI = iolist_to_binary(cowboy_req:uri(InReq)),
    {Body, _Rq} = case HB of
                    true ->
                        fetch_body(InReq);
                    _ ->
                        <<" - no body - ">>
               end,
    R4 = <<"Hello Erlang!",
           "\nMethod: ", Method/binary,
           "\nPath: ", Path/binary,
           "\nQuery: ", Qs/binary,
           "\nURI: ", URI/binary,
           "\nBody:\n", Body/binary,
           "\n">>,
    R4.

fetch_body(Req0) ->
    ?L_DEBUG("fetch_body(Req0)"),
    case cowboy_req:has_body(Req0) of
        true ->
            {ok, Body, Req} = fetch_body(Req0, <<>>),
            {Body, Req};
        false ->
            {<<>>, Req0}
    end.

fetch_body(Req0, Acc) ->
    ?L_DEBUG("fetch_body(Req0, Acc)"),
    case cowboy_req:read_body(Req0) of
        {ok, Data, Req} ->
            {ok, <<Acc/binary, Data/binary>>, Req};
        {more, Data, Req} ->
            fetch_body(Req, <<Acc/binary, Data/binary>>)
    end.

%%====================================================================
%% TESTs
%%====================================================================


%% End of file

