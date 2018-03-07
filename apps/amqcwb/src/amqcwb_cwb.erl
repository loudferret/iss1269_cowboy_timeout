%%%-------------------------------------------------------------------
%% @doc amqcwb: Cowboy Handler
%%
%% @end
%%%-------------------------------------------------------------------

-module(amqcwb_cwb).

-include("amqcwb_priv.hrl").

%% API

-export([initialization/0]).
-export([init/2]).

%%====================================================================
%% API functions
%%====================================================================

initialization() ->
    ?L_INFO(">>> Initializing Cowboy listeners and routes."),
    Dispatch = cowboy_router:compile(routes()),
    {ok, _Pid} = cowboy:start_clear(?CWBNAME,
                                 [{port, ?CWBPORT}],
                                 #{env => #{dispatch => Dispatch}}),
    ?L_INFO("<<< Initializing Cowboy listeners and routes.").

init(InReq, HttpState) ->
    Resp = cowboy_req:reply(200,
                           #{<<"content-type">> => <<"text/plain">>},
                           build_test_body(InReq), InReq),
    {ok, Resp, HttpState}.

%%====================================================================
%% Internal functions
%%====================================================================

%% https://ninenines.eu/docs/en/cowboy/2.2/guide/routing/
routes() ->
	[   {?CWBHOST,
           [ {"/", cowboy_static, {priv_file, amqcwb, "index.html"}},
             {"/mqw/v1/pub/raw", amqcwb_publish, []},      %% timeout
             {"/mqw/v2/pub/raw", amqcwb_cwb, #{in => yes}} %% ok
           ]}
    ].



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


%% -- end of file --

