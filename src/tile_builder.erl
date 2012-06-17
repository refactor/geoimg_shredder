-module(tile_builder).
-behaviour(gen_server).


%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/3]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(CutterInfo, {RawTile, {Tx, Ty, Tz}}, RiakClientSocketPid) ->
    gen_server:start_link(?MODULE, [CutterInfo, {RawTile, {Tx, Ty, Tz}}, RiakClientSocketPid], []).

-record(state, {cutter_pid, riakclient, ref, rawtile, tileinfo}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([{CutterPid, Ref}, {RawTile, {Tx, Ty, Tz}}, RiakClientSocketPid]) ->
    self() ! start,
    {ok, #state{cutter_pid=CutterPid,
                riakclient = RiakClientSocketPid,
                ref = Ref,
                rawtile = RawTile, 
                tileinfo={Tx,Ty,Tz}} }.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(start, State = #state{ref=Ref, 
                                  rawtile=RawTile, 
                                  tileinfo={Tx,Ty,Tz}, 
                                  riakclient=RiakClientSocketPid}) ->
    {ok, Tile} = gdal_nif:build_tile(RawTile),
    Res = export_tile(RiakClientSocketPid, Tile, {Tx, Ty, Tz}),
    lager:debug("export result: ~p", [Res]),
    ok = Res,
    img_cutter:complete(State#state.cutter_pid, Ref, {Tx,Ty,Tz}),
    {stop, normal, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
export_tile(RiakClientSocketPid, Tile, {Tx, Ty, Tz}) ->
    QuadtreeKey = global_grid:quadtree(Tx, Ty, Tz),
    {ok, TileBinary} = gdal_nif:tile_to_binary(Tile, QuadtreeKey),
    lager:debug("img quadtreekey: ~p, binary size: ~p~n", 
                [QuadtreeKey, size(TileBinary)]),

    ContentType = "image/png",
    TileRiakObject = riakc_obj:new(<<"gis">>, QuadtreeKey, TileBinary, ContentType),
    riakc_pb_socket:put(RiakClientSocketPid, TileRiakObject).

