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

-spec start_link(pid(), {gdal_nif:rawtile(), tile_grid:tile_info()}, pid()) -> {ok,pid()} | ignore | {error,any()}.
start_link(CutterPid, {RawTile, TileInfo}, RiakClientSocketPid) ->
    gen_server:start_link(?MODULE, [CutterPid, {RawTile, TileInfo}, RiakClientSocketPid], []).

-record(state, {cutter_pid :: pid(), 
                riakclient :: pid(), 
                rawtile    :: gdal_nif:rawtile(), 
                tileinfo   :: tile_grid:tile_info()}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([CutterPid, {RawTile, TileInfo}, RiakClientSocketPid]) ->
    self() ! start,
    {ok, #state{cutter_pid = CutterPid,
                riakclient = RiakClientSocketPid,
                rawtile    = RawTile, 
                tileinfo   = TileInfo} }.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(start, State = #state{rawtile    = RawTile, 
                                  tileinfo   = TileInfo, 
                                  riakclient = RiakClientSocketPid}) ->
    {ok, Tile} = gdal_nif:build_tile(RawTile),
    Res = export_tile(RiakClientSocketPid, Tile, TileInfo),
    lager:debug("tile(~p) export result: ~p", [TileInfo, Res]),
    ok = Res,
    img_cutter:complete(State#state.cutter_pid, TileInfo),
    {stop, normal, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
-spec export_tile(pid(), gdal_nif:tile(), tile_grid:tile_info()) -> ok.
export_tile(RiakClientSocketPid, Tile, {Tx, Ty, TileZoom}) ->
    QuadtreeKey = tile_grid:quadtree({Tx, Ty, TileZoom}),
    {ok, TileBinary} = gdal_nif:tile_to_binary(Tile, QuadtreeKey, "png"),
    lager:debug("img quadtreekey: ~p, binary size: ~p~n", 
                [QuadtreeKey, size(TileBinary)]),

    ContentType = "image/png",
    TileRiakObject = riakc_obj:new(<<"gis">>, QuadtreeKey, TileBinary, ContentType),
    riakc_pb_socket:put(RiakClientSocketPid, TileRiakObject).

