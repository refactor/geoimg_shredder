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

start_link(ScannerInfo, {Tile, {Tx, Ty, Tz}}, RiakClientSocketPid) ->
    gen_server:start_link(?MODULE, [ScannerInfo, {Tile, {Tx, Ty, Tz}}, RiakClientSocketPid], []).

-record(state, {scanner, riakclient, ref, tile, tileinfo}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([{ScannerPid, Ref}, {Tile, {Tx, Ty, Tz}}, RiakClientSocketPid]) ->
    self() ! start,
    {ok, #state{scanner=ScannerPid,
                riakclient = RiakClientSocketPid,
                ref = Ref,
                tile = Tile, 
                tileinfo={Tx,Ty,Tz}} }.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(start, State = #state{ref=Ref, tile=Tile, tileinfo={Tx,Ty,Tz}, riakclient=RiakClientSocketPid}) ->
    gdal_nif:build_tile(Tile),
    Res = tile_export(RiakClientSocketPid, Tile, {Tx, Ty, Tz}),
    lager:debug("export result: ~p", [Res]),
    ok = Res,
    img_scanner:complete(State#state.scanner, Ref, {Tx,Ty,Tz}),
    {stop, normal, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
tile_export(RiakClientSocketPid, Tile, {Tx, Ty, Tz}) ->
    SaveTilesToDir = "/tmp/gtiles",
    TilesFileExt = "png",

    TileFilename = filename:join([SaveTilesToDir, 
            integer_to_list(Tz), integer_to_list(Tx), 
            integer_to_list(Ty) ++ "." ++ TilesFileExt]),
    ok = filelib:ensure_dir(TileFilename),
    lager:debug("saved tile(~p)", [TileFilename]),
    %gdal_nif:save_tile(Tile, TileFilename),

    Filename = 
            integer_to_list(Tz) ++ "_" ++ integer_to_list(Tx) ++ "_" ++
            integer_to_list(Ty) ++ "." ++ TilesFileExt,
    {ok, Binary} = gdal_nif:tile_to_binary(Tile, Filename),
%    ok = file:write_file(TileFilename, Binary),
    QuadtreeKey = list_to_binary(global_grid:quadtree(Tx, Ty, Tz)),
    lager:debug("img key: ~p, binary size: ~p~n", [QuadtreeKey, size(Binary)]),
    TileObject = riakc_obj:new(<<"gis">>, QuadtreeKey, Binary, "image/png"),
    riakc_pb_socket:put(RiakClientSocketPid, TileObject).
