-module(img_cutter).
-behaviour(gen_fsm).

-export([init/1,
         copyouting/2, 
         listening/2, 
         handle_event/3, 
         handle_sync_event/4, 
         handle_info/3, 
         terminate/3, 
         code_change/4]).

%% API
-export([start_link/3,
         complete/2
        ]).

-record(state, {start_time      :: erlang:timestamp(),
                img_filename    :: string(),
                tile_refs = [],
                riakclient      :: pid(),
                counter = 0     :: non_neg_integer(),
                img_tiler,      %% instanced parameterized module
                tile_size = 256 :: non_neg_integer()}).

start_link(TileMapProfileMod, ImgFileName, RiakClientConfig) ->
    gen_fsm:start_link(?MODULE, {TileMapProfileMod, ImgFileName, RiakClientConfig}, []).

-spec complete(pid(), {TileX::integer(), TileY::integer(), TileZoom::integer()}) -> ok.
complete(Pid, TileRef) ->
    gen_fsm:send_all_state_event(Pid, {complete, TileRef}).

init({ProfileMod, ImgFileName, {RiakIp, RiakPort}}) ->
    StartTime = os:timestamp(),
    {ok, RiakClientSocketPid} = riakc_pb_socket:start_link(RiakIp, RiakPort),

    self() ! {start, ImgFileName, ProfileMod},
    {ok, copyouting, #state{riakclient  = RiakClientSocketPid,
                            img_filename = ImgFileName,
                            start_time = StartTime}}.

copyouting({continue, TileInfo, Continuation}, State=#state{tile_refs=TileRefs, 
                                                            riakclient=RiakClient,
                                                            img_tiler=ImgTiler}) ->
    {TileX,TileY,TileZoom} = TileInfo, %make_ref(),
    {ok, TileRawdata} = ImgTiler:copyout_rawtile_for(TileX, TileY, TileZoom),
    tile_builder:start_link(self(), {TileRawdata, TileInfo}, RiakClient),
    gen_fsm:send_event(self(), Continuation()),
    {next_state, copyouting, State#state{tile_refs=[TileInfo|TileRefs]}};
copyouting(done, State) ->
%    {next_state, copyouting, State}.
    listening(done, State).

listening(done, #state{tile_refs=[]} = State) -> 
    {stop, normal, State};
listening(done, State) ->
    {next_state, listening, State}.

handle_event({complete, TileInfo}, StateName, StateData = #state{tile_refs=TileRefs, 
                                                                 counter=Counter}) ->
    lager:debug("complete: ~p, length(TileRefs): ~p", [TileInfo, length(TileRefs)]),
    NewStateData = StateData#state{tile_refs = TileRefs -- [TileInfo], 
                                   counter = Counter + 1},
    case StateName of
        copyouting ->
            {next_state, copyouting, NewStateData};
        listening ->
            listening(done, NewStateData)
    end;
handle_event(debug, StateName, StateData) ->
    lager:debug("state name: ~p, state data: ~p", [StateName, StateData]),
    {next_state, StateName, StateData};
handle_event(_Event, StateName, StateData) ->
    {next_state, StateName, StateData}.
    
handle_sync_event(_Event, _From, StateName, StateData) ->
    {next_state, StateName, StateData}.

handle_info({start, ImgFileName, ProfileMod}, StateName, StateData) ->
    {ok, Img, RasterInfo} = gdal_nif:create_warped_vrt(ImgFileName, 
                                                       ProfileMod:epsg_code()),
    ImgTiler = img_tiler:new(ProfileMod, Img, RasterInfo),
    gen_fsm:send_event(self(), ImgTiler:scan_img()),
    {next_state, StateName, StateData#state{img_tiler=ImgTiler}}.

terminate(_Reason, _StateName, #state{counter = Counter, 
                                      riakclient = RiakClient, 
                                      img_filename = ImgFileName,
                                      start_time = StartTime}) ->
    riakc_pb_socket:stop(RiakClient),
    lager:info("img('~s') copyout-build-export to tiles had done, tiles sum: ~p, take time: ~p(sec)", 
               [ImgFileName, Counter, timer:now_diff(os:timestamp(), StartTime)/1000000.0]),
    ok.

code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.


