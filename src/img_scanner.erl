-module(img_scanner).
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
         complete/3
        ]).

-record(state, {refs = [],
                riakclient     :: pid(),
                counter = 0,
                img_tiler,
                map_profile,    % tile map profile, 
                                % such as global-geodetic or global-mercator module
                tile_size = 256 :: integer()}).

start_link(TileMapProfileMod, ImgFileName, RiakClientConfig) ->
    gen_fsm:start_link(?MODULE, {TileMapProfileMod, ImgFileName, RiakClientConfig}, []).

complete(Pid, Ref, Result) ->
    gen_fsm:send_all_state_event(Pid, {complete, Ref, Result}).

init({ProfileMod, ImgFileName, {RiakIp, RiakPort} = _RiakClientConfig}) ->
    {ok, RiakClientSocketPid} = riakc_pb_socket:start_link(RiakIp, RiakPort),

    self() ! {start, ImgFileName, ProfileMod},
    {ok, copyouting, #state{riakclient  = RiakClientSocketPid}}.

copyouting({continue, {TileX, TileY, TileZoom}, Continuation}, State=#state{refs=Refs, img_tiler=ImgTiler}) ->
    {ok, TileRawdata} = ImgTiler:copyout_tile_for(TileX, TileY, TileZoom),
    Ref = make_ref(),
    tile_builder:start_link({self(), Ref}, {TileRawdata, {TileX, TileY, TileZoom}}, State#state.riakclient),
    gen_fsm:send_event(self(), Continuation()),
    {next_state, copyouting, State#state{refs=[Ref|Refs]}};
copyouting(done, State) ->
%    {next_state, copyouting, State}.
    listening(done, State).

listening(done, #state{refs=[], counter=Counter}) ->
    lager:info("the tile copyout-build-work had done, built tiles sum: ~p", [Counter]),
    {stop, normal, done};
listening(done, State) ->
    {next_state, listening, State}.

handle_event({complete, Ref, Result}, StateName, StateData = #state{refs=Refs, counter=Counter}) ->
    lager:debug("complete: ~p, length(Refs): ~p", [Result, length(Refs)]),
    NewStateData = StateData#state{refs=Refs--[Ref], counter=Counter+1},
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

terminate(_Reason, _StateName, _StateData) ->
    ok.

code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.


