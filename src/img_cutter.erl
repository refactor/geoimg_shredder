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
                img_tiler       :: module(),      %% instanced parameterized module
                tile_size = 256 :: non_neg_integer()}).


%% API

-spec start_link(module(), string(), tuple()) -> {ok,pid()} | ignore | {error,any()}.
start_link(RiakClientConfig, TileMapProfileMod, ImgFileName) ->
    gen_fsm:start_link(?MODULE, {RiakClientConfig, TileMapProfileMod, ImgFileName}, []).

-spec complete(pid(), global_grid:tile_info()) -> ok.
complete(Pid, TileRef) ->
    gen_fsm:send_all_state_event(Pid, {complete, TileRef}).


%% callback

init({{RiakIp, RiakPort}, ProfileMod, ImgFileName}) ->
    lager:debug("img_cutter start: ~p", [self()]),
    StartTime = os:timestamp(),

    Limit = 100,
    PoolName = list_to_atom(ImgFileName),
    ppool:start_pool(PoolName, Limit, {tile_builder, start_link, []}),

    {ok, RiakClientSocketPid} = riakc_pb_socket:start_link(RiakIp, RiakPort),

    self() ! {start, ImgFileName, ProfileMod},
    {ok, copyouting, #state{riakclient  = RiakClientSocketPid,
                            img_filename = ImgFileName,
                            start_time = StartTime}}.

copyouting({continue, TileInfo, Continuation}, State=#state{tile_refs=TileRefs, 
                                                            img_filename=ImgFileName,
                                                            riakclient=RiakClient,
                                                            img_tiler=ImgTiler}) ->
    {TileX,TileY,TileZoom} = TileInfo, %make_ref(),
    {ok, TileRawdata} = ImgTiler:copyout_rawtile_for(TileX, TileY, TileZoom),
    PoolName = list_to_existing_atom(ImgFileName),
    ppool:async_queue(PoolName, [self(), {TileRawdata, TileInfo}, RiakClient]),
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
    ImgTiler = img_tiler:new(ProfileMod, ImgFileName),
    gen_fsm:send_event(self(), ImgTiler:scan_img()),
    {next_state, StateName, StateData#state{img_tiler=ImgTiler}}.

terminate(_Reason, _StateName, #state{counter = Counter, 
                                      riakclient = RiakClient, 
                                      img_filename = ImgFileName,
                                      start_time = StartTime}) ->
    riakc_pb_socket:stop(RiakClient),

    PoolName = list_to_existing_atom(ImgFileName),
    ppool:stop_pool(PoolName),

    lager:info("img(\"~s\") copyout-build-export to tiles had done, sum: ~p, time: ~p(sec)", 
               [ImgFileName, Counter, timer:now_diff(os:timestamp(), StartTime)/1000000.0]),
    ok.

code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.


