-module(img_picker).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-define(POOL_NAME, img_picker__pool).

-include_lib("kernel/include/file.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0,
         start_link/2,
         scan_dir/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {start_time      :: erlang:timestamp(),
                img_filesdir    :: string(),
                map_profile     :: module(),
                riak_client,
                counter = 0     :: non_neg_integer()}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    {ok, RiakClient} = application:get_env(riak_client),
    {ok, TileMapProfileMod} = application:get_env(map_profile),
    start_link(RiakClient, TileMapProfileMod).

start_link(RiakClientConfig, TileMapProfileMod) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, {RiakClientConfig, TileMapProfileMod}, []).

scan_dir(ImgFilesDir) ->
    gen_server:cast(?MODULE, {scan_imgs, ImgFilesDir}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

-spec init({{string(), non_neg_integer()}, module()}) -> ok.
init({RiakClient, ProfileMod}) ->
    StartTime = os:timestamp(),

    Limit = 64,
    ppool:start_pool(?POOL_NAME, Limit, {img_cutter, start_link, [RiakClient, ProfileMod]}),

    case application:get_env(imgdir) of
        {ok, Dir} ->
            self() ! {start_imgs_scan, Dir};
        undefined ->
            lager:warning("NO default imgdir in application env")
    end,

    {ok, #state{start_time=StartTime, 
                riak_client = RiakClient,
                map_profile=ProfileMod} }.


handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({scan_imgs, ImgFilesDir}, State) ->
    lager:info("scan dir: ~p", [ImgFilesDir]),
    start_imgs_scan(ImgFilesDir),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({start_imgs_scan, ImgFilesDir}, State) ->
    lager:info("scan app's env.dir: ~p", [ImgFilesDir]),
    start_imgs_scan(ImgFilesDir),
    {noreply, State}.


terminate(_Reason, #state{counter = Counter, 
                          start_time = StartTime}) ->
    ppool:stop_pool(?POOL_NAME),

    lager:info("imgs-to-tiles had done, sum: ~p, time: ~p(sec)", 
               [Counter, timer:now_diff(os:timestamp(), StartTime)/1000000]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

start_imgs_scan(ImgFilesDir) ->
    {ok, Files} = file:list_dir(ImgFilesDir),
    ImgFiles = lists:filter(fun(F) -> {ok, FileInfo} = file:read_file_info(ImgFilesDir ++ "/" ++ F), FileInfo#file_info.type==regular end, Files),
    lists:foreach(fun(ImgFileName) -> 
                ImgFile = ImgFilesDir ++ "/" ++ ImgFileName,
                case gdal_nif:is_imgfile(ImgFile) of
                    true ->
                        lager:info("start img_scan: ~s", [ImgFile]),
                        ppool:run(?POOL_NAME, [ImgFile]);
                    false ->
                        skip
                end
    end, ImgFiles).
