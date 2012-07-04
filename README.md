geoimg_shredder
===============

geo-image shredder, cut out tiles along space-cell, like gdal2tiles

erl -pz ebin -env ERL_LIBS ./deps/ -geoimg_shredder imgdir '"../tz"'

application:start(compiler), application:start(syntax_tools), application:start(lager).
application:start(ppool).
lager:set_loglevel(lager_console_backend, debug).

application:start(geo_utils).
application:start(geoimg_shredder).

img_picker:start_link({{"127.0.0.1", 8087}, global_geodetic). 

img_picker:scan_dir("../tz").



{ok,Pid} = img_cutter:start_link({"127.0.0.1", 8087}, global_geodetic, "../tz/tzh.tif").

