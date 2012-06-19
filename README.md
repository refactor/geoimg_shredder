geoimg_shredder
===============

geo-image shredder, cut out tiles along space-cell, like gdal2tiles


application:start(compiler), application:start(syntax_tools), application:start(lager).
lager:set_loglevel(lager_console_backend, debug).
{ok,Pid} = img_cutter:start_link(global_geodetic, "../tz/tzh.tif", {"127.0.0.1", 8087}).

