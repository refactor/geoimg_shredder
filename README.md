geoimg_shredder
===============

geo-image shredder, cut out tiles along space-cell, like gdal2tiles


application:start(compiler), application:start(syntax_tools), application:start(lager).
lager:set_loglevel(lager_console_backend, debug).
{ok,Pid} = img_scanner:start_link(global_mercator, "../tz/tzh.tif", {"10.0.72.1", 8087}).

