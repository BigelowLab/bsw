library(ncdf4)
library(xyzt)
library(bsw)
library(sf)

origx <- x <- read_sab() |>
  dplyr::select(-depth, -time) |>
  dplyr::mutate(lon = xyzt::to_360(lon),
                time = as.POSIXct("1995-12-18 22:05:00", tz = 'UTC')) |>
  xyzt::as_POINT(dims= "xyt")
  
  
  xyzt::as_BBOX(dims = 'xyt')

g <- sf::st_geometry(x) 

X <- nc_open(bsw_url("Aggregation_of_6h_Ocean_Wind"))

lon <- X$dim$lon$vals
lat <- X$dim$lat$vals


nav <- bsw_nc_nav_bb(X,x, varname = c("u", "v") )


uv <- bsw::extract(x, X, varname = c("u", "v"))

