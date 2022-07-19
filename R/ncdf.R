#' Retrieve the spatial resolution stated in the global metadata
#' 
#' @export
#' @param X ncdf4 object
#' @return 2 element [lon, lat] resolution vector
bsw_res <- function(X){
  c(0.25, 0.25)
}

#' Retrieve the time epoch stated in the dimension attribute
#' 
#' @export
#' @param X ncdf4 object
#' @return POSIXct time origin (possibly different than POSIX epoch origin)
bsw_t0 <- function(X){
  #dim$time$units units: hours since 1987-07-09 00:00:00.000 UTC
  as.POSIXct(X$dim$time$units,
             format = "hours since %Y-%m-%d %H:%M:%OS UTC",
             tz = "UTC")
}

#' Retrieve the time dimension of the NCDF object
#'
#' @export
#' @param X ncdf4 object
#' @param t0 POSIXct, the origin of the time dimension
#' @param form character, output format. One of "Date" or "POSIXct" (default)
#' @return vector fo time stamps as determined by \code{form} argument
bsw_time <- function(X,
                     t0 = bsw_t0(X),
                     form = c("Date", "POSIXct")[2]){
  stopifnot(inherits(X, 'ncdf4'))
  if (!("time" %in% names(X$dim))) stop("time dimension not found")
  time <- X$dim$time$vals * 3600 + t0
  switch(tolower(form[1]),
         "date" = as.Date(time),
         time)
}

#' Retrieve the bsw type (derived form the path - terrible practice! - but works)
#'
#' @export
#' @param X ncdf4 object
#' @return charcater of the type
bsw_type <- function(X){
  stopifnot(inherits(X, "ncdf4"))
  basename(dirname(X$filename))
}


#' Retrieve bsw variables
#' 
#' @export
#' @param X ncdf4 object
#' @param drop character ietms to not list as variables.  Set to 'none' to drop none.
#' @return character vector
bsw_vars <- function(X, drop = c("time_run", "time_offset")){
  if (inherits(X, "ncdf4")){
    x <- names(X$var)
  } else {
    # this is a union of wind and stress, so a jumble
    x <- c("u", "v", "w", "taux", "tauy", "tau")
  }
  x[!(x %in% drop)]
}

#' Retrieve bsw navigation values (start, count, lons, lats)
#'
#' @export
#' @param X ncdf4 object
#' @param g geometry object that defines point locations
#' @param res numeric, 2 element resolution \code{[res_x,res_y]}
#' @param varname character the name of the variable
#' @param time numeric two elements time indexing \code{[start, length]}.
#'   \code{start} is a 1-based index into the time dimension
#'   \code{length} is the number of indices to retrieve (assumed to be contiguous sequence)
#' @param lev numeric two elements time indexing \code{[start, length]}.
#'   \code{start} is a 1-based index into the level dimension
#'   \code{length} is the number of indices to retrieve (assumed to be contiguous sequence)
#' @return data frame 
#' \itemize{
#'   \item{g the requested lonlats}
#'   \item{res the resolution}
#'   \item{start vector of start values for \code{\link[ncdf4]{ncvar_get}}}
#'   \item{count vector of count values \code{\link[ncdf4]{ncvar_get}}}
#'   \item{ext vector of extent see \code{\link[raster]{raster}}}
#'   \item{crs character, proj string for \code{\link[raster]{raster}}}
#'   \item{varname character}
#' }
bsw_nc_nav_point <- function(X, g,
                          res = bsw_res(X),
                          #time = c(35000, 1),
                          lev = c(1, -1),
                          varname = bsw_vars(X)){
  
  stopifnot(inherits(X, 'ncdf4'))
  if (!(varname[1] %in% names(X$var))) stop("varname not known:", varname[1])
  if (length(res) == 1) res <- c(res[1],res[1])
  half <- res/2
  d <- xyzt::get_geometry_dimension(g)
  if (!(d %in% c("XYM", "XYZ")))stop("time must be included in xyzt coordinates")
  
  # accepts one row tibble (key is empty since we are rowwise)
  # X the ncdf4 objecy
  # returns tibble with start and count appended [x, y, level, time]
  locate_xyt <- function(tbl, key, X = NULL){
    ix <- sapply(tbl[['X']],
                 function(x){
                   which.min(abs(X$dim$lon$vals - x))[1]
                 })
    iy <- sapply(tbl[['Y']],
                 function(y){
                   which.min(abs(X$dim$lat$vals - y))[1]
                })
    iz <- sapply(tbl[['T']],
                 function(z, t0 = NULL){
                   which.min(abs(bsw_time(X) - (z + t0)))[1]
                 }, t0 = xyzt::POSIX_epoch())
    
    start <-  unname(c(ix,iy, lev[1], iz))
    count <- rep(1, length(start))
    tbl |>
      dplyr::mutate(start = list(start), count = list(count))
  }
  
  name_t <- function(x) {"T"} 
  xy <- sf::st_coordinates(g) |>
    dplyr::as_tibble() |>
    dplyr::select(dplyr::any_of(c("X", "Y", "M", "Z"))) |>
    dplyr::rename_with(name_t, dplyr::any_of(c("M", "Z"))) |>
    dplyr::rowwise() |>
    dplyr::group_map(locate_xyt, X = X) |>
    dplyr::bind_rows() |>
    dplyr::mutate(varname = paste(varname, collapse = ",")) |>
    tidyr::separate_rows(.data$varname, sep = ",")
  
  xy
}




#' Retrieve bsw navigation values (start, count, lons, lats)
#'
#' @export
#' @param X ncdf4 object
#' @param g geometry object that defines a bounding box and possibly coded with time (XYM/XYZ)
#' @param res numeric, 2 element resolution \code{[res_x,res_y]}
#' @param varname character the name of the variable
#' @param time numeric two elements time indexing \code{[start, length]}. Ignored if
#'   the geometry is coded with a 3rd dimension
#'   \code{start} is a 1-based index into the time dimension
#'   \code{length} is the number of indices to retrieve (assumed to be contiguous sequence)
#' @param lev numeric two elements time indexing \code{[start, length]}.
#'   \code{start} is a 1-based index into the level dimension
#'   \code{length} is the number of indices to retrieve (assumed to be contiguous sequence)
#' @return list with
#' \itemize{
#'   \item{bb the requested bounding box}
#'   \item{res the resolution}
#'   \item{start vector of start values for \code{\link[ncdf4]{ncvar_get}}}
#'   \item{count vector of count values \code{\link[ncdf4]{ncvar_get}}}
#'   \item{ext vector of extent see \code{\link[raster]{raster}}}
#'   \item{crs character, proj string for \code{\link[raster]{raster}}}
#'   \item{varname character}
#' }
bsw_nc_nav_bb <- function(X, g,
                       res = bsw_res(X),
                       time = c(35000, 1),
                       lev = c(1, 1),
                       varname =  bsw_vars(X)){
  
  stopifnot(inherits(X, 'ncdf4'))
  if (!(varname[1] %in% names(X$var))) stop("varname not known:", varname[1])
  if (length(res) == 1) res <- c(res[1],res[1])
  half <- res/2
  
  # if the polyon contains time then we override any user supplied values 
  # for time
  d <- get_geometry_dimension(g)
  if (nchar(d) == 3){
    xyz <- sf::st_coordinates(g)
    btimes <- bsw_time(X)
    time <- c(findInterval(xyz[1,3] + xyzt::POSIX_epoch(), btimes),1)
  }
  
  bb <- sf::st_bbox(g) |> as.numeric()
  bb <- bb[c(1,3,2,4)]

  bb2 <- bb + c(-half[1], half[1], -half[2], half[2])
  ix <- sapply(bb2[1:2],
               function(xbb) which.min(abs(X$dim$lon$vals - xbb)))
  we <- X$dim$lon$vals[ix]
  iy <- sapply(bb2[3:4],
               function(ybb) which.min(abs(X$dim$lat$vals-ybb)))
  sn <- X$dim$lat$vals[iy]
  
  list(bb = bb,
       res = res,
       start = c(ix[1], iy[1], lev[1], time[1]),
       count = c(ix[2] - ix[1] + 1, iy[2] - iy[1] + 1, lev[2], time[2]),
       ext = c(we + (half[1] * c(-1,1)), sn + (half[2] * c(-1,1)) ),
       crs = 4326,
       varname = varname)
}
