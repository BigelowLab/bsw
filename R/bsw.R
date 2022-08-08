#' Provides a brief tally/overview of available data 
#' 
#' @export
#' @return a tibble of summary information
bsw_tally <- function(){

  pp <- bsw_product(x = NULL)
  xx <- lapply(names(pp), function(p) {ncdf4::nc_open(bsw_url(p))}  )
  names(xx) <- sapply(xx, bsw_type)
  
  vv <- sapply(xx,
               function(x){
                 paste(names(x$var)[-c(1:2)], collapse = ", ")
               })
  t0 <- sapply(xx, bsw_t0) |>
    as.POSIXct(origin = "1970-01-01 00:00", tz = "UTC") 
  
  tn <- sapply(xx, function(x) x$dim$time$len)
  ok <- lapply(xx, ncdf4::nc_close)
  
  
  dplyr::tibble(name = names(xx), 
                longname = names(pp), 
                vars = vv,
                t0 = t0,
                time_count = tn)
}


#' Retrieve the base URL for BSW
#' @export
#' @return character, the base URL
bsw_base_url <- function(){
  "https://www.ncei.noaa.gov/thredds/dodsC/uv"
}

#' Retrieve a named character vector of paths relative to the root path.
#' Each element is named with the product name
#' 
#' @export
#' @param x missing or character, if missing return all products. If character,
#'   return only those requested
#' @return named character vector
bsw_product <- function(x = NULL){
  p <- c(
    "Aggregation_of_6h_Ocean_Wind" = 
      "6h_agg/Aggregation_of_6h_Ocean_Wind_best.ncd", 
    "Aggregation_of_Daily_Ocean_Wind"=
      "daily_agg/Aggregation_of_Daily_Ocean_Wind_best.ncd",
    "Aggregation_of_Monthly_Ocean_Wind" = 
      "monthly_agg/Aggregation_of_Monthly_Ocean_Wind_best.ncd",
    "Latest_Climatology_of_Ocean_Winds" = 
      "clm_agg/Latest_Climatology_of_Ocean_Winds_best.ncd",
    "Aggregation_of_6h_Ocean_Wind_Stress"= 
      "6h_strs_agg/Aggregation_of_6h_Ocean_Wind_Stress_best.ncd",
    "Aggregation_of_Daily_Ocean_Wind_Stress" = 
      "daily_strs_agg/Aggregation_of_Daily_Ocean_Wind_Stress_best.ncd",
    "Aggregation_of_Monthly_Ocean_Wind_Stress" = 
      "monthly_strs_agg/Aggregation_of_Monthly_Ocean_Wind_Stress_best.ncd")
  if (!is.null(x)) p <- p[x]
  p
}

#' Craft a bsw URL for a given date
#' 
#' @export
#' @param product character, the name of the product
#' @param root character, the root URL
#' @return one or more URLs
bsw_url <- function(product = c("Aggregation_of_6h_Ocean_Wind", 
                                "Aggregation_of_Daily_Ocean_Wind",
                                "Aggregation_of_Monthly_Ocean_Wind",
                                "Latest_Climatology_of_Ocean_Winds",
                                "Aggregation_of_6h_Ocean_Wind_Stress",
                                "Aggregation_of_Daily_Ocean_Wind_Stress",
                                "Aggregation_of_Daily_Ocean_Wind_Stress")[1],
                      root = bsw_base_url()){
  
  file.path(root, bsw_product(product[1]))             
}

