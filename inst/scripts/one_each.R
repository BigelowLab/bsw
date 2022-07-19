library(ncdf4)
library(bsw)
library(dplyr)


pp <- bsw_product(x = NULL)

xx <- lapply(names(pp), function(p) {nc_open(bsw_url(p))}  )
names(xx) <- sapply(xx, bsw_type)
names(xx)

vv <- lapply(xx,
             function(x){
               paste(names(x$var)[-c(1:2)], collapse = ", ")
             })

#lapply(xx, bsw_t0)

z = dplyr::tibble(name = names(xx), longname = names(pp), vars = vv)

ok <- lapply(xx, nc_close)
