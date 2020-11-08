library(geosphere)

# find path

distnM <- function(x, timeCol, shipT, shipN, units = "secs", 
                   up_units = 30, ret = c("first", "last")){
  
  tab <- x[ship_type == shipT][SHIPNAME == shipN]
  
  setorderv(tab, timeCol)
  
  tab <- tab[, .(LON2 = LON, LAT2 = LAT, 
                 LON = shift(LON), LAT = shift(LAT),
                 dist = distHaversine(tab[, .(LON, LAT)],
                                      tab[, .(shift(LON), shift(LAT))]),
                 tm = difftime(get(timeCol), shift(get(timeCol)), units = units))
             ][tm <= up_units]
  
  if(nrow(tab) < 1) return(NULL)

  ret <- match.arg(ret)
  
  cor <- switch(ret, 
                "first" = tab[dist == max(dist, na.rm = TRUE), first(.SD)], 
                "last" = tab[dist == max(dist, na.rm = TRUE), last(.SD)])
  
  return(cor)
}
