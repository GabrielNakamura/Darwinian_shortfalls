
# reading data and libraries ----------------------------------------------

library(here)
library(phyloregion)
library(raster)
d <- read.csv(here("data", "processed", "filtered_occ.csv"), header = TRUE) # reading occurrence records
m <- shapefile(here("data", "shp_marine_MEOW", "meow_ecos.shp")) # marine ecoregions 
f <- shapefile(here("data", "shp_fresh_tedesco", "Basin042017_3119.shx"))


# spliting species in 10 chunks -------------------------------------------

d <- d[!d$species %in% "", ]

s <- unique(d$species)
k <- length(s)/10
xx <- split(s, (seq_along(s)-1) %/% k)

# community matrix for MEOW -----------------------------------------------

names(m)[2] <- "grids"
l <- phyloregion:::progress(xx, function(x) {
  dd <- d[d$species %in% x, ]
  p <- points2comm(dat = dd, lon = "lon", lat = "lat", shp.grids = m)
  q <- sparse2long(p$comm_dat)
  return(q)
})
r1 <- do.call(rbind, l)
r2 <- unique(r1)

# community matrix for FEOW -----------------------------------------------

names(f)[1] <- "grids"
z <- phyloregion:::progress(xx, function(x) {
  dd <- d[d$species %in% x, ]
  p <- points2comm(dat = dd, lon = "lon", lat = "lat", shp.grids = f)
  q <- sparse2long(p$comm_dat)
  return(q)
})
r3 <- do.call(rbind, z)
r4 <- unique(r3)

# community matrix for grids ----------------------------------------------


# saving community data  --------------------------------------------------

write.csv(r1, here("data", "processed", "abundance_MEOW.csv"), row.names = FALSE)
write.csv(r2, here("data", "processed", "presab_MEOW.csv"), row.names = FALSE)

write.csv(r3, here("data", "processed", "abundance_fresh.csv"), row.names = FALSE)
write.csv(r4, here("data", "processed", "presab_fresh.csv"), row.names = FALSE)

  




