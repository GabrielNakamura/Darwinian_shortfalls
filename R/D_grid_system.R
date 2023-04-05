
# using a grid system  ----------------------------------------------------


library(phyloregion)
library(raster)
library(here)
m <- shapefile(here("data", "shp_marine_MEOW", "meow_ecos.shp"))
e <- raster::extent(c(-180, 180, -90, 90))
p <- as(e, "SpatialPolygons")
r <- raster::raster(ncol = 180, nrow = 180, resolution = 2)
extent(r) <- extent(p)
r1 <- setValues(r, sample(x = 0:1, size = ncell(r), replace = TRUE))
p <- rasterToPolygons(r1, na.rm = FALSE)
p$grids <- paste0("v", 1:nrow(p))
p <- p[, "grids"]
rgdal::writeOGR(p, dsn = "data/grid_200", layer = "grid_200km", driver = "ESRI Shapefile")
plot(p, lwd = .3, add = TRUE)
