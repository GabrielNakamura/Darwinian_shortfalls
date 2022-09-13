
# reading data and libraries ----------------------------------------------
library(countrycode)
library(CoordinateCleaner)
library(phyloregion)
library(terra)
library(here)
library(ggplot2)
library(sf)
library(patchwork)
library(dplyr)
# occ_all <- read.csv(here("data", "gbif_occ_allActino.csv"), header = TRUE, sep = ";")
occ_all <- read.csv2(here("data", "occ_perciformes.csv"), header = TRUE, sep = "\t")
shp_marine <- sf::read_sf(here::here("data", "shp_marine_MEOW"), as_tibble = T)
# shp_fresh <- sf::read_sf(here::here("data", "shp_freshBasin"), as_tibble = T)
shp_fresh <- sf::read_sf(here::here("data", "shp_fresh_FEOW"), as_tibble = T)

class(occ_all$decimalLatitude)
occ_test <- data.frame(occ_all[1:500, ])
occ_test <- transform(occ_all, decimalLatitude = as.numeric(occ_all$decimalLatitude), decimalLongitude = as.numeric(occ_all$decimalLongitude))
class(occ_test$decimalLatitude)
rm(occ_test)
# cleaning coordinates ----------------------------------------------------

#select columns of interest
dat <- occ_test %>%
  dplyr::select(species, decimalLongitude, decimalLatitude, countryCode, individualCount,
                gbifID, family, taxonRank, coordinateUncertaintyInMeters, year,
                basisOfRecord, institutionCode)

# remove records without coordinates
dat <- dat%>%
  filter(!is.na(decimalLongitude))%>%
  filter(!is.na(decimalLatitude))

# cleaning with coordinate cleaner 
#convert country code from ISO2c to ISO3c
dat$countryCode <-  countrycode(dat$countryCode, origin =  'iso2c', destination = 'iso3c')


#flag problems
dat <- data.frame(dat)
flags <- clean_coordinates(x = dat,
                           lon = "decimalLongitude",
                           lat = "decimalLatitude",
                           countries = "countryCode",
                           species = "species",
                           tests = c("capitals", "centroids", "equal","gbif", "institutions",
                                     "zeros", "countries")) # most test are on by default

saveRDS(object = flags, file = here("data", "flags_coord.rds"))

# removing flagged occs

dat_cl <- dat[-unique(which(flags$.zer == FALSE), 
                which(flags$.val == FALSE), 
                which(flags$.gbf == FALSE),
                which(flags$.inst == FALSE), 
                which(flags$.equ == FALSE),
                which(flags$.cap == FALSE)), ]
saveRDS(dat_cl, file = here("data", "processed", "dat_cl_test.rds"))

# intersection ecoregions and species occurrences -------------------------------------

# converting points to spatial points
points_test <- st_as_sf(x = dat_cl, coords = c("decimalLongitude", "decimalLatitude")) 

st_crs(points_test) <- st_crs(shp_marine) # same projection to points and shp

intersection_marine_test <- st_intersection(x = shp_marine, y = points_test) # all perciformes in marine ecoregions


# exploring shp for marine and fresh --------------------------------------

# marine
plot_realm <- 
ggplot() +
  geom_sf(data = shp_marine, aes(geometry = geometry, group = REALM, colour = REALM)) +
  labs(title = "Realms") +
  theme(legend.position="none", title = element_text())

plot_realm +
  geom_sf(data = points_test, aes(geometry = geometry))
  

plot_province <- 
ggplot() +
  geom_sf(data = shp_marine, aes(geometry = geometry, group = PROVINCE, colour = PROVINCE)) +
  labs(title = "Provinces") +
  theme(legend.position = "none", title = element_text())

plot_realm/plot_province

# freshwater

plot_fresh_ecoregion <- 
  ggplot() +
  geom_sf(data = shp_fresh, aes(geometry = geometry)) +
  labs(title = "Fresh Basins") +
  theme(legend.position="none", title = element_text())
