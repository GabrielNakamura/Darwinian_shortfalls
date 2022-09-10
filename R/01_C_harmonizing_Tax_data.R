library(rfishbase)
library(dplyr)
load_all(path = here::here())


# Eschmeyer's Catalog  ----------------------------------------------------

db <- species_family()
all_families_Esch <- db$family


all_spp_family_Esch <-
  search_cas(query = all_families_Esch, 
             type = "species_family")
dim(all_spp_family_Esch)
table(all_spp_family_Esch$status)
valid_spp_Esch <- all_spp_family_Esch[which(all_spp_family_Esch$status == "Validation"), ]
length(unique(valid_spp_Esch$species))

# Fishbase

db_fishbase <- species()
length(unique(db_fishbase$Species))
colnames(db_fishbase)

# simple match between two datasets
sum(is.na(match(unique(valid_spp_Esch$species),
                db_fishbase$Species)
)
) # difference in the number of species between two datasets
spp_esch_not_fb <- 
  valid_spp_Esch$species[which(is.na(match(unique(valid_spp_Esch$species), 
                                           db_fishbase$Species)) == TRUE)]

# searching species in Eschmeyer

status_spp_esch_not_fb <- search_cas(query = spp_esch_not_fb, type = "species")
colnames(status_spp_esch_not_fb)
table(status_spp_esch_not_fb$status)
dim(status_spp_esch_not_fb)
valid_sppEsch_notFb <- status_spp_esch_not_fb[which(status_spp_esch_not_fb$status == "Validation"), ]
spp_esch_not_fb_2 <-  valid_sppEsch_notFb$species[which(is.na(match(valid_sppEsch_notFb$species, 
                                                                    db_fishbase$Species)) == TRUE)] # checking the species in Eschmeyer and FB
match(spp_esch_not_fb_2, db_fishbase$Species)