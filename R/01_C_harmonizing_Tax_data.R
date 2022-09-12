library(rfishbase)
library(tidyverse)
library(taxadb)
library(rFishTaxa)



# Eschmeyer's Catalog  ----------------------------------------------------

db <- species_family()
all_families_Esch <- db$family


all_spp_family_Esch <-
  search_cas(query = all_families_Esch, 
             type = "species_family")
all_spp_Esch <- all_spp_family_Esch[-which(is.na(all_spp_family_Esch$species) == T), ]
spp_esch <- data.frame(spp = all_spp_Esch$species, status = all_spp_Esch$status)
status <- spp_esch[match(unique(spp_esch$spp), spp_esch$spp), "status"]
df_esch <- data.frame(spp_esch = unique(spp_esch$spp), status = status)

# Fishbase ----------------------------------------------------------------

db_fishbase <- species()
length(unique(db_fishbase$Species))
colnames(db_fishbase)
df_fishbase <- data.frame(spp_FB = db_fishbase$Species, code = db_fishbase$Species)


# manual harmonization ----------------------------------------------------

# simple match between two datasets
# sum(is.na(match(unique(valid_spp_Esch$species),
#                 db_fishbase$Species)
# )
# ) # difference in the number of species between two datasets
# spp_esch_not_fb <- 
#   valid_spp_Esch$species[which(is.na(match(unique(valid_spp_Esch$species), 
#                                            db_fishbase$Species)) == TRUE)]
# 
# status_spp_esch_not_fb <- search_cas(query = spp_esch_not_fb, type = "species")
# colnames(status_spp_esch_not_fb)
# table(status_spp_esch_not_fb$status)
# dim(status_spp_esch_not_fb)
# valid_sppEsch_notFb <- status_spp_esch_not_fb[which(status_spp_esch_not_fb$status == "Validation"), ]
# spp_esch_not_fb_2 <-  valid_sppEsch_notFb$species[which(is.na(match(valid_sppEsch_notFb$species, 
#                                                                     db_fishbase$Species)) == TRUE)] # checking the species in Eschmeyer and FB
# match(spp_esch_not_fb_2, db_fishbase$Species)


# harmonizing with taxadb -------------------------------------------------

joined_esch_fb <- full_join(df_esch, df_fishbase, by = c("spp_esch" = "spp_FB")) # just joining all names 

# now getting IDs from Catalog of Life 
esch <- df_esch %>% mutate(id = get_ids(spp_esch, "col"))

fishbase <- df_fishbase %>% mutate(id = get_ids(spp_FB, "col"))
esch_code <- esch[-which(is.na(esch$id) == TRUE), ]
fb_code <- fishbase[-which(is.na(fishbase$id) == TRUE), ]
joined_esch_fb <- full_join(esch_code[, c(1,3)], fb_code[, c(1,3)], by = c("id")) # most reliable level of similarity btween datasets

joined_esch_fb <- full_join(esch[, c(1,3)], fishbase[, c(1,3)], by = c("id")) # just joining all names 
dim(joined_esch_fb)
