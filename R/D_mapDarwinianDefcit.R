
# reading data and functions ----------------------------------------------
phylo_all <- readRDS(here::here("output", "phylo_all.rds"))
insertions_all <- readRDS(here::here("output", "insertions_all.rds"))
all_taxa_names <- readRDS(here::here("output", "all_taxa_names.rds"))
drainage_basins <- read.csv(here::here("data", "Drainage_Basins_Table.csv"), sep = ";")
occ_drainage <- read.csv(here::here("data", "Occurrence_Table.csv"), sep = ";")
occ_drainage$X2.Species.Name.in.Source <- gsub("[.]", "_", occ_drainage$X2.Species.Name.in.Source)
occ_drainage$X6.Fishbase.Valid.Species.Name <- gsub("[.]", "_", occ_drainage$X6.Fishbase.Valid.Species.Name)

# libraries
library(FishPhyloMaker)

# correcting species names in occurrence data
basin_names <- unique(occ_drainage$X1.Basin.Name) # extracting basin names
user_name_wrong <- gsub(" ", "_", all_taxa_names$All_info_fishbase[which(all_taxa_names$All_info_fishbase$user_spp != 
                                                                           gsub(" ", "_", all_taxa_names$All_info_fishbase$valid_names)), 
                                                                   "user_spp"]
)
correct_names <- gsub(" ", "_", all_taxa_names$All_info_fishbase[which(all_taxa_names$All_info_fishbase$user_spp !=
                                                                         gsub(" ", "_", all_taxa_names$All_info_fishbase$valid_names)),
                                                                 "valid_names"])

wrong_occ <- occ_drainage[which(!is.na(match(occ_drainage$X6.Fishbase.Valid.Species.Name, user_name_wrong)) == TRUE), "X6.Fishbase.Valid.Species.Name"]
correct_occ <- gsub(" ", "_", all_taxa_names$All_info_fishbase[match(wrong_occ, all_taxa_names$All_info_fishbase$user_spp), "valid_names"])
occ_drainage[which(!is.na(match(occ_drainage$X6.Fishbase.Valid.Species.Name, user_name_wrong)) == TRUE),
             "X6.Fishbase.Valid.Species.Name"] <- correct_occ

list_spp_perBasin <- lapply(basin_names, 
                            function(x) occ_drainage[which(x == occ_drainage$X1.Basin.Name)
                                                     , "X6.Fishbase.Valid.Species.Name"]) # valid species names occurrence

names(list_spp_perBasin) <- unique(occ_drainage$X1.Basin.Name) # basin names

# prunning phylogeny for each basin
phy_perBasin <- lapply(list_spp_perBasin, function(x){
  rm_tip <- ape::drop.tip(phylo_all, tip = x)$tip.label
  ape::drop.tip(phylo_all, tip = rm_tip)
})

# Darwinian shortfall all basins ------------------------------------------

PD_deficit_all <- lapply(phy_perBasin, function(x){
  if(length(x) == 0){
    NA
  } else{
    PD_deficit(phylo = x, 
               data = insertions_all, 
               level = c("Congeneric_insertion",
                         "Family_insertion", "Order_insertion")
    ) 
  }
}
)


# Darwinian shortfall by orders -------------------------------------------------------

## siluriformes

phy_perBasin_Siluri <- lapply(phy_perBasin, function(x){
  if(length(x) == 0){
    NA
    } else{
      ape::drop.tip(x, 
                    ape::drop.tip(x, 
                                  tip = dplyr::filter(insertions_all, 
                                                      o == "Siluriformes")[, "s"])$tip.label)
    } 
} 
                              )


PD_deficit_Siluri <- lapply(phy_perBasin_Siluri, function(x){
  if(length(x) == 0 || is.na(x) == TRUE){
    NA
  } else{
    PD_deficit(phylo = x, 
               data = insertions_all, 
               level = c("Congeneric_insertion",
                         "Family_insertion"))
  }

} 
)

## Characiformes

phy_perBasin_Characi <- lapply(phy_perBasin, function(x){
  if(length(x) == 0){
    NA
  } else{
    ape::drop.tip(x, 
                  ape::drop.tip(x, 
                                tip = dplyr::filter(insertions_all, 
                                                    o == "Characiformes")[, "s"])$tip.label)
  }
} 
)


PD_deficit_Characi <- lapply(phy_perBasin_Characi, function(x) {
  if(length(x) == 0 || is.na(x) == TRUE){
    NA
  } else{
    PD_deficit(phylo = x, 
               data = insertions_all, 
               level = c("Congeneric_insertion",
                         "Family_insertion"))
  }
}
)

## Cipryniformes

phy_perBasin_Cyprini <- lapply(phy_perBasin, function(x){
  if(length(x) == 0){
    NA
  } else{
    ape::drop.tip(x, 
                  ape::drop.tip(x, 
                                tip = dplyr::filter(insertions_all, 
                                                    o == "Cypriniformes")[, "s"])$tip.label)
  }
} 
)



PD_deficit_Cyprini <- lapply(phy_perBasin_Cyprini, function(x) {
  if(length(x) == 0 || is.na(x) == TRUE){
    NA
  } else{
    PD_deficit(phylo = x, 
               data = insertions_all, 
               level = c("Congeneric_insertion",
                         "Family_insertion"))
  }
  }
  )

## Perciformes

phy_perBasin_Cichli <- lapply(phy_perBasin, function(x){
  if(length(x) == 0 || is.na(x) == TRUE){
    NA
  } else{
    ape::drop.tip(x, 
                  ape::drop.tip(x, 
                                tip = dplyr::filter(insertions_all, 
                                                    o == "Cichliformes")[, "s"])$tip.label)
  }
} 
)


PD_deficit_Cichli <- lapply(phy_perBasin_Cichli, function(x){
  if(length(x) == 0 || is.na(x) == TRUE){
    NA
  } else{
    PD_deficit(phylo = x, 
               data = insertions_all, 
               level = c("Congeneric_insertion",
                         "Family_insertion"))
  }
} 
)

## data frame deficit per order

PD_deficit_perOrder_characi <- do.call(rbind, PD_deficit_Characi)
df_PD_deficit_characi <- data.frame(BasinName = rownames(PD_deficit_perOrder_characi), Darwinian_deficit_characi = PD_deficit_perOrder_characi[, 4])
PD_deficit_perOrder_siluri <- do.call(rbind, PD_deficit_Siluri)
df_PD_deficit_siluri <- data.frame(BasinName = rownames(PD_deficit_perOrder_siluri), Darwinian_deficit_siluri = PD_deficit_perOrder_siluri[, 4])
PD_deficit_perOrder_cyprino <- do.call(rbind, PD_deficit_Cyprini)
df_PD_deficit_cyprino <- data.frame(BasinName = rownames(PD_deficit_perOrder_cyprino), Darwinian_deficit_cyprino = PD_deficit_perOrder_cyprino[, 4])
PD_deficit_perOrder_cichli <- do.call(rbind, PD_deficit_Cichli)
df_PD_deficit_cichli <- data.frame(BasinName = rownames(PD_deficit_perOrder_cichli), Darwinian_deficit_cichli = PD_deficit_perOrder_cichli[, 4])


# saving objects ----------------------------------------------------------
saveRDS(object = phy_perBasin, file = here::here("output", "phy_perBasin.rds"))
saveRDS(PD_deficit_all, file = here::here("output", "PD_deficit_all.rds"))
saveRDS(df_PD_deficit_characi, file = here::here("output", "df_PD_deficit_characi.rds"))
saveRDS(df_PD_deficit_siluri, file = here::here("output", "df_PD_deficit_siluri.rds"))
saveRDS(df_PD_deficit_cyprino, file = here::here("output", "df_PD_deficit_cyprino.rds"))
saveRDS(df_PD_deficit_cichli, file = here::here("output", "df_PD_deficit_cichli.rds"))


