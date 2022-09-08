
# Data, libraries and functions ------------------------------------------------------
library(ape)
library(FishPhyloMaker)

# Data
res_phylo_marine <- readRDS(here::here("output", "phylo_marine.rds"))
phy_marine <- res_phylo_marine$Phylogeny
insertions_marine <- res_phylo_marine$Insertions_data

# functions
source(here::here("R", "functions", "function_parallel_bifurctr.R"))


# solving polytomies (Sunplin method Rangel et al) ------------------------
phylo_solve_100_marine <- bifurcatr_parallel(tree = phy_marine, runs = 5, parallel = 3)


# Darwinian shortfall for all trees ---------------------------------------

# calculating for all phylogenetic trees 
Darwinian_shortfall_marine <- 
  lapply(phylo_solve_100_marine, function(x){
  FishPhyloMaker::PD_deficit(phylo = x, 
                             data = insertions_marine, 
                             level = c("Congeneric_insertion", 
                                       "Family_insertion",
                                       "Order_insertion")
  )
}
) 

deficits_all <- do.call(rbind, Darwinian_shortfall_marine)

# base tree with polytomies
Darwinian_shortfall_marine_base <- 
  FishPhyloMaker::PD_deficit(phylo = res_phylo_marine$Phylogeny, 
                             data = insertions_marine, 
                             level = c("Congeneric_insertion", 
                                       "Family_insertion",
                                       "Order_insertion")
  )
