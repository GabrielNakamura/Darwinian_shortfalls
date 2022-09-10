
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
phylo_solve_1000_marine <- bifurcatr_parallel(tree = phy_marine,
                                             runs = 1000,
                                             parallel = 6)
saveRDS(object = phylo_solve_1000_marine, file = here::here("output", "1000_trees_marine.rds"))

# Darwinian shortfall for all trees ---------------------------------------

# calculating for all phylogenetic trees 
Darwinian_shortfall_marine <- 
  lapply(phylo_solve_1000_marine, function(x){
  PD_deficit(phylo = x, 
                             data = insertions_marine, 
                             level = c("Congeneric_insertion", 
                                       "Family_insertion",
                                       "Order_insertion")
  )
}
) 

deficits_all <- do.call(rbind, Darwinian_shortfall_marine)

apply(deficits_all, 2, mean)
# base tree with polytomies
Darwinian_shortfall_marine_base <- 
  PD_deficit(phylo = res_phylo_marine$Phylogeny, 
                             data = insertions_marine, 
                             level = c("Congeneric_insertion", 
                                       "Family_insertion",
                                       "Order_insertion")
  )

