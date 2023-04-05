
# Data, libraries and functions ------------------------------------------------------
library(ape)
library(FishPhyloMaker)

# Data
p <- readRDS(here::here("output", "tree_graft_final.rds")) # phylogenetic tree
i <- readRDS(here::here("output", "data_insertion.rds")) # data with insertions

# functions
source(here::here("R", "functions", "function_parallel_bifurctr.R")) # function to solve politomies


# solving polytomies (Sunplin method Rangel et al) ------------------------
phylo_solve <- bifurcatr_parallel(tree = p, 
                                  runs = 1000, 
                                  parallel = 4)
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


