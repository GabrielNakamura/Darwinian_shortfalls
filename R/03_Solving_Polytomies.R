
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
system.time(phylo_solve_100_marine <- bifurcatr_parallel(tree = phy_marine, runs = 5, parallel = 3)
)
