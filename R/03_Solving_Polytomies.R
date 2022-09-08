library(ape)

res_phylo_marine <- readRDS(here::here("output", "phylo_marine.rds"))
phy_marine <- res_phylo_marine$Phylogeny
insertions_marine <- res_phylo_marine$Insertions_data


devtools::install_github("davidnipperess/PDcalc",build_vignettes = TRUE)
library(PDcalc)

help(package = "PDcalc")
phy_marine


test <- bifurcatr_2(phy = phy_marine, runs = 2)

lsf.str("package:PDcalc")
quartz()
plot(test[[1]])
