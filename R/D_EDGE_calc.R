
# data and library --------------------------------------------------------

library(phyloregion)
library(rfishbase)
library(rredlist)
spp_df <- read.table(here::here("data", "taxa_table.txt"), header = TRUE) # species cleaned
res_phylo_marine <- readRDS(file = here::here("output", "phylo_marine.rds"))

phylo_marine <- res_phylo_marine$Phylogeny
db_fishbase <- species() 



# cleaning data -----------------------------------------------------------

cons_status <- db_fishbase$Vulnerability # check if this can be used as a proxy for IUCN endangered categories
cons_status_spp_fb <- db_fishbase[which(is.na(cons_status) != TRUE), ]
status_df_spp <- cons_status_spp_fb[which(is.na(match(spp_df$s, unique(gsub(" ", "_", cons_status_spp_fb$Species)))) != TRUE), "Vulnerability"]

# calculate EDGE ----------------------------------------------------------

data(africa)
y <- EDGE(x=africa$IUCN, phy=africa$phylo, Redlist="IUCN", species="Species")
