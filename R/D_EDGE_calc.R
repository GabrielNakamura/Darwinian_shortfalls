
# data and library --------------------------------------------------------

library(phyloregion)
library(rfishbase)
library(rredlist)
library(tidyverse)

spp_df <- read.table(here::here("data", "taxa_table.txt"), header = TRUE) # species cleaned
res_phylo_marine <- readRDS(file = here::here("output", "phylo_marine.rds"))

phylo_marine <- res_phylo_marine$Phylogeny
insertions_marine <- res_phylo_marine$Insertions_data


# getting IUCN data -----------------------------------------------------------

Sys.getenv("IUCN_KEY") # API key for IUCN - saved as a system var - cant see here in local env
apikey <- Sys.getenv("IUCN_KEY") # calling the token number from system

df_species_names <- data.frame(species_name = insertions_marine$s)

# make a data frame of the species you want to iterate over
df_iucn_search <- 
  df_species_names %>% # now apply the rl_search function to each species using purrr::map()
  mutate(iucn_pull = map(species_name, rl_search, key = apikey)
         ) 


# cleaning results from Iucn

api_clean <- df %>% 
  mutate(category = map_chr(iucn_pull, pluck, "result", "category")) %>% 
  select(species_names, category)



# calculate EDGE ----------------------------------------------------------

data(africa)
y <- EDGE(x=africa$IUCN, phy=africa$phylo, Redlist="IUCN", species="Species")
