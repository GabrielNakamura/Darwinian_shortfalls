library(rtrees)
library(FishPhyloMaker)


# Insertion with fishphylomaker -------------------------------------------
'/Users/gabriel.nakamuradesouza/Library/CloudStorage/OneDrive-Personal/Manuscritos/Darwinian_shortfalls/data/taxa_table.txt'
spp_df <- read.table('/Users/gabriel.nakamuradesouza/Library/CloudStorage/OneDrive-Personal/Manuscritos/Darwinian_shortfalls/data/taxa_table.txt', header = TRUE)
spp_df <- read.table(here::here("Data", "taxa_table.txt"), header = TRUE)

# Run FishPhyloMaker  all species - Not working
res_phylo <- 
  FishPhyloMaker(data = spp_df, 
               insert.base.node = TRUE,
               return.insertions = TRUE,
               progress.bar = TRUE)


# Run FishPhyloMaker only for marine

med <- species(sub("_", " ", spp_df$s), fields = species_fields$habitat)

marine_spp <- spp_df[which(med$Fresh | med$Brack == 1), ]
res_phylo_marine <- 
  FishPhyloMaker(data = marine_spp, 
                 insert.base.node = TRUE,
                 return.insertions = TRUE,
                 progress.bar = TRUE)



# insertion with rtrees ---------------------------------------------------
list_spp_rtrees <- sp_list_df(sp_list = spp_df$s, taxon = "fish")


# saving objects ----------------------------------------------------------

saveRDS(object = res_phylo_marine, file = here::here("phylo_marine.rds"))
table(res_phylo_marine$Insertions_data$insertions)["Order_insertion"]/sum(table(res_phylo_marine$Insertions_data$insertions))
