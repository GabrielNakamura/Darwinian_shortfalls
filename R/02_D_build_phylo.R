
spp_df <- read.table(here::here("taxa_table.txt"), header = TRUE)
res_phylo <- 
  FishPhyloMaker(data = spp_df, 
               insert.base.node = TRUE,
               return.insertions = TRUE,
               progress.bar = TRUE)



med <- species(sub("_", " ", spp_df$s), fields = species_fields$habitat)

marine_spp <- spp_df[which(med$Fresh | med$Brack == 1), ]
res_phylo_marine <- 
  FishPhyloMaker(data = marine_spp, 
                 insert.base.node = TRUE,
                 return.insertions = TRUE,
                 progress.bar = TRUE)
saveRDS(object = res_phylo_marine, file = here::here("phylo_marine.rds"))
table(res_phylo_marine$Insertions_data$insertions)["Order_insertion"]/sum(table(res_phylo_marine$Insertions_data$insertions))
›