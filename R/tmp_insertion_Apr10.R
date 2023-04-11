install.packages("letsR")
library(rfishbase)


saveRDS(object = as.phylo(tb_tree), here::here("output", "family_tree.rds")) 
saveRDS(object = tree_update_order, here::here("output", "order_tree.rds"))
saveRDS(object = tb_graft, here::here("output", "graft_table.rds"))


tb_graft <- readRDS(here::here("output", "graft_table.rds"))
table(tb_graft$status)
not_inserted <- data.frame(tb_graft[which(tb_graft$status == "skipped as no co-family in the megatree"), ])$species
not_inserted_family <- data.frame(tb_graft[which(tb_graft$status == "grafted at order level"), ])$species
gsub("_", " ", not_inserted)

order_not_inserted <- rfishbase::validate_names(gsub("_", " ", not_inserted))


tb_all <- rfishbase::load_taxa()
df_spp_order <- data.frame(tb_all[match(gsub("_", " ", order_not_inserted), tb_all$Species), c(2, 5, 6)])
genus <- unlist(lapply(strsplit(x = df_spp_order$Species, split = " "), function(x) x[[1]]))
df_spp_order$genus <- genus
df_ord <- df_spp_order[, c(1, 4, 2)]
colnames(df_ord) <- c("species", "genus", "family")
tree_test <- rtrees::get_tree(sp_list = df_spp_order$Species, taxon = "fish", scenario = "at_basal_node")
tree_test <- rtrees::get_tree(sp_list = df_ord, taxon = "fish", scenario = "at_basal_node")
data.frame(tree_test$graft_status)


df_family <- data.frame(tb_all[match(gsub("_", " ", not_inserted_family), tb_all$Species), ])
write.csv2(df_family, file = here::here("output", "test_data_family.csv"))
