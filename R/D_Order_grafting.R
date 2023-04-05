# Testing graft species with rtrees up to family level

# reading libraries
library(rtrees)
library(tidytree)

# list of species 
data <- read.table(here::here("data", "taxa_table.txt"), header = T) # data from fishbase containing all valid names of fish species
species <- data$s # only species names
?rtrees::sp_list_df()


# getting the tree up to family level using rtrees 
sp_tree <- rtrees::get_tree(sp_list = species, taxon = 'fish', scenario = 'at_basal_node') # phylo object
tb_tree <- as_tibble(sp_tree) # tibble obj

tb_graft <- sp_tree$graft_status
colnames(tb_graft)[1] <- "label"
tb_tree_graft <- 
  dplyr::full_join(tb_tree, tb_graft, by = "label") %>% 
  as.treedata()
no_family <- tb_tree_graft@data[which(tb_tree_graft@data$status == "skipped as no co-family in the megatree"), ]


# adding species at order level

tax <- fishtree::fishtree_taxonomy() # taxonomy from rfishtree
orders <-  tax[which(tax$rank == "order"), ]
orders_no_incertae <- orders[-grep("Incertae sedis in", orders$name), ] # removing incertae sedis
spp_order <- lapply(orders_no_incertae$name, function(x) fishtree::fishtree_taxonomy(ranks = x)[[1]]$sampled_species)
names(spp_order) <- orders_no_incertae$name
spp_order <- lapply(spp_order, function(x) gsub(" ", "_", x))

# all node position
mrca_order_node <- lapply(spp_order, function(x) tidytree::MRCA(tb_tree, c(tb_tree[na.omit(match(x, tb_tree$label)), "label"])[[1]])) 

# species with no co-family
data_not_family <- data[match(no_family$species, data$s), ]
data_not_family <- data_not_family[-which(data_not_family$o == "Incertae sedis in Eupercaria"), ]
data_order <- mrca_order_node[na.omit(match(names(table(data_not_family$o)), names(mrca_order_node)))]
names(data_order) # species with orders in the tree

# adding species to order node number
data_order_node <- lapply(data_order[na.omit(match(data_not_family$o, names(data_order)))], function(x) x$node)
order_node_info <- data_not_family[which(!is.na(match(data_not_family$o, names(data_order))) == TRUE), ]
order_node_info$OrdNode <- unlist(data_order_node)
order_node_info$NameCheckOrd <- names(unlist(data_order_node))

pb_order <- progress::progress_bar$new(format = "Adding species to order nodes [:bar] :percent", 
                                       total = dim(order_node_info)[1], clear = FALSE, 
                                       width = 60, current = "<", incomplete = ">", 
                                       complete = ">")

tree_update_order <- as.phylo(tb_tree)
is.ultrametric(tree_update_order)
tree_update_order <- force.ultrametric(tree_update_order, method = "extend")


# binding species to order node
for(i in 1:dim(order_node_info)[1]){
  tree_update_order <-
    phytools::bind.tip(tree = tree_update_order, 
                       tip.label = order_node_info$s[i], 
                       where = order_node_info$OrdNode[i], 
                       position = 0) # position can be a value provided by the user
  pb_order$tick()
}


# saving trees

saveRDS(object = as.phylo(tb_tree), here::here("output", "family_tree.rds")) 
saveRDS(object = tree_update_order, here::here("output", "order_tree.rds"))

