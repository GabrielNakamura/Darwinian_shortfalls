# Testing graft species with rtrees up to family level

# reading libraries
library(rtrees)
library(tidytree)
library(ape)
library(phytools)

# list of species 
data <- read.table(here::here("data", "taxa_table.txt"), header = T) # data from fishbase containing all valid names of fish species

# grafting species up to family level -------------------------------------

genus <- unlist(lapply(strsplit(x = data$s, split = "_"), function(x) x[[1]]))
data$genus <- genus
data_new <- data.frame(species = data$s, genus = data$genus, family = data$f) # data formated accordingly to rtrees required format

# getting the tree up to family level using rtrees 
sp_tree <- rtrees::get_tree(sp_list = data_new, taxon = 'fish',
                            scenario = 'at_basal_node') # phylo object


tb_tree <- as_tibble(sp_tree) # tibble obj
tb_graft <- sp_tree$graft_status # status of insertion


no_family <- sp_tree$graft_status[which(sp_tree$graft_status$status == "skipped as no co-family in the megatree"), "species"]
no_family <- as.vector(no_family)


# getting species and nodes to be added at order level --------------------

tax <- fishtree::fishtree_taxonomy() # taxonomy from rfishtree
orders <-  tax[which(tax$rank == "order"), ]
orders_no_incertae <- orders[-grep("Incertae sedis in", orders$name), ] # removing incertae sedis

# getting species names for all species at order level
spp_order <- lapply(orders_no_incertae$name, function(x) fishtree::fishtree_taxonomy(ranks = x)[[1]]$sampled_species)
names(spp_order) <- orders_no_incertae$name
spp_order <- lapply(spp_order, function(x) gsub(" ", "_", x))

# getting all node position for orders
# mrca_order_node <- lapply(spp_order, function(x) phytools::findMRCA(sp_tree, tips = x))
mrca_order_node <- lapply(spp_order, function(x) tidytree::MRCA(tb_tree, c(tb_tree[na.omit(match(x, tb_tree$label)), "label"])[[1]])) 


# species with no co-family in phylogenetic tree
data_not_family <- data[match(no_family$species, data$s), ]

# getting the nodes for each order of species to be inserted
data_order <- mrca_order_node[na.omit(match(names(table(data_not_family$o)), names(mrca_order_node)))]
names(data_order) # species with orders in the tree
data_order_node <- lapply(data_order[na.omit(match(data_not_family$o, names(data_order)))], function(x) x$node) # nodes of each order
order_node_info <- data_not_family[which(!is.na(match(data_not_family$o, names(data_order))) == TRUE), ]
order_node_info$OrdNode <- unlist(data_order_node)
order_node_info$NameCheckOrd <- names(unlist(data_order_node)) # data frame with all nodes of orders for all species to be grafted

# grafting species --------------------------------------------------------

tree_update_order <- as.phylo(tb_tree)
tree_update_order <- phytools::force.ultrametric(tree_update_order, method = "extend") # this take some time 

# just a bar progress
pb_order <- progress::progress_bar$new(format = "Adding species to order nodes [:bar] :percent", 
                                       total = dim(order_node_info)[1], clear = FALSE, 
                                       width = 60, current = "<", incomplete = ">", 
                                       complete = ">")

# binding species to order node
for(i in 1:dim(order_node_info)[1]){
  tree_update_order <-
    phytools::bind.tip(tree = tree_update_order, 
                       tip.label = order_node_info$s[i], 
                       where = order_node_info$OrdNode[i], 
                       position = 0) # position can be a value provided by the user
  pb_order$tick()
}



# insertion table ---------------------------------------------------------

pos <- match(order_node_info$s, tb_graft$species)
tb_graft[pos, "status"] <- "grafted at order level"
tb_graft[which(tb_graft$status == "skipped as no co-family in the megatree"), "status"] <- "not inserted"
table(tb_graft$status)
tb_graft[which(tb_graft$status == "not inserted"), "species"]

data[match(tb_graft[which(tb_graft$status == "not inserted"), "species"]$species, data$s), ] # not grafted at any level

# saving trees and insertion table ----------------------------------------

saveRDS(object = as.phylo(tb_tree), here::here("output", "family_tree.rds")) 
saveRDS(object = tree_update_order, here::here("output", "order_tree.rds"))
saveRDS(object = tb_graft, here::here("output", "graft_table.rds"))


