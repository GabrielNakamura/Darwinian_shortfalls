

library(patchwork)
res_phylo <- readRDS(here("output", "phylo_marine.rds"))
phylo = res_phylo$Phylogeny
data = res_phylo$Insertions_data
level = c("Congeneric_insertion", "Family_insertion", "Order_insertion")

t <- which(sf_darwinian$PDdeficit == max(sf_darwinian$PDdeficit))
s <- insertion_marine[match(gsub("\\.", "_", colnames(comm_sub[, which(comm_sub[t, ] >=1 )])), 
                       insertion_marine$s),]
phy <- ape::keep.tip(phy = phy_marine, tip = s$s)
names_exclude <- phy$tip.label[na.omit(match(s[which(s$insertions == "Present_in_Tree"), "s"], 
                                               phy$tip.label))]

insert <- ape::drop.tip(phy, tip = names_exclude)$tip.label
present <- ape::drop.tip(phylo, tip = exclude)$tip.label

PD_deficit(phylo = phy, data = s, level = c("Congeneric_insertion", "Family_insertion", "Order_insertion"))

insertions.types <- list(insert = insert,
                         present = present
                         )
tree.a <- groupOTU(phy, insertions.types)
e <- sub("^[^_]*_", "", tree.a$tip.label)
g <- substr(tree.a$tip.label, 1, 1)
tree.a$tip.label <- paste(g, e, sep = ".")
plot.a <- ggtree(tree.a)  + 
  geom_tiplab(size = 0.02) + labs(title = "Complete Tree") +
  theme(legend.position = c(0,1)) +
  geom_tiplab(size = 2, show.legend = FALSE) +
  xlim(0, 370)

plot.b <- ggtree(tree.a, aes(linetype = group))  +
  scale_color_manual(values = c("black", "#33CCCC"), breaks = "present") +
  scale_linetype_manual(values = c("solid", "dashed"), breaks = "insert") +
  labs(title = "Only insertion") +
  theme(legend.position = "none")
p <- plot.a + plot.b
ggsave(filename = here("output", "insertion_ex.png"), plot = p, device = "png", dpi = 200)
