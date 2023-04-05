
# reading data and libraries ----------------------------------------------

# Data
res_phylo_marine <- readRDS(here::here("output", "phylo_FishPhyloMaker.rds"))
phy_marine <- res_phylo_marine$Phylogeny
insertions_marine <- res_phylo_marine$Insertions_data

library(ggtree)
library(ggtreeExtra)
library(ggnewscale)
library(patchwork)
library(ggplot2)

# setting theme -----------------------------------------------------------

theme_set(theme_bw())

# barplot -------------------------------------------------------------


df_barplot_marine <- res_phylo_marine$Insertions_data
df_barplot_fresh <- insertions_fresh

# Raw insertion values marine
barplot_insertion_marine <-
  ggplot(data = df_barplot_marine, aes(x = o, fill = insertions)) +
  geom_bar(na.rm = TRUE) +
  rcartocolor::scale_fill_carto_d(palette = "Safe", 
                                  labels = c("Present",
                                             "Congeneric",
                                             "Family", 
                                             "Congeneric Family",
                                             "Order")
  ) +
  labs(y = "Total number of insertions", fill = "Type of insertion") +
  theme(legend.position = "bottom", panel.background = element_rect(fill = "transparent"),
        plot.margin = unit(c(0.4, 0.4, 0.4, 0.4), "mm"),
        legend.title = element_text(family = "Times",
                                    color = "black", face = "bold", size = 12),
        legend.text = element_text(family = "Times", color = "black", size = 12), 
        axis.text = element_text(family = "Times", color = "black", size = 8),
        axis.text.x = element_text(angle = 55, hjust = 1, size = 10),
        panel.border = element_blank(),
        axis.ticks = element_blank(),
        plot.subtitle = element_text(family = "Arial", 
                                     color = "black",
                                     size = 9, 
                                     hjust = 0.5, 
                                     margin = margin(b = 6)
        )
  )

barplot_insertion_marine


# Raw insertion values for freshwater 

barplot_insertion_fresh <-
  ggplot(data = df_barplot_fresh, aes(x = o, fill = insertions)) +
  geom_bar(na.rm = TRUE) +
  rcartocolor::scale_fill_carto_d(palette = "Safe", 
                                  labels = c("Present",
                                             "Congeneric",
                                             "Family", 
                                             "Congeneric Family",
                                             "Order")
  ) +
  labs(y = "Total number of insertions", fill = "Type of insertion") +
  theme(legend.position = "bottom", panel.background = element_rect(fill = "transparent"),
        plot.margin = unit(c(0.4, 0.4, 0.4, 0.4), "mm"),
        legend.title = element_text(family = "Times",
                                    color = "black", face = "bold", size = 12),
        legend.text = element_text(family = "Times", color = "black", size = 12), 
        axis.text = element_text(family = "Times", color = "black", size = 8),
        axis.text.x = element_text(angle = 55, hjust = 1, size = 10),
        panel.border = element_blank(),
        axis.ticks = element_blank(),
        plot.subtitle = element_text(family = "Arial", 
                                     color = "black",
                                     size = 9, 
                                     hjust = 0.5, 
                                     margin = margin(b = 6)
        )
  )
quartz()
barplot_insertion_fresh

# plotting tree -----------------------------------------------------------

# marine fishes

df_insertion_marine <- insertions_marine[match(insertions_marine$s, phy_marine$tip.label), ]
df_insertion_marine <- unique(df_insertion_marine)
df_barplot_marine <- na.omit(df_insertion_marine)
df_insertion_tree <- data.frame(df_insertion_marine$insertions[-which(is.na(df_insertion_marine$s) == TRUE)])
rownames(df_insertion_tree) <- df_insertion_marine$s[-which(is.na(df_insertion_marine$s) == TRUE)]
orders <- names(sort(table(insertions_marine$o), decreasing = TRUE)[1:5])
nodedf <- data.frame(nodes = unlist(lapply(orders, 
                                           function(x) phytools::findMRCA(tree = phy_marine, 
                                                                          tips = insertions_marine[which(insertions_marine$o == x)
                                                                                                   , "s"]
                                           )
)
))
names_df_order <- names(sort(table(insertions_marine$o), decreasing = TRUE)[c(3, 4, 6, 7, 8)])
nodedf$Orders <- names_df_order

# adjusting the order of insertions
insertions_marine$insertions <- factor(insertions_marine$insertions, 
                                       levels = c("Present_in_Tree", 
                                                  "Congeneric_insertion",
                                                  "Family_insertion",
                                                  "Congeneric_Family_level", 
                                                  "Order_insertion"
                                       )
)



# plotting tree marine -----------------------------------------------------------

phylo_all_marine <- 
  ggtree::ggtree(phy_marine, layout = "circular") + 
  geom_fruit(data = insertions_marine, geom = geom_tile,
             mapping = aes(y = s, x = insertions, fill = insertions),
             color = "grey50", offset = 0.1, size = 0.02, 
             pwidth = 0.3, stat = "identity") +
  rcartocolor::scale_fill_carto_d(palette = "Safe", 
                                  labels = c("Present",
                                             "Congeneric",
                                             "Family", 
                                             "Congeneric Family",
                                             "Order")) +
  labs(subtitle = "", fill = "Insertions") +
  theme(legend.position = "bottom", panel.background = element_rect(fill = "transparent"),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "mm"),
        legend.title = element_text(family = "Times", color = "black", face = "bold", size = 12),
        legend.text = element_text(family = "Times", color = "black", size = 12), 
        axis.text = element_blank(), panel.border = element_blank(), axis.ticks = element_blank(),
        plot.subtitle = element_text(family = "Arial", 
                                     color = "black",
                                     size = 9, 
                                     hjust = 0.5, 
                                     margin = margin(b = 6)
        )
  )

# freshwater fishes

df_insertion_fresh <- insertions_fresh[match(insertions_fresh$s, phy_fresh$tip.label), ]
df_insertion_tree_fresh <- data.frame(df_insertion_fresh$insertions)
rownames(df_insertion_tree_fresh) <- df_insertion_fresh$s
orders_fresh <- names(sort(table(insertions_fresh$o), decreasing = TRUE)[1:5])
nodedf_fresh <- data.frame(nodes = unlist(lapply(orders, 
                                                 function(x) phytools::findMRCA(tree = phy_fresh, 
                                                                                tips = insertions_fresh[which(insertions_fresh$o == x)
                                                                                                        , "s"]
                                                 )
)
))
names_df_order_fresh <- names(sort(table(insertions_fresh$o), decreasing = TRUE)[1:5])
nodedf_fresh$Orders <- names_df_order_fresh

# adjusting the order of insertions
insertions_fresh$insertions <- factor(insertions_fresh$insertions, 
                                      levels = c("Present_in_Tree", 
                                                 "Congeneric_insertion",
                                                 "Family_insertion",
                                                 "Congeneric_Family_level", 
                                                 "Order_insertion"
                                      )
)



# plotting tree freshwater -----------------------------------------------------------

phylo_all_fresh <- 
  ggtree::ggtree(phy_fresh, layout = "circular") + 
  geom_hilight(data = nodedf_fresh, mapping = aes(node = nodes), extendto = 400,
               alpha = 0.3, fill = "grey", color = "grey60",
               size = 0.05) +
  geom_cladelab(data = nodedf_fresh, 
                mapping=aes(node = nodes, 
                            label = Orders),
                hjust=0.5,
                angle="auto",
                barsize=NA,
                horizontal=FALSE, 
                fontsize=2.4,
                fontface="italic"
  ) +
  geom_fruit(data = insertions_fresh, geom = geom_tile,
             mapping = aes(y = s, x = insertions, fill = insertions),
             color = "grey50", offset = 0.1, size = 0.02, 
             pwidth = 0.3, stat = "identity") +
  rcartocolor::scale_fill_carto_d(palette = "Safe", 
                                  labels = c("Present",
                                             "Congeneric",
                                             "Family", 
                                             "Congeneric Family",
                                             "Order")) +
  labs(subtitle = "", fill = "Insertions") +
  theme(legend.position = "bottom", panel.background = element_rect(fill = "transparent"),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "mm"),
        legend.title = element_text(family = "Times", color = "black", face = "bold", size = 12),
        legend.text = element_text(family = "Times", color = "black", size = 12), 
        axis.text = element_blank(), panel.border = element_blank(), axis.ticks = element_blank(),
        plot.subtitle = element_text(family = "Arial", 
                                     color = "black",
                                     size = 9, 
                                     hjust = 0.5, 
                                     margin = margin(b = 6)
        )
  )


# saving figures ----------------------------------------------------------

# phylogeny marine
ggsave(filename = here::here("output", "phylo_all_marine.png"),
       plot = phylo_all_marine,
       width = 7, height = 8, 
       dpi = 300)


# barplot insertions marine
ggsave(here::here("output", "images", "barplot_insertions_marine.png"), 
       plot = barplot_insertion_marine,
       width = 7, height = 7,
       dpi = 500)


# phylogeny fresh
ggsave(filename = here::here("output", "images", "phylo_all_fresh.png"),
       plot = phylo_all_fresh,
       width = 7, height = 8, 
       dpi = 500)


# barplot insertions fresh
ggsave(here::here("output", "images", "barplot_insertions_fresh.png"), 
       plot = barplot_insertion_fresh,
       width = 7, height = 7,
       dpi = 500)


