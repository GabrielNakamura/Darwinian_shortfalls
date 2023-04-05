
library(ape)
library(ggtree)
library(ggtreeExtra)
library(ggnewscale)
library(patchwork)
library(ggplot2)

tree <- readRDS(here::here("output", "tree_graft_final.rds"))
insertion_spp <- readRDS(here::here("output", "data_insertion.rds"))

df_insertion <- insertion_spp[match(insertion_spp$s, tree$tip.label), ]
df_insertion <- unique(df_insertion)
df_insertion_tree <- data.frame(df_insertion$insertion[-which(is.na(df_insertion$s) == TRUE)])
rownames(df_insertion_tree) <- df_insertion$s[-which(is.na(df_insertion$s) == TRUE)]
orders <- names(sort(table(insertion_spp$o), decreasing = TRUE)[1:5])
nodedf <- data.frame(nodes = unlist(lapply(orders, 
                                           function(x) phytools::findMRCA(tree = tree, 
                                                                          tips = insertion_spp[which(insertion_spp$o == x)
                                                                                                   , "s"]
                                           )
)
))
names_df_order <- names(sort(table(insertion_spp$o), decreasing = TRUE)[c(3, 4, 6, 7, 8)])
nodedf$Orders <- names_df_order

# adjusting the order of insertions
insertion_spp$insertions <- factor(insertion_spp$insertion, 
                                       levels = c("present in tree", 
                                                  "genus_insertion",
                                                  "family_insertion",
                                                  "Order_insertion"
                                       )
)



# plotting tree -----------------------------------------------------------


phylo_all <- 
  ggtree::ggtree(tree, layout = "circular") + 
  geom_fruit(data = df_insertion, geom = geom_tile,
             mapping = aes(y = s, x = insertion, fill = insertion),
             color = "grey50", offset = 0.1, size = 0.02, 
             pwidth = 0.3, stat = "identity") +
 # ggplot2::scale_fill_manual(
 #   name = "", 
 #   labels = c("Present", "Congeneric", "Family", "Other"),
 #   values = rev(col_five_hues)
 # ) +
  rcartocolor::scale_fill_carto_d(palette = "Vivid", 
                                  labels = c("Family",
                                             "Genus",
                                             "Order", 
                                             "Present")) +
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

ggsave(here::here("output", "images", "tree_all_spp.png"), 
       plot = phylo_all,
       width = 7, height = 7,
       dpi = 300)
