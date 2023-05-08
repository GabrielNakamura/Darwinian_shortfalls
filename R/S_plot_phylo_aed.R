
# libraries and data ------------------------------------------------------

library(ape)
library(ggplot2)
library(here)
library(ggtree)
library(ggtreeExtra)
library(phyloregion)
library(dplyr)

# data
phy <- ape::read.tree(here::here("data", "phylo_final.tre"))
aed <- read.table(here::here("data", "processed", "aed_final.txt"))
names_spp <- rownames(aed)[match(phy$tip.label, rownames(aed))]
# aed <- rnorm(length(phy$tip.label))
cut_depth <- 110 # where the phylogeny is gonna be collapsed to be plotted
  
# collapsing nodes at 100 myr ---------------------------------------------

phy_collapse <- phyloregion::timeslice(phy = phy, n = cut_depth, collapse = FALSE) # collapsing nodes at a given depth
spp_depth <- phyloregion::get_clades(tree = phy, cut = cut_depth) # specis at each node depth
keep_spp <- unlist(lapply(spp_depth, function(x) sample(x, 1))) # sampling only one species to use in the plot
tmp_phy <- keep.tip(phy = phy_collapse, tip = keep_spp) # keeping the species in the tree
tmp_gg <- ggtree(tmp_phy) # converting to ggtree format
aed_t <- tibble::tibble(aed = aed$x, label = names_spp) # transforming to tibble 
aed_t_cut <- aed_t[match(keep_spp, aed_t$label), ] # keeping only the focal species in tibble
list_tib <- lapply(spp_depth, function(x) as.vector(aed_t[match(x, aed_t$label), "aed"])[[1]]) # finding the species aed values for each species
l <- unlist(lapply(spp_depth, function(x) length(x))) # transforming in a vector
df_aed_long <- data.frame(label = rep(tmp_phy$tip.label, times = l), aed = unlist(list_tib)) # df with only the focal species in the collapsed tree
df_aed_long$aed <- sqrt(df_aed_long$aed)

# calculating mean aed value for each collapsed groups of species
df_fim <- df_aed_long %>%
  group_by(label) %>%
  dplyr::summarize(Mean = mean(aed, na.rm = TRUE))

# calculating the quantile to be used as categories for the boxplots
df_fim$Quantile <- with(df_fim, cut(Mean, 
                                      breaks = quantile(Mean, probs = seq(0,1, by = 0.25), 
                                                        na.rm = TRUE), 
                                      include.lowest = TRUE))


tmp_gg$data <- left_join(tmp_gg$data, df_fim, by = "label") # joining to the tree the data containing mean aed values and categories based on quantiles



# plotting tree -----------------------------------------------------------

# base tree
tree2 <- tmp_gg +
  geom_fruit(
    data = df_aed_long,
    geom = geom_boxplot,
    mapping = aes(
      y = label,
      x = aed),
    offset = .1,
    outlier.size = 0.2,
    #outlier.stroke = 0.05,
    outlier.shape = 1,
    axis.params = list(
      axis       = "x",
      text.size  = 1.8,
      hjust      = 1,
      vjust      = 0.5,
      nbreak     = 3
    ),
    grid.params = list()
  )


# final tree with concentric circles at each 65 myr

final_tree <- tree2 + layout_fan(angle = 15) +
  annotate("rect", xmin = 65, xmax = 130, ymin = 0, ymax = 110,
           alpha = .1)  +
  annotate("rect", xmin = 195, xmax = 260, ymin = 0, ymax = 110,
           alpha = .1) +
  theme(legend.title = element_text(size = 9), legend.text = element_text(size = 7)) 

# saving plot -------------------------------------------------------------

cowplot::save_plot(here::here("output", "images", "Phylogeny_Aed.png"), final_tree,
                   base_height = 10, base_width = 12)
