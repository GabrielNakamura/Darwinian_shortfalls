
# data,  libs and functions -----------------------------------------------

library(terra)
library(here)
library(FishPhyloMaker)
library(rFishTaxa)
library(phyloregion)
library(rfishbase)
library(sf)
library(ggplot2)
source(here("R", "functions", "function_PD_deficit.R"))
source(here("R", "functions", "function_PD_deficit2.R"))
comm_marine <- read.csv(here("data", "processed", "presab_MEOW.csv"))
comm_clean <- read.csv(here("data", "processed", "comm_clean_marine.csv"), row.names = 1)
res_phylo <- readRDS(here("output", "phylo_marine.rds"))
insertion_marine <- res_phylo$Insertions_data
phy_marine <- res_phylo$Phylogeny

# plotting richness in marine ecoregions ----------------------------------

v <- vect(here("data", "shp_marine_MEOW", "meow_ecos.shp")) # shapefile marine ecoregions
names(v)[2] <- "grids"
m <- data.frame(rowSums(comm_clean))
m$richness <- m$rowSums.comm_clean.
m$grids <- rownames(m)
m <- data.frame(m[, c(3, 2)])
r <- merge(v, m, by = "grids")
plot(r, "richness", type = "continuous", border = NA)


# plotting darwinian shorfall ---------------------------------------------


names_match_fb <- colnames(comm_clean)[which(gsub("\\.", "_", colnames(comm_clean)) %in% insertion_marine$s == TRUE)]

comm_sub <- comm_clean[, names_match_fb]

seq_comm <- 1:dim(comm_sub)[1]
basin_deficit_marine <- 
lapply(seq_comm, function(x){
  PD_deficit(phylo = phy_marine, data = insertion_marine[match(gsub("\\.", "_", colnames(comm_sub[, which(comm_sub[x, ] >=1 )])), 
                                                               insertion_marine$s),
  ], 
  level = c("Congeneric_insertion", "Family_insertion", "Order_insertion")
  )
})
deficit_marine <- do.call(rbind, basin_deficit_marine)
rownames(deficit_marine) <- rownames(comm_sub)
deficit_marine <- data.frame(deficit_marine, grids = rownames(comm_sub))

v <- vect(here("data", "shp_marine_MEOW", "meow_ecos.shp")) # shapefile marine ecoregions
names(v)[2] <- "grids"
s <- deficit_marine[, c("n_spp_present", "n_spp_insert", "PDdeficit", "Darwinian_deficit", "grids")]
r <- merge(v, s, by = "grids")
#r <- merge(r, m, by = "grids")
#plot(r, "Darwinian_deficit", type = "continuous", border = NA)
#plot(r, "richness", type = "continuous", border = NA)

sf_darwinian <- sf::st_as_sf(r)
sf_darwinian <- st_transform(sf_darwinian, 
                             crs = "+proj=robin")

map_PD_deficit_wrld <- ggplot() +
  geom_sf(data = sf_darwinian, aes(geometry = geometry, 
                                   fill = Darwinian_deficit),
          color = "transparent", size = 0.1, na.rm = T) +
  rcartocolor::scale_fill_carto_c(palette = "SunsetDark", 
                                  direction = 1, 
                                  limits = c(0, 0.6),  ## max percent overall
                                  breaks = seq(0, 0.6, by = .1),
                                  labels = glue::glue("{seq(0, 0.6, by = 0.1)}")) +
  guides(fill = guide_colorbar(barheight = unit(2.3, units = "mm"),  
                               barwidth = unit(100, units = "mm"),
                               direction = "horizontal",
                               ticks.colour = "grey20",
                               title.position = "top",
                               label.position = "bottom",
                               title.hjust = 0.5)) +
  labs(subtitle = "Proportion of total branch lenght inserted", fill = expression(PD[inserted]/PD[total])) +
  theme(legend.position = "bottom", panel.background = element_rect(fill = "transparent"),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "mm"),
        legend.title = element_text(family = "Times", color = "black", face = "bold", size = 12),
        legend.text = element_text(family = "Times", color = "black", size = 10), 
        axis.text = element_blank(), panel.border = element_blank(), axis.ticks = element_blank(),
        plot.subtitle = element_text(family = "Arial", 
                                     color = "black",
                                     size = 11, 
                                     hjust = 0.5, 
                                     margin = margin(b = 6))
  ) 

map_n_insertion_wrld <- ggplot() +
  geom_sf(data = sf_darwinian, aes(geometry = geometry, 
                                   fill = n_spp_insert),
          color = "transparent", size = 0.1, na.rm = T) +
  rcartocolor::scale_fill_carto_c(palette = "SunsetDark", 
                                  direction = 1, 
                                  limits = c(0, 800),  ## max percent overall
                                  breaks = seq(0, 800, by = 100),
                                  labels = glue::glue("{seq(0, 800, by = 100)}")) +
  guides(fill = guide_colorbar(barheight = unit(2.3, units = "mm"),  
                               barwidth = unit(100, units = "mm"),
                               direction = "horizontal",
                               ticks.colour = "grey20",
                               title.position = "top",
                               label.position = "bottom",
                               title.hjust = 0.5)) +
  labs(subtitle = "Number of species inserted", fill = "Number of species") +
  theme(legend.position = "bottom", panel.background = element_rect(fill = "transparent"),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "mm"),
        legend.title = element_text(family = "Times", color = "black", face = "bold", size = 12),
        legend.text = element_text(family = "Times", color = "black", size = 10), 
        axis.text = element_blank(), panel.border = element_blank(), axis.ticks = element_blank(),
        plot.subtitle = element_text(family = "Arial", 
                                     color = "black",
                                     size = 11, 
                                     hjust = 0.5, 
                                     margin = margin(b = 6))
  ) 

map_PD_isert_wrld <- ggplot() +
  geom_sf(data = sf_darwinian, aes(geometry = geometry, 
                                   fill = PDdeficit),
          color = "transparent", size = 0.1, na.rm = T) +
  rcartocolor::scale_fill_carto_c(palette = "SunsetDark", 
                                  direction = 1, 
                                  limits = c(0, 33000),  ## max percent overall
                                  breaks = seq(0, 33000, by = 10000),
                                  labels = glue::glue("{seq(0, 33000, by = 10000)}")) +
  guides(fill = guide_colorbar(barheight = unit(2.3, units = "mm"),  
                               barwidth = unit(100, units = "mm"),
                               direction = "horizontal",
                               ticks.colour = "grey20",
                               title.position = "top",
                               label.position = "bottom",
                               title.hjust = 0.5)) +
  labs(subtitle = "Total branch length inserted", fill = "Sum branch length (myr)") +
  theme(legend.position = "bottom", panel.background = element_rect(fill = "transparent"),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "mm"),
        legend.title = element_text(family = "Times", color = "black", face = "bold", size = 12),
        legend.text = element_text(family = "Times", color = "black", size = 10), 
        axis.text = element_blank(), panel.border = element_blank(), axis.ticks = element_blank(),
        plot.subtitle = element_text(family = "Arial", 
                                     color = "black",
                                     size = 11, 
                                     hjust = 0.5, 
                                     margin = margin(b = 6))
  ) 

ggsave(filename = here("output", "deficit_marine.png"), 
       plot = map_PD_deficit_wrld, 
       dpi = 300)
ggsave(filename = here("output", "deficit_n_spp_marine.png"), 
       plot = map_n_insertion_wrld, 
       dpi = 300)
ggsave(filename = here("output", "deficit_pd_insert_marine.png"), 
       plot = map_PD_isert_wrld, 
       dpi = 300)

