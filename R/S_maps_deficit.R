
# data,  libs and functions -----------------------------------------------

library(here)
library(sf)
library(rnaturalearth)
library(ggplot2)
library(dplyr)
library(magrittr)
library(fuzzyjoin)

# reading data ------------------------------------------------------------

ed_all <- read.table(here("data", "aed.txt"))
df_ed_all <- data.frame(species = rownames(ed_all), 
                        vulnerability = ed_all$vuln_final.Vulnerability)
insertion_spp <- readRDS(here::here("output", "data_insertion.rds"))
shortfall_countries <- read.csv(here("data", "processed", "countries_insertion.csv"))
colnames(shortfall_countries)[1] <- "name_long"
countries <- ne_countries()
sf_countries <- st_as_sf(countries)
sf_countries <- st_transform(sf_countries, 
                             crs = "+proj=robin")


# merging insertion and aed -----------------------------------------------


sf_insertion <- 
sf_countries %>% 
  left_join(shortfall_countries, by = "name_long")

# employing fuzzy matching 
sf_insertion_fuzzy <- stringdist_join(x = sf_insertion, y = shortfall_countries, by = "name_long") 


# manual correction -------------------------------------------------------

names_sf <- sf_insertion$name_long[which(is.na(match(sf_insertion$name_long, shortfall_countries$name_long)) == TRUE)]

shortfall_countries[which(shortfall_countries$name_long == "Bosnia Herzegov."), "name_long"] <- sort(names_sf)[1]
shortfall_countries[which(shortfall_countries$name_long == "Brunei Darsm "), "name_long"] <- sort(names_sf)[2]
shortfall_countries[which(shortfall_countries$name_long == "Central African Republic"), "name_long"] <- sort(names_sf)[3]
shortfall_countries[which(shortfall_countries$name_long == "Cote d'Ivoire"), "name_long"] <- sort(names_sf)[4]
shortfall_countries[which(shortfall_countries$name_long == "Czechia"), "name_long"] <- sort(names_sf)[5]
shortfall_countries[which(shortfall_countries$name_long == "Korea (North)"), "name_long"] <- sort(names_sf)[6]
shortfall_countries[which(shortfall_countries$name_long == "Congo"), "name_long"] <- sort(names_sf)[7]
shortfall_countries[which(shortfall_countries$name_long == "Dominican Rp"), "name_long"] <- sort(names_sf)[8]
# shortfall_countries[which(shortfall_countries$name_long == "Dominican Rp"), "name_long"] <- sort(names_sf)[9]
shortfall_countries[which(shortfall_countries$name_long == "Falkland Is."), "name_long"] <- sort(names_sf)[10]
# shortfall_countries[which(shortfall_countries$name_long == "Falkland Is."), "name_long"] <- sort(names_sf)[11]
# shortfall_countries[which(shortfall_countries$name_long == "Falkland Is."), "name_long"] <- sort(names_sf)[12]
shortfall_countries[which(shortfall_countries$name_long == "Laos"), "name_long"] <- sort(names_sf)[13]
shortfall_countries[which(shortfall_countries$name_long == "Laos"), "name_long"] <- sort(names_sf)[14]
shortfall_countries[which(shortfall_countries$name_long == "Cyprus"), "name_long"] <- sort(names_sf)[15]
# shortfall_countries[which(shortfall_countries$name_long == "Cyprus"), "name_long"] <- sort(names_sf)[16]
shortfall_countries[which(shortfall_countries$name_long == "Congo Dem Rp"), "name_long"] <- sort(names_sf)[17]
shortfall_countries[which(shortfall_countries$name_long == "Korea (South)"), "name_long"] <- sort(names_sf)[18]
shortfall_countries[which(shortfall_countries$name_long == "Russia"), "name_long"] <- sort(names_sf)[19]
# shortfall_countries[which(shortfall_countries$name_long == "Russia"), "name_long"] <- sort(names_sf)[20]
# shortfall_countries[which(shortfall_countries$name_long == "Russia"), "name_long"] <- sort(names_sf)[21]
shortfall_countries[which(shortfall_countries$name_long == "Gambia"), "name_long"] <- sort(names_sf)[22]
shortfall_countries[which(shortfall_countries$name_long == "Trinidad Tobago"), "name_long"] <- sort(names_sf)[23]
shortfall_countries[which(shortfall_countries$name_long == "Trinidad Tobago"), "name_long"] <- sort(names_sf)[24]
shortfall_countries[which(shortfall_countries$name_long == "UK"), "name_long"] <- sort(names_sf)[25]
shortfall_countries[which(shortfall_countries$name_long == "USA"), "name_long"] <- sort(names_sf)[26]
shortfall_countries[which(shortfall_countries$name_long == "Viet Nam"), "name_long"] <- sort(names_sf)[27]
shortfall_countries[which(shortfall_countries$name_long == "West Sahara"), "name_long"] <- sort(names_sf)[28]
shortfall_countries[which(shortfall_countries$name_long == "West Sahara"), "name_long"] <- sort(names_sf)[29]

sf_insertion <- 
  sf_countries %>% 
  left_join(shortfall_countries, by = "name_long")



# plotting country deficits ----------------------------------

quartz()
map_PD_deficit_wrld <- ggplot() +
  geom_sf(data = sf_insertion, aes(geometry = geometry, 
                                   fill = PD_deficit),
          color = "transparent", size = 0.1, na.rm = T) +
  rcartocolor::scale_fill_carto_c(palette = "Teal", 
                                  direction = 1, 
                                  limits = c(0, 1),  ## max percent overall
                                  breaks = seq(0, 1, by = .2),
                                  labels = glue::glue("{seq(0, 1, by = 0.2)}")) +
  labs(subtitle = "A)", fill = expression(PD[inserted]/PD[total])) +
  guides(fill = guide_colorbar(barheight = unit(30, units = "mm"),  
                               barwidth =  unit(2, units = "mm"),
                               direction = "vertical",
                               ticks.colour = "grey20"
  )) +
  theme(legend.position = "right", panel.background = element_rect(fill = "transparent"),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "mm"),
        legend.title = element_text(family = "Times", color = "black", face = "bold", size = 10),
        legend.text = element_text(family = "Times", color = "black", size = 10), 
        axis.text = element_blank(), panel.border = element_blank(), axis.ticks = element_blank()
  ) 

map_PD_deficit_genus <- ggplot() +
  geom_sf(data = sf_insertion, aes(geometry = geometry, 
                                   fill = genus_level),
          color = "transparent", size = 0.1, na.rm = T) +
  rcartocolor::scale_fill_carto_c(palette = "Teal", 
                                  direction = 1, 
                                  limits = c(0, 1930),  ## max percent overall
                                  breaks = seq(0, 1930, by = 400),
                                  labels = glue::glue("{seq(0, 1930, by = 400)}")) +
  labs(subtitle = "B)", fill = "") +
  guides(fill = guide_colorbar(barheight = unit(12, units = "mm"),  
                               barwidth =  unit(1.5, units = "mm"),
                               direction = "vertical",
                               ticks.colour = "grey20"
  )) +
  theme(legend.position = "right", panel.background = element_rect(fill = "transparent"),
        plot.margin = unit(c(0, 0, 0, 0.), "mm"),
        legend.title = element_text(family = "Times", color = "black", size = 12),
        legend.text = element_text(family = "Times", color = "black", size = 6), 
        axis.text = element_blank(), panel.border = element_blank(), axis.ticks = element_blank()
  ) 

map_PD_deficit_family <- ggplot() +
  geom_sf(data = sf_insertion, aes(geometry = geometry, 
                                   fill = family_level),
          color = "transparent", size = 0.1, na.rm = T) +
  rcartocolor::scale_fill_carto_c(palette = "Teal", 
                                  direction = 1, 
                                  limits = c(0, 604),  ## max percent overall
                                  breaks = seq(0, 604, by = 150),
                                  labels = glue::glue("{seq(0, 604, by = 150)}")) +
  labs(subtitle = "C)", fill = "") +
  guides(fill = guide_colorbar(barheight = unit(12, units = "mm"),  
                               barwidth =  unit(1.5, units = "mm"),
                               direction = "vertical",
                               ticks.colour = "grey20"
                               )) +
  theme(legend.position = "right", panel.background = element_rect(fill = "transparent"),
        plot.margin = unit(c(0, 0, 0, 0), "mm"),
        legend.title = element_text(family = "Times", color = "black", size = 12),
        legend.text = element_text(family = "Times", color = "black", size = 6), 
        axis.text = element_blank(), panel.border = element_blank(), axis.ticks = element_blank()
  )

map_PD_deficit_order <- ggplot() +
  geom_sf(data = sf_insertion, aes(geometry = geometry, 
                                   fill = order_level),
          color = "transparent", size = 0.1, na.rm = T) +
  rcartocolor::scale_fill_carto_c(palette = "Teal", 
                                  direction = 1, 
                                  limits = c(0, 82),  ## max percent overall
                                  breaks = seq(0, 82, by = 20),
                                  labels = glue::glue("{seq(0, 82, by = 20)}"))  +
  labs(subtitle = "D)", fill = "") +
  guides(fill = guide_colorbar(barheight = unit(12, units = "mm"),  
                               barwidth =  unit(1.5, units = "mm"),
                               direction = "vertical",
                               ticks.colour = "grey20"
  )) +
  theme(legend.position = "right", panel.background = element_rect(fill = "transparent"),
        plot.margin = unit(c(0, 0, 0, 0), "mm"),
        legend.title = element_text(family = "Times", color = "black", size = 12),
        legend.text = element_text(family = "Times", color = "black", size = 6), 
        axis.text = element_blank(), panel.border = element_blank(), axis.ticks = element_blank()
  )


# crafting Figure ---------------------------------------------------------

plot_deficit <- cowplot::plot_grid(map_PD_deficit_genus, 
                                   map_PD_deficit_family, 
                                   map_PD_deficit_order,
                                   ncol = 3, rel_widths = c(2, 2, 2))

plot_compose_deficit <- 
cowplot::plot_grid(map_PD_deficit_wrld + theme(legend.position = "right"),
                   plot_deficit, ncol = 1,
                   rel_heights = c(2.4, 2))


# saving Figures ----------------------------------------------------------

ggsave(filename = here("output", "images", "deficit_world.pdf"), 
       plot = plot_compose_deficit, device = "pdf",
       dpi = 300, width = 9, height = 5)
ggsave(filename = here("output", "images", "deficit_world_blue.pdf"), 
       plot = plot_compose_deficit, device = "pdf",
       dpi = 300, width = 9, height = 5)
ggsave(filename = here("output", "images", "deficit_world_blue.png"), 
       plot = plot_compose_deficit, device = "png",
       dpi = 300, width = 9, height = 5)
ggsave(filename = here("output", "images", "deficit_world.png"), 
       plot = plot_compose_deficit, 
       dpi = 300, width = 9, height = 5)

