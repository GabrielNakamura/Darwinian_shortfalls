
# prunning the occ table from gbif ----------------------------------------

# The original data was first prunned using the terminal to overcome memory limitation in R

#  cut -d$'\t' -f8,10,22,23,31,32,33,36 gbif_occ_allActino.csv > Fish_pruned.csv

d <- data.table::fread(here::here("data", "processed", "Fish_pruned.csv")) 
d <- d[, c("species", "decimalLatitude", "decimalLongitude")] # keep only species and coordinates
d <- na.omit(d) # removing all NAs
d <- unique(d) # removing duplicates
names(d) <- c("species", "lat", "lon")
d <- d[, c(1, 3, 2)] # reordering columns 


# saving processed data ---------------------------------------------------

write.csv(d, here("data", "processed", "filtered_occ.csv"), row.names = FALSE) # saving filtered occurrence data
