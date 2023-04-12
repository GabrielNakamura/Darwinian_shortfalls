
# checkin monofiletic groups ----------------------------------------------

data_new_fam <- data_new[data_new$species %in% sp_tree$tip.label, ]
f <- unique(data_new_fam$family)
# f <- f[1:10]
list_spp_f <- 
  lapply(f, function(x){
  spp_f <- 
    subset(data_new_fam, family == x)$species
})
names(list_spp_f) <- f

mrca_n <- lapply(list_spp_f, function(x) findMRCA(sp_tree, tips = x))
sing_fam <- names(unlist(lapply(mrca_n, function(x) which(is.null(x) == TRUE))))
list_extract <- list_spp_f[-match(names(unlist(lapply(mrca_n, function(x) which(is.null(x) == TRUE)))), names(list_spp_f))]

mono <- 
lapply(list_extract, function(x){
  s <- extract.clade(sp_tree, findMRCA(sp_tree, tips = x))$tip.label
  d <- data_new_fam[match(s, data_new_fam$species), ]
  unique(d$family)
})



unlist(lapply(mono, function(x) which(length(x) > 1)))
fams_mono <- sort(unlist(lapply(mono, function(x) length(x))))

df_fams_mono <- data.frame(family = names(fams_mono), n.family = fams_mono)
saveRDS(df_fams_mono, file = here::here("output", "mofiletic_accessment.rds"))
