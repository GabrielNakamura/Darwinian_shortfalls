
library(here)
library(FishPhyloMaker)
library(rFishTaxa)
library(phyloregion)
library(rfishbase)
source(here("R", "functions", "function_PD_deficit.R"))
comm_marine <- read.csv(here("data", "processed", "presab_MEOW.csv")) # gbif occurrence records for MEOW
res_phylo_marine <- readRDS(here("output", "phylo_marine.rds")) # phylogeny and insertions from FishPhyloMaker

# processing data  -----------------------------------------

phy_marine <- res_phylo_marine$Phylogeny # phylogeny
insertion_marine <- res_phylo_marine$Insertions_data # insertions
comm_marine <- long2dense(comm_marine) # dense community matrix for MEOW 
names_no_match_fb <- colnames(comm_marine)[which(gsub(" ", "_", colnames(comm_marine)) %in% insertion_marine$s != TRUE)] # occ not in insertion table from FishBase

# searching valid names in Fish Base

df_validation <- data.frame(sp.gbif = gsub(" ", "_", colnames(comm_marine)), sp.valid.fb = NA, sp.valid.cas = NA)
df_validation$not.cas.fb <- NA
df_fb <- FishTaxaMaker(data = gsub(" ", "_", names_no_match_fb), allow.manual.insert = FALSE)
diff_names <- df_fb$All_info_fishbase[which(is.na(match(gsub("_", " ", df_fb$All_info_fishbase$user_spp), df_fb$All_info_fishbase$valid_names)) == TRUE),]
equal_names <- df_fb$All_info_fishbase[which(is.na(match(gsub("_", " ", df_fb$All_info_fishbase$user_spp), df_fb$All_info_fishbase$valid_names)) != TRUE),]
not_found_fb <- diff_names[which(is.na(diff_names$valid_names) == TRUE), ] 
found_valid_fb <- diff_names[which(is.na(diff_names$valid_names) != TRUE), ] 
df_validation[match(found_valid_fb$user_spp, df_validation$sp.gbif), "sp.valid.fb"] <- gsub(" ", "_", found_valid_fb$valid_names)
df_validation[match(equal_names$user_spp, df_validation$sp.gbif), "sp.valid.fb"] <- equal_names$valid_names

# searching in CAS all species 

l_cas <- vector(mode = "list", length = length(names_no_match_fb)) 
pb = txtProgressBar(min = 0, max = length(l_cas), initial = 1, style = 3) 
for(i in 1:length(names_no_match_fb)){
  # i = 1
  tryCatch(
    l_cas[[i]]<- rFishTaxa::search_cas(query = gsub(" ", "_", names_no_match_fb[i]), type = "species"),
    error = function(e){
      paste("not_found", gsub(" ", "_", names_no_match_fb[i]), sep = "_")
    }
  )
  setTxtProgressBar(pb = pb, value = i)
}
df_cas <- do.call(rbind, l_cas)
list_not_cas <- lapply(l_cas, function(x) which(!any(class(x) == "data.frame")))
spp_not_cas <- gsub(" ", "_", names_no_match_fb[which(unlist(lapply(list_not_cas, function(x) length(x) >= 1)) == TRUE)])
df_validation[match(spp_not_cas, df_validation$sp.gbif), "not.cas.fb"] <- TRUE # not in cas neither fb
query_cas <- 
  lapply(strsplit(x = df_cas$query, split = ","), 
         function(x) gsub(" ", "", paste(x[2], x[1], sep = "_")))
query_cas <- unlist(query_cas)
df_cas$query.name <- query_cas
spp_cas <- df_cas$query.name[which(is.na(match(df_cas$query.name, df_validation$sp.gbif)) != TRUE)] # match query cas and gbif
spp_valid_cas <- data.frame(df_cas[match(spp_cas, df_cas$query.name), c("species", "status")])
df_validation[match(spp_cas, df_validation$sp.gbif), "sp.valid.cas"] <- spp_valid_cas$species
df_validation$status.cas <- NA
df_validation[match(spp_cas, df_validation$sp.gbif), "status.cas"] <- spp_valid_cas$status
df_validation$join.fb.cas <- NA
joint.cas.fb <- 
  apply(df_validation, 1, function(x){
    if(is.na(x[2]) & is.na(x[3])){
      paste("not_find_", x[1], sep = "")
    } else{
      name <- x[c("sp.valid.fb", "sp.valid.cas")[which(is.na(x[c("sp.valid.fb", "sp.valid.cas")]) != TRUE)]]
      if(length(unlist(name)) == 2){
        name <- unlist(name)[2]
      }
      name
    }
  })
df_validation[, "join.fb.cas"] <- joint.cas.fb


dup_marine <- names(which(table(df_validation$join.fb.cass) > 1))
dup_df <- df_validation[unlist(lapply(dup_marine, function(x) which(df_validation$join.fb.cass == x))), ]
list_dup <- lapply(dup_marine, function(x) which(df_validation$join.fb.cass == x))
names(list_dup) <- dup_marine
comm_clean <- comm_marine
col_delete <- 
unlist(unique(lapply(list_dup, function(x){
  colnames(comm_marine[, x])
})))
col_add <- 
lapply(list_dup, function(x){
  pres <- ifelse(rowSums(comm_marine[, x]) >= 1, 1, 0) 
})
col_add <- do.call(cbind, col_add)
comm_clean <- comm_clean[, -match(col_delete, colnames(comm_clean))]
comm_clean <- cbind(comm_clean, col_add)


# saving tables -----------------------------------------------------------

write.csv(df_validation, here("data", "processed", "df_validation_marine.csv"))
write.csv(comm_clean, here("data", "processed", "comm_clean_marine.csv"), row.names = TRUE)
