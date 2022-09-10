library(rgbif)
?name_suggest
tax_keys <- name_suggest(q = "Actinopterygii", rank = "Class")
lapply(tax_keys$data$key, "occ_count")
tax_keys <- tax_keys$data$key[1]
occ_count(tax_keys)

dat_ne <- occ_search(taxonKey = tax_key, return = "data", hasCoordinate = T, 
                     geometry = study_a, limit = 50000)