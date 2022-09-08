devtools::install_github("GabrielNakamura/FishPhyloMaker", ref = "main", build_vignettes = TRUE)
library(FishPhyloMaker)
library(rfishbase)

list<-species(fields="Species")
list$Species
med<-species(list$Species, fields = species_fields$habitat)
med

bet<-ecosystem("Oreochromis niloticus")

table<-FishTaxaMaker(list$Species)
table$All_info_fishbase$Class=="Actinopterygii"

final<-table$Taxon_data_FishPhyloMaker[table$All_info_fishbase$Class=="Actinopterygii",]
nrow(final)

final[which(final$o == "Scorpaeniformes"), "o"] <- "Perciformes"
final[which(final$f == "Cichlidae"), "o"] <- "Cichliformes"
final[which(final$f == "Gobiidae"), "o"] <- "Gobiiformes"
final[which(final$f == "Eleotridae"), "o"] <- "Gobiiformes"
final[which(final$f == "Thalasseleotrididae"), "o"] <- "Gobiiformes"
final[which(final$f == "Rhyacichthyidae"), "o"] <- "Gobiiformes"
final[which(final$f == "Odontobutidae"), "o"] <- "Gobiiformes"

final[which(final$f == "Kurtidae"), "o"] <- "Kurtiformes"
final[which(final$f == "Apogonidae"), "o"] <- "Kurtiformes"

final[which(final$f == "Trichonotidae"), "o"] <- "Incertae sedis in Eupercaria"

final[which(final$f == "Pholidichthyidae"), "o"] <- "Pholidichthyiformes"

final[which(final$f == "Polycentridae"), "o"] <- "Incertae sedis in Ovalentaria"

final[which(final$f == "Labridae"), "o"] <- "Labriformes"

final[which(final$f == "Sparidae"), "o"] <- "Spariformes"
final[which(final$f == "Nemipteridae"), "o"] <- "Spariformes"
final[which(final$f == "Lethrinidae"), "o"] <- "Spariformes"

final[which(final$f == "Sciaenidae"), "o"] <- "Incertae sedis in Eupercaria"
final[which(final$f == "Malacanthidae"), "o"] <- "Incertae sedis in Eupercaria"
final[which(final$f == "Hapalogenyidae"), "o"] <- "Incertae sedis in Eupercaria"
final[which(final$f == "Pomacanthidae"), "o"] <- "Incertae sedis in Eupercaria"
final[which(final$f == "Haemulidae"), "o"] <- "Incertae sedis in Eupercaria"
final[which(final$f == "Lutjanidae"), "o"] <- "Incertae sedis in Eupercaria"
final[which(final$f == "Monodactylidae"), "o"] <- "Incertae sedis in Eupercaria"
final[which(final$f == "Emmelichthyidae"), "o"] <- "Incertae sedis in Eupercaria"
final[which(final$f == "Dinopercidae"), "o"] <- "Incertae sedis in Eupercaria"
final[which(final$f == "Sillaginidae"), "o"] <- "Incertae sedis in Eupercaria"
final[which(final$f == "Moronidae"), "o"] <- "Incertae sedis in Eupercaria"
final[which(final$f == "Centrogenyidae"), "o"] <- "Incertae sedis in Eupercaria"
final[which(final$f == "Gerreidae"), "o"] <- "Incertae sedis in Eupercaria"
final[which(final$f == "Dinolestidae"), "o"] <- "Incertae sedis in Eupercaria"
final[which(final$f == "Bathyclupeidae"), "o"] <- "Incertae sedis in Eupercaria"
final[which(final$f == "Symphysanodontidae"), "o"] <- "Incertae sedis in Eupercaria"

final[which(final$f == "Lobotidae"), "o"] <- "Lobotiformes"

final[which(final$f == "Chaetodontidae"), "o"] <- "Chaetodontiformes"
final[which(final$f == "Leiognathidae"), "o"] <- "Chaetodontiformes"

final[which(final$f == "Pinguipedidae"), "o"] <- "Uranoscopiformes"
final[which(final$f == "Uranoscopidae"), "o"] <- "Uranoscopiformes"

final[final$s == "Siniperca_chuatsi","f"] <- "Sinipercidae"
final[final$s == "Siniperca_fortis","f"] <- "Sinipercidae"
final[final$s == "Siniperca_knerii","f"] <- "Sinipercidae"
final[final$s == "Siniperca_liuzhouensis","f"] <- "Sinipercidae"
final[final$s == "Siniperca_obscura","f"] <- "Sinipercidae"
final[final$s == "Siniperca_roulei","f"] <- "Sinipercidae"
final[final$s == "Siniperca_scherzeri","f"] <- "Sinipercidae"
final[final$s == "Siniperca_undulata","f"] <- "Sinipercidae"
final[final$s == "Siniperca_vietnamensis","f"] <- "Sinipercidae"

final[which(final$f == "Terapontidae"), "o"] <- "Centrarchiformes"
final[which(final$f == "Kuhliidae"), "o"] <- "Centrarchiformes"
final[which(final$f == "Kyphosidae"), "o"] <- "Centrarchiformes"
final[which(final$f == "Oplegnathidae"), "o"] <- "Centrarchiformes"
final[which(final$f == "Elassomatidae"), "o"] <- "Centrarchiformes"
final[which(final$f == "Centrarchidae"), "o"] <- "Centrarchiformes"
final[which(final$f == "Percichthyidae"), "o"] <- "Centrarchiformes"
final[which(final$f == "Cirrhitidae"), "o"] <- "Centrarchiformes"
final[which(final$f == "Cheilodactylidae"), "o"] <- "Centrarchiformes"
final[which(final$f == "Enoplosidae"), "o"] <- "Centrarchiformes"

final[which(final$f == "Pentacerotidae"), "o"] <- "Pempheriformes"
final[which(final$f == "Banjosidae"), "o"] <- "Pempheriformes"
final[which(final$f == "Lateolabracidae"), "o"] <- "Pempheriformes"
final[which(final$f == "Polyprionidae"), "o"] <- "Pempheriformes"
final[which(final$f == "Howellidae"), "o"] <- "Pempheriformes"
final[which(final$f == "Acropomatidae"), "o"] <- "Pempheriformes"
final[which(final$f == "Epigonidae"), "o"] <- "Pempheriformes"
final[which(final$f == "Percophidae"), "o"] <- "Pempheriformes"
final[which(final$f == "Creediidae"), "o"] <- "Pempheriformes"

final[which(final$f == "Scombropidae"), "o"] <- "Scombriformes"

write.table(final,"taxa_table.txt")

final<-read.table("taxa_table.txt",header=T,row.names=1)

final[which(final$f=="Callichthyidae"),]

memory.limit(9999999999)

teste<-FishPhyloMaker(final,insert.base.node = TRUE)
teste
warnings()

FishPhyloMaker
filter_rank

spp_df <- read.table(here::here("taxa_table.txt"), header = TRUE)
res_phylo <- 
  FishPhyloMaker(data = spp_df, 
               insert.base.node = TRUE,
               return.insertions = TRUE,
               progress.bar = TRUE)



med <- species(sub("_", " ", spp_df$s), fields = species_fields$habitat)

marine_spp <- spp_df[which(med$Fresh | med$Brack == 1), ]
res_phylo_marine <- 
  FishPhyloMaker(data = marine_spp, 
                 insert.base.node = TRUE,
                 return.insertions = TRUE,
                 progress.bar = TRUE)
saveRDS(object = res_phylo_marine, file = here::here("phylo_marine.rds"))
table(res_phylo_marine$Insertions_data$insertions)["Order_insertion"]/sum(table(res_phylo_marine$Insertions_data$insertions))
›