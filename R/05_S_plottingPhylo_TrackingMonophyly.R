
# ploting full phylogenetic tree for fishes -------------------------------

library(tidytree)
library(ape)
library(phytools)
library(ggplot2)
library(ggtree)

# read data
data <- read.table(here::here("data", "taxa_table.txt"), header = T) # data from fishbase containing all valid names of fish species
sp_tree <- readRDS(here::here("output/order_tree.rds")) # phy for all species 
graft_status <- readRDS(here::here("output/graft_table.rds")) # insertion information

genus <- unlist(lapply(strsplit(x = data$s, split = "_"), function(x) x[[1]]))
data$genus <- genus

data.new <- data.frame(species = data$s, genus = data$genus, family = data$f, order = data$o)

# finding monophyletic families (not run) ---------------------------------------------

n.fam <- unique(data.new$family)

n.Monophyletic <- list()

pb <- txtProgressBar(min = 0,   
                     max = length(n.fam), 
                     style = 3, 
                     width = 50,
                     char = "=")


for(i in 1:length(n.fam)){
  test <- subset(data.new, family == n.fam[i])
  if(sum(is.na(match(test$species, sp_tree$tip.label))) == nrow(test)){
    n.Monophyletic[[i]] <- "Family not inserted"
    names(n.Monophyletic)[i] <- n.fam[i]
  } else {
    if(sum(is.na(match(test$species, sp_tree$tip.label))) > 0){
      test <- test[-which(is.na(match(test$species, sp_tree$tip.label))),]
    }
    
    if(nrow(test) == 1){
      n.Monophyletic[[i]] <- "Singleton"
      names(n.Monophyletic)[i] <- n.fam[i]
    } else {
      
      sub.tree <- extract.clade(sp_tree, findMRCA(sp_tree, tips = test$species))
      x_tree <- data.new[match(sub.tree$tip.label, data.new$species), ]
      
      if(length(unique(x_tree$family)) > 1){
        super.clade <- x_tree[which(x_tree$family != n.fam[i]), ]
        n.Monophyletic[[i]] <- unique(super.clade$family)
        names(n.Monophyletic)[i] <- n.fam[i]
      } else {
        n.Monophyletic[[i]] <- "Monophyletic"
        names(n.Monophyletic)[i] <- n.fam[i]
      }
    }
  }
  setTxtProgressBar(pb, i)
}
saveRDS(n.Monophyletic, here::here("output/Table_info_mono.rds"))
# end ---------------------------------------------------------------------

# reading the file with information about the monophyletic families
Fam.mono <- readRDS(here::here("output/Table_info_mono.rds"))

# transforming into a dataframe
df.info <- plyr::ldply(Fam.mono, rbind)
colnames(df.info)[1] <- c("family")

df.info$is.mono <- as.factor(ifelse(df.info$`1` == "Monophyletic", "Monophyletic", "No"))
df.info$is.singeton <- as.factor(ifelse(df.info$`1` == "Singleton", "Singleton", "No"))
df.info$not.insert <- as.factor(ifelse(df.info$`1` == "Family not inserted", "Not_inserted", "No"))
df.info[which(df.info$`1` == "Monophyletic"), 2] <- NA
df.info[which(df.info$`1` == "Singleton"), 2] <- NA
df.info[which(df.info$`1` == "Family not inserted"), 2] <- NA
df.info$no.mono <- as.factor(ifelse(!is.na(df.info$`1`) == TRUE, "Non-monophyletic", "No"))

df.info$Sum <- rowSums(!is.na(df.info[,2:486])) # number of families inside a non-mono family

df.mono <- as_tibble(df.info[, c(1, 487:491)])

# joint information ------------------------------------------------------

data.new <- as_tibble(data.new)
colnames(data.new)[1] <- "label"
colnames(graft_status)[1] <- "label"

# not run
# p <- ggtree(sp_tree)
# saveRDS(p, here::here("output/ggtree_all.rds"))

p <- readRDS(here::here("output/ggtree_all.rds"))


# let's put all the information in a single dataframe
p$data <- left_join(p$data, data.new, by = "label")
p$data <- left_join(p$data, graft_status, by = "label")
p$data <- left_join(p$data, df.mono, by = "family")
View(p$data)

# Tracking the species of non-monophyletic families -------------
df.no_mono <- subset(p$data, no.mono == "Non-monophyletic")

OTU.list <- list()

for (i in 1:length(unique(df.no_mono$family))) {
  OTU.list[[i]] <- subset(p$data, family == unique(df.no_mono$family)[i])$label
}
names(OTU.list) <- unique(df.no_mono$family) # setting names

# The remaining species belong to monophyletic families
OTU.list[[length(unique(df.no_mono$family))+1]] <- subset(p$data, no.mono == "No")$label
names(OTU.list)[length(unique(df.no_mono$family))+1] <- "Monophyletic"

OTU.list

# Families with more than 300 species
names(tail(sort(lengths(OTU.list)), 10))

d1 <- as.data.frame(p$data)
d1$Type <- NA

d1[which(d1$is.singeton == "Singleton"), "Type"] <- "Singleton"
d1[which(d1$is.mono == "Monophyletic"), "Type"] <- "Monophyletic"
d1[which(d1$no.mono == "Non-monophyletic"), "Type"] <- "Non-monophyletic"
d1[which(d1$family == "Zoarcidae"), ]

### tentar dar match entre as 10 familias mais especiosas


d2 <- as.data.frame(cbind(sp_tree$tip.label, 
                          d1[match(sp_tree$tip.label, d1$label), "Type"],
                          d1[match(sp_tree$tip.label, d1$label), "family"]))

t.list <- split(d2$V1, d2$V2)
t.list
names(t.list)

p.group <- groupOTU(p, t.list, 'Species') + aes(color = Species) +
  theme(legend.position = "none") + scale_color_viridis_d()
p1 <- p.group + theme(legend.position = "right") +
  layout_circular()

fam.mono <- unique(subset(d2, V2 == "Monophyletic")$V3)
n.nodes <- as.numeric()

for (i in 1:length(fam.mono)) {
  n.nodes[i] <- min(subset(d1, family == fam.mono[i])$parent)
}

for (i in 1:length(n.nodes)) {
  #i=1
  p1 <- p1 %>% collapse(node = n.nodes[i]) 
}
quartz()
p1

names(OTU.list)

