
PD_deficit <- function(phylo, data, level = "Congeneric_insertion"){
  if(is.null(phylo) == TRUE){
    stop("/n A phylogenetic tree must be provided")
  }
  if(class(phylo) != "phylo"){
    stop("/n A phylo object must be provided")
  }
  names_exclude <- phylo$tip.label[na.omit(match(data[which(data$insertions == "Present_in_Tree"), "s"], 
                                                 phylo$tip.label))]
  
  if(all(!is.na(match(phylo$tip.label, names_exclude))) == TRUE){
    PD_present <- sum(phylo$edge.length)
    PD_level <- 0
    PD_total <- sum(phylo$edge.length)
    Darwinian_deficit <- PD_level/(PD_present + PD_level)
    res <- c(PD_present, PD_level, PD_total, Darwinian_deficit)
    names(res) <- c("PDintree", "PDdeficit", "PDtotal", "Darwinian_deficit")
    return(res)
  } else{
    exclude <- ape::drop.tip(phylo, tip = names_exclude)$tip.label
    phylo_present <- ape::drop.tip(phylo, tip = exclude)
    PD_present <- sum(phylo_present$edge.length)
    n_present <- length(phylo_present$tip.label)
    if(length(level) == 1){
      level_exclude <- ape::drop.tip(phylo, tip = phylo$tip.label[na.omit(match(data[which(data$insertions == level), "s"], 
                                                                                phylo$tip.label))])$tip.label
    }
    if(length(level) > 1){
      level_exclude <- ape::drop.tip(phylo, data[!is.na(match(data$insertions, level)), "s"])$tip.label 
    }
    phylo_level <- ape::drop.tip(phylo, level_exclude)
    n_insertion <- length(phylo_level$tip.label)
    PD_level <- sum(phylo_level$edge.length)
    PD_total <- sum(phylo$edge.length)
    Darwinian_deficit <- PD_level/(PD_present + PD_level)
    res <- c(n_present, n_insertion, PD_present, PD_level, PD_total, Darwinian_deficit)
    names(res) <- c("n_spp_present", "n_spp_insert", "PDintree", "PDdeficit", "PDtotal", "Darwinian_deficit")
    return(res)
  }
}