polytomies_solver <- function(tree, n.polytomy, nruns){
  for (j in 1:n.polytomy) {
    descendent_counts <- rle(sort(tree$edge[, 1]))
    polytomies <- descendent_counts$values[which(descendent_counts$lengths > 
                                                   2)]
    if (length(polytomies) > 1) 
      target_polytomy <- sample(polytomies, size = 1)
    else target_polytomy <- polytomies
    polytomy_edges <- which(tree$edge[, 1] == target_polytomy)
    target_edges <- sample(polytomy_edges, size = 2)
    new_node <- max(tree$edge) + 1
    tree$edge[target_edges, 1] <- new_node
    new_edge <- c(target_polytomy, new_node)
    tree$edge <- rbind(tree$edge, new_edge)
    new_length <- runif(n = 1, min = 0, max = min(tree$edge.length[target_edges]))
    tree$edge.length <- c(tree$edge.length, new_length)
    tree$edge.length[target_edges] <- tree$edge.length[target_edges] - 
      new_length
    tree$Nnode <- tree$Nnode + 1
  }
  return(tree)
}

#' Solve polytomies in a phylogenetic tree (parallel version)
#'
#' @param tree A newick phylogenetic tree. The tree must be ultrametric
#' @param runs Scalar. number of times to solve the polytomies
#' @param parallel Scalar. number of computer cores used to distribute the runs 
#'
#' @return A phylo or multiPhylo object containing fully solved trees 
#' @export
#'
#' @examples
bifurcatr_parallel <- 
  function (tree, runs = 1, parallel = NULL) 
  {
    resolves <- ape::Ntip(tree) - ape::Nnode(tree) - 1
    newClusters <- FALSE
    if (is.numeric(parallel)) {
      if(!parallel%%1==0){
        stop("\n parallel must an integer")
      }
      n.cluster <- parallel
      parallel <- parallel::makeCluster(parallel, type = "PSOCK")
      newClusters <- TRUE
    }
    if (!inherits(parallel, "cluster")) {
      trees <- vector("list", length = runs)
      for (i in 1:runs) {
        tree <- phy
        resolves <- ape::Ntip(tree) - ape::Nnode(tree) - 1
        tree <- polytomies_solver(tree = tree, n.polytomy = resolves, nruns = 0)
        trees[[i]] <- tree
      }
      if (runs == 1) {
        trees <- trees[[1]]
        class(trees) <- "phylo"
      }
      else {
        class(trees) <- "multiPhylo"
      }
    } else {
      trees <- parallel::parLapply(parallel, seq_len(runs), fun = polytomies_solver,
                                   tree = tree,
                                   n.polytomy = resolves)
      class(trees) <- "multiPhylo"
    }
    if (newClusters) {
      parallel::stopCluster(parallel)
    }
    return(trees)
  }