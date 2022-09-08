tree <- phy_marine
runs <- 3
parallel <- 4
bifurcatr_2<- 
  function (tree, runs = 1, parallel = NULL) 
  {
    resolves <- Ntip(tree) - Nnode(tree) - 1
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
        resolves <- Ntip(tree) - Nnode(tree) - 1
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
    }
    if (newClusters) {
      parallel::stopCluster(parallel)
    }
    
    return(trees)
  }