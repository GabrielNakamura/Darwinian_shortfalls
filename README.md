
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Darwinian Shortfalls

This repository contains all data, scripts and functions to the
manuscript “Darwinian Shortfall in Fishes”

# Repository structure

This repository contains the full data to reproduce the analysis and
figures present in the manuscript “Darwinian Shortfall in Fishes”

## Folders

-   data: contains all data needed to perform the analysis

-   doc: .doc documents containing the manuscript describing the package

-   output: contains all results, including figures and .rds files
    generated with archives contained in R folder that starts with
    letters D and S

-   R: contains all .R scripts. Archives that starts with letter D and S
    are designed to, respectivelly, run analysis and generate figures
    for the results of these analyses.

# Downloading the repository

The user can download this repo to a local folder in your computer or
clone it:

## downloading all files

`download.file(url = "https://github.com/GabrielNakamura/MS_FishPhyloMaker/archive/main.zip", destfile = "MS_DarwinianShortfallFish.zip")`

to unzip the .zip file in your computer type

`unzip(zipfile = "MS_DarwinianShortfallFish.zip")`

# Tasks

-   Use a single phylogeny
    -   [ ] Modify FishPhyloMaker or use rtrees
-   Update figures
    -   [x] Absolute Barplot for Orders
    -   [ ] Stacked barplot for Orders
    -   [x] Phylogenies and insertions for Marine and Fresh
    -   [ ] Single Phylogeny
-   Metrics
    -   [x] Shorfalls using pseudo posterior distributions
    -   [ ] Edge for all data
    -   [x] Script for EDGE
    -   [x] Request API from IUCN
    -   [ ] Possibility to use data from FB
-   Possible improvements
    -   [ ] Use the availability of molecular data check [Rentrez
        package](https://cran.r-project.org/web/packages/rentrez/vignettes/rentrez_tutorial.html)
        and [Rudbeck et al
        paper](https://onlinelibrary.wiley.com/doi/full/10.1111/ecog.06142)
    -   [ ] Project Darwinian shortfall in space using meaningful scale
        (watershed for freshwater and realms for marine?)
