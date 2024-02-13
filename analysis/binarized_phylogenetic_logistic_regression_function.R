
library(ape)
library(phylolm)
library(tidyverse)

run_binarized_phylogenetic_logistic_regression <- function(outcome_var, predictor_var, tree_file_path, data_file_path) {
  # Read mammal tree
  mammaltree <- read.tree(tree_file_path)

  # Read data
  Binary_traits <- read_csv(data_file_path)

  # Combine genus and species names and capitalize first letter
  Binary_traits_combined <- Binary_traits %>%
    unite("species", Genus, species) %>%
    mutate(species = str_to_title(species))

  # Prune tree for species in data
  species_not_in_tree <- setdiff(mammaltree$tip.label, Binary_traits_combined$species)
  pruned.tree <- drop.tip(mammaltree, species_not_in_tree)

  # Prune data for species in tree
  data_pruned <- Binary_traits_combined %>% filter(species %in% pruned.tree$tip.label)

  # Put data into useful form for phylolm
  colnames(data_pruned) <- gsub(" ", "_", colnames(data_pruned))
  data_pruned_rownames <- column_to_rownames(data_pruned, var = "species")

  # Run phylolm and get summary
  model <- phyloglm(as.formula(paste(outcome_var, "~", predictor_var)),
                    data_pruned_rownames, phy = pruned.tree, method = "logistic_MPLE")

  return(summary(model))
}
