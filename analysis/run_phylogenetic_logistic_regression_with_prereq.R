# Load necessary libraries
library(phylolm)
library(ape)
library(tidyverse)

run_phylogenetic_logistic_regression_with_prereq <- function(outcome_var, predictor_var, tree_file_path, data_file_path) {


# Safely attempt to get a prerequisite trait, defaulting to a safe value if not found
prerequisite_trait <- ifelse(predictor_var %in% names(prerequisite_traits), prerequisite_traits[[predictor_var]], "")

  # Read mammal tree
  mammaltree <- read.tree(tree_file_path)

  # Read data
  Binary_traits <- read_csv(data_file_path)

  # Combine genus and species names and capitalize first letter
  Binary_traits_combined <- Binary_traits %>%
    unite("species", Genus_names, species_name) %>%
    mutate(species = str_to_title(species))

  # Prune tree for species in data
  species_not_in_tree <- setdiff(mammaltree$tip.label, Binary_traits_combined$species)
  pruned.tree <- drop.tip(mammaltree, species_not_in_tree)

  # Prune data for species in tree
  data_pruned <- Binary_traits_combined %>% filter(species %in% pruned.tree$tip.label)

  # Put data into useful form for phylolm
  colnames(data_pruned) <- gsub(" ", "_", colnames(data_pruned))
  data_pruned_rownames <- column_to_rownames(data_pruned, var = "species")

  # Adjust formula to include prerequisite_trait if specified
  formula_str <- if (!is.null(prerequisite_trait) && prerequisite_trait != "") {
    paste(outcome_var, "~", predictor_var, "+", prerequisite_trait)
  } else {
    paste(outcome_var, "~", predictor_var)
  }

  # Fit the model using phylolm
  model <- phylolm(as.formula(formula_str), data = data_pruned_rownames, phy = pruned.tree, model = "logistic_MPLE")

  return(model)
}
