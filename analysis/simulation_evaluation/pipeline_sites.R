################################################################################
# pipeline_sites.R
#
# Site-level candidate mutation detection for RMutaPhy.
#
# This script is called by main_script.R for each (scenario × n_tips) setting,
# after pipeline_simulation_trees.R has produced and saved tree-level outputs.
#
# For each simulated tree replicate i:
#   - retrieve significant nodes detected by MutaPhy under H1
#     (permutation-based, corrected p-values),
#   - run get_site_candidates() on the branch(es) leading to these nodes using
#     ancestral state reconstruction,
#   - store the candidate sites by node.
#
# Required inputs (loaded from params$output_dir):
#   - saves/tree_outputs_H0_H1_<n_tips>seqs.RData
#       * provides tree_outputs[[i]]$mutaphy_h1$positifs$permutation_nodes_corrected
#   - saves/01-simulation_trees_H1_<n_tips>seqs.RData
#       * provides list_tree (phylogenies) and results_tree_list (simulated sequences)
#
# Main output:
#   - saves/site_outputs_H0_H1_<n_tips>seqs.RData
#       * site_output: list of length n_trees, each element is a named list
#         candidates_by_node (empty if no significant node was detected).
#
################################################################################

library(rlist)

# ---------------------------------------------------------------------------
# 1) Load tree-level outputs (MutaPhy results per replicate tree)
# ---------------------------------------------------------------------------

load(here::here(sprintf("%s/saves/tree_outputs_H0_H1_%dseqs.RData", # nolint
                        params$output_dir, params$n_tips)))

# ---------------------------------------------------------------------------
# 2) Load simulated trees and sequences under H1
# (used as inputs for ancestral reconstruction and site detection)
# ---------------------------------------------------------------------------

load(here::here(sprintf("%s/saves/01-simulation_trees_%s_%dseqs.RData",
                        params$output_dir, "H1", params$n_tips)))

# ---------------------------------------------------------------------------
# 3) For each replicate tree, extract significant nodes and detect candidate sites
# ---------------------------------------------------------------------------

site_output <- vector("list", length(tree_outputs))

for (i in seq_along(tree_outputs)) {

  # Nodes detected as significant by MutaPhy under H1 (corrected permutation test)
  nodes_corr <- tree_outputs[[i]][["mutaphy_h1"]][["positifs"]][["permutation_nodes_corrected"]]
  nodes_corr <- unique(as.character(na.omit(nodes_corr)))

  # If no significant node, store an empty result and continue
  if (length(nodes_corr) == 0) { site_output[[i]] <- list(); next }

  # Inputs for site-level analysis: tree and simulated sequences for replicate i
  tree_i <- list_tree[[i]]
  seqs_i <- results_tree_list[[i]][["simulation"]][["sequences"]]

  # Identify candidate mutations on branches leading to the detected node(s)
  res <- get_site_candidates(
    nodes     = nodes_corr,
    tree      = tree_i,
    sequences = seqs_i,
    threshold = 0.3
  )
  site_output[[i]] <- res$candidates_by_node
}

# ---------------------------------------------------------------------------
# 4) Save site-level outputs for reproducibility
# ---------------------------------------------------------------------------

# Note: site-level candidates are computed based on H1 replicates and MutaPhy-H1 significant
# nodes; the output filename keeps the H0/H1 convention for consistency with tree_outputs.

save_path <- here::here(sprintf("%s/saves/site_outputs_H0_H1_%dseqs.RData", # nolint
                                params$output_dir, params$n_tips))
if (!dir.exists(dirname(save_path))) {
  dir.create(dirname(save_path), recursive = TRUE)
}
save(site_output,
     file = save_path)
