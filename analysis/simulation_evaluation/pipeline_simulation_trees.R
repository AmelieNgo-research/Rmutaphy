################################################################################
# pipeline_simulation_trees.R
#
# Tree-based simulation and evaluation pipeline for RMutaPhy.
#
# This script is called by main_script.R for each scenario defined in params.yml
# and for each value of n_tips. It performs the following steps:
#   1) Simulate phylogenetic trees under H0 and H1 (including phenotype labels).
#   2) Save subtree-level information for downstream analyses.
#   3) Initialize result files for multiple alpha thresholds.
#   4) For each simulated tree replicate (1..n_trees), run the tree-level
#      benchmarking workflow (MutaPhy under H0 and H1), with robust error
#      handling, and store outputs.
#   5) Save all per-tree outputs to an .RData file for reproducibility.
#
# Required objects in the environment:
#   - params: list of scenario parameters (from params.yml)
#   - Functions sourced from:
#       * analysis/simulation_evaluation/functions.R
#       * R/functions.R
#
# Main outputs (written under params$output_dir):
#   - subtree summaries produced by simulation_save_subtrees()
#   - result placeholders created by create_save_file()
#   - per-tree results: saves/tree_outputs_H0_H1_<n_tips>seqs.RData
#
################################################################################

# ---------------------------------------------------------------------------
# 1) Simulate trees under H0 and H1 for the current scenario
# ---------------------------------------------------------------------------

results_h0 <- generate_simulation_trees(params = params,
                                        score = params$score_h0,
                                        hypothesis = params$null_hypothesis,
                                        Mtot_prop = params$Mtot_prop,
                                        output_dir = params$output_dir,
                                        trait1 = params$trait1,
                                        trait0 = params$trait0)

results_h1 <- generate_simulation_trees(params = params,
                                        score = params$score_h1,
                                        hypothesis = params$alternative_hypothesis,
                                        Mtot_prop = params$Mtot_prop,
                                        output_dir = params$output_dir,
                                        trait1 = params$trait1,
                                        trait0 = params$trait0)

# ---------------------------------------------------------------------------
# 2) Save subtree-level information for downstream evaluation
# ---------------------------------------------------------------------------

simulation_save_subtrees(results_tree = results_h0,
                         hypothesis = params$null_hypothesis,
                         n_tips = params$n_tips,
                         output_dir = params$output_dir)

simulation_save_subtrees(results_tree = results_h1,
                         hypothesis = params$alternative_hypothesis,
                         n_tips = params$n_tips,
                         output_dir = params$output_dir)

# ---------------------------------------------------------------------------
# 3) Initialize output files for each hypothesis and alpha threshold
# ---------------------------------------------------------------------------

create_save_file(params$output_dir, params$null_hypothesis, params$n_tips, params$alpha[1], params$n_trees)
create_save_file(params$output_dir, params$null_hypothesis, params$n_tips, params$alpha[2], params$n_trees)
create_save_file(params$output_dir, params$alternative_hypothesis, params$n_tips, params$alpha[1], params$n_trees)
create_save_file(params$output_dir, params$alternative_hypothesis, params$n_tips, params$alpha[2], params$n_trees)

# ---------------------------------------------------------------------------
# 4) Run benchmarking for each tree replicate (robust to per-tree failures)
# ---------------------------------------------------------------------------

tree_outputs <- vector("list", params$n_trees)
for (tree_number in seq_len(params$n_trees)) {
  cat("Tree", tree_number, "/", params$n_trees, "\n")
  tree_outputs[[tree_number]] <- tryCatch({
    tree_result <- tree_runner(tree_number, results_h0, results_h1, params)
    list(
      mutaphy_h0  = tree_result$mutaphy_h0,
      mutaphy_h1  = tree_result$mutaphy_h1,
      error       = NULL
    )
  }, error = function(e) {
    message("[Tree error ", tree_number, "] ", conditionMessage(e))
    list(
      mutaphy_h0  = NULL,
      mutaphy_h1  = NULL,
      error       = conditionMessage(e)
    )
  })
}

# ---------------------------------------------------------------------------
# 5) Save all per-tree outputs for reproducibility
# ---------------------------------------------------------------------------

save_path <- here::here(sprintf("%s/saves/tree_outputs_H0_H1_%dseqs.RData", # nolint
                                params$output_dir, params$n_tips))
if (!dir.exists(dirname(save_path))) {
  dir.create(dirname(save_path), recursive = TRUE)
}
save(tree_outputs,
     file = save_path)
