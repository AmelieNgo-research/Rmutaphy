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

simulation_save_subtrees(results_tree = results_h0,
                         hypothesis = params$null_hypothesis,
                         n_tips = params$n_tips,
                         output_dir = params$output_dir)

simulation_save_subtrees(results_tree = results_h1,
                         hypothesis = params$alternative_hypothesis,
                         n_tips = params$n_tips,
                         output_dir = params$output_dir)

create_save_file(params$output_dir, params$null_hypothesis, params$n_tips, params$alpha[1], params$n_trees)
create_save_file(params$output_dir, params$null_hypothesis, params$n_tips, params$alpha[2], params$n_trees)
create_save_file(params$output_dir, params$alternative_hypothesis, params$n_tips, params$alpha[1], params$n_trees)
create_save_file(params$output_dir, params$alternative_hypothesis, params$n_tips, params$alpha[2], params$n_trees)

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

save_path <- here::here(sprintf("%s/saves/tree_outputs_H0_H1_%dseqs.RData", # nolint
                                params$output_dir, params$n_tips))
if (!dir.exists(dirname(save_path))) {
  dir.create(dirname(save_path), recursive = TRUE)
}
save(tree_outputs,
     file = save_path)
