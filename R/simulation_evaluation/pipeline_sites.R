library(rlist)

load(here::here(sprintf("%s/saves/tree_outputs_H0_H1_%dseqs.RData", # nolint
                        params$output_dir, params$n_tips)))


load(here::here(sprintf("%s/saves/01-simulation_trees_%s_%dseqs.RData",
                        params$output_dir, "H1", params$n_tips)))

site_output <- vector("list", length(tree_outputs))

for (i in seq_along(tree_outputs)) {
  nodes_corr <- tree_outputs[[i]][["mutaphy_h1"]][["positifs"]][["permutation_nodes_corrected"]]
  nodes_corr <- unique(as.character(na.omit(nodes_corr)))
  if (length(nodes_corr) == 0) { site_output[[i]] <- list(); next }

  tree_i <- list_tree[[i]]
  seqs_i <- results_tree_list[[i]][["simulation"]][["sequences"]]

  res <- get_site_candidates(
    nodes     = nodes_corr,
    tree      = tree_i,
    sequences = seqs_i,
    threshold = 0.3
  )
  site_output[[i]] <- res$candidates_by_node
}


save_path <- here::here(sprintf("%s/saves/site_outputs_H0_H1_%dseqs.RData", # nolint
                                params$output_dir, params$n_tips))
if (!dir.exists(dirname(save_path))) {
  dir.create(dirname(save_path), recursive = TRUE)
}
save(site_output,
     file = save_path)
