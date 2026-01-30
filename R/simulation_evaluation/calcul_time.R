#### calcul time
generate_data <- function(n) {
  tree <- rtree(n)
  trait <- sample(c(0,1), n, replace = TRUE)
  names(trait) <- tree$tip.label
  list(tree = tree, trait = trait)
}


benchmark_methods <- function(n, n_simu = 1000) {
  data <- generate_data(n)
  tree <- data$tree
  trait <- data$trait


  trait_char <- as.character(trait)

  # AI
  time_ai <- system.time({
    ai_res <- ai_test(tree, trait, n_simu = n_simu)
  })[3]

  # PS
  time_ps <- system.time({
    ps_res <- ps_test(tree, trait, n_simu = n_simu)
  })[3]

  # MC
  time_mc <- system.time({
    mc_res <- mc_test(tree, trait, trait0 = "0", trait1 = "1", n_simu = n_simu)
  })[3]

  # CRP-Tree
  time_crp <- system.time({
    tips_corresponding <- cbind(
      tree$tip.label,
      ifelse(trait_char == "1", -1, -2)  # 1 = severe, 0 = non-severe
    )

    tree_processed <- CRPTree::process_tree(tree, tip_corresponding = tips_corresponding)
    crptree_res <- CRPTree::one_tree_all_methods(tree_processed)
  })[3]

  # MutaPhy
  time_mutaphy <- system.time({
    mutaphy_res <- mutaphy_test(tree = tree, trait0 = "0",trait1 = "1", n_simu = n_simu)
  })[3]

  data.frame(
    n = n,
    AI = time_ai,
    PS = time_ps,
    MC = time_mc,
    CRP = time_crp,
    MutaPhy = time_mutaphy
  )
}


results_nsimu100 <- do.call(rbind, lapply(c(20, 50, 100, 300), function(n) {
  benchmark_methods(n, n_simu = 100)
}))

results_nsimu1000 <- do.call(rbind, lapply(c(20, 50, 100, 300), function(n) {
  benchmark_methods(n, n_simu = 1000)
}))

results_nsimu10000 <- do.call(rbind, lapply(c(20, 50, 100, 300), function(n) {
  benchmark_methods(n, n_simu = 10000)
}))
