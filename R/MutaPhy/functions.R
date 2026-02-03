### table_info_subtrees.R
table_info_subtrees_obs <- function(tree, trait1) {
  node <- unique(tree$edge[, 1])
  min_node <- min(tree$edge[, 1])
  pheno <- ifelse(tree$edge[, 2] < min_node, tree$tip.label[tree$edge[, 2]], 0)
  binaire <- ifelse(pheno == trait1, 1, 0)
  tree$edge <- cbind(tree$edge, binaire)
  df <- data.frame(node = integer(),
                   info_node = character(),
                   info_direct_child = character(),
                   info_direct_parent = integer(),
                   prop_trait1 = numeric(),
                   n_trait1 = integer(),
                   n_non_trait1 = integer(),
                   n = integer(),
                   stringsAsFactors = FALSE)
  for (i in rev(seq_along(node))) {
    node_i <- as.data.frame(tree$edge[tree$edge[, 1] == node[i], ])
    sum_trait1_leaves <- 0
    sum_n_leaves <- 0
    sum_trait1_nodes <- 0
    sum_n_nodes <- 0
    if (nrow(node_i) == 1) {
      sum_trait1_leaves <- as.numeric(node_i[, 3])
      sum_n_leaves <- 1
    } else {
      leaves <- node_i[node_i[, 2] < min_node, ]
      nodes <- node_i[node_i[, 2] > min_node, ]
      if (nrow(leaves) > 0) {
        sum_trait1_leaves <- sum(leaves[, 3])
        sum_n_leaves <- nrow(leaves)
      }
      if (nrow(nodes) > 0) {
        for (j in seq_len(nrow(nodes))) {
          node_child <- df[df[, 1] == nodes[j, 2], ]
          if (nrow(node_child) > 0) {
            sum_trait1_nodes <- sum_trait1_nodes + as.numeric(node_child[, 6])
            sum_n_nodes <- sum_n_nodes + as.numeric(node_child[, 8])
          }
        }
      }
    }
    total_trait1 <- sum_trait1_leaves + sum_trait1_nodes
    total_nodes <- sum_n_leaves + sum_n_nodes
    total_non_trait1 <- total_nodes - total_trait1
    if (total_non_trait1 < 0) {
      total_non_trait1 <- 0
      total_nodes <- total_trait1
    }
    prop_i <- total_trait1 / total_nodes
    vect <- data.frame(
      node = node[i],
      info_node = ifelse(nrow(node_i) == 1, "terminal", ifelse(nrow(leaves) > 0 && nrow(nodes) > 0, "internal (terminal+leaves)", "internal (nodes)")),
      info_direct_child = ifelse(nrow(node_i) == 1, "", paste(phangorn::Children(tree, node[i]), collapse = "//")),
      info_direct_parent = phangorn::Ancestors(tree, node[i], type = "parent"),
      prop_trait1 = round(prop_i, 3),
      n_trait1 = total_trait1,
      n_non_trait1 = total_non_trait1,
      n = total_nodes,
      stringsAsFactors = FALSE
    )
    df <- rbind(df, vect)
  }
  df$n_trait1 <- as.numeric(df$n_trait1)
  df$n <- as.numeric(df$n)
  df$n_non_trait1 <- as.numeric(df$n_non_trait1)
  df
}

### random_trees_labels.R
random_trees_labels <- function(tree, n_simu, trait1) {
  node <- unique(tree$edge[, 1])
  df_simu <- data.frame(matrix(NA, nrow = length(node), ncol = n_simu))
  df_n_trait1 <- data.frame(matrix(NA, nrow = length(node), ncol = n_simu))
  df_n <- data.frame(matrix(NA, nrow = length(node), ncol = n_simu))
  for (M in 1:n_simu) {
    tree_theo <- tree
    random_indices <- sample(length(tree$tip.label))
    tree_theo$tip.label <- tree$tip.label[random_indices]
    df <- table_info_subtrees_obs(tree_theo, trait1) # nolint
    df_simu[, M] <- as.numeric(df$prop_trait1)
    df_n_trait1[, M] <- df$n_trait1
    df_n[, M] <- df$n
  }
  rownames(df_simu) <- rev(node)
  colnames(df_simu) <- 1:n_simu
  rownames(df_n_trait1) <- rev(node)
  colnames(df_n_trait1) <- 1:n_simu
  rownames(df_n) <- rev(node)
  colnames(df_n) <- 1:n_simu
  list(proba = df_simu, n_trait1 = df_n_trait1, n = df_n)
}

#### calculate_pvalues.R
calculate_pvalues_hypergeom <- function(df_obs, adj_method = "BH") {
  N_total <- max(df_obs$n)
  K_total <- df_obs$n_trait1[df_obs$n == N_total]

  p_raw <- mapply(function(k, n) {
    phyper(k - 1, K_total, N_total - K_total, n, lower.tail = FALSE)
  }, k = df_obs$n_trait1, n = df_obs$n)

  data.frame(node = df_obs$node, pvalue = p_raw)
}

calculate_pvalues_perm <- function(res, df, n_simu) {
  pvalues_table <- data.frame(node = character(),
                              pvalue = numeric(),
                              stringsAsFactors = FALSE)

  binary_matrix <- matrix(NA, nrow = n_simu, ncol = nrow(res[["proba"]]))
  colnames(binary_matrix) <- rownames(res[["proba"]])

  for (i in seq_len(nrow(res[["proba"]]))) {
    sim_props <- as.numeric(res[["proba"]][i, ])
    obs_prop  <- df[i, "prop_trait1"]
    comparison_vector <- as.numeric(!(sim_props < obs_prop))
    binary_matrix[, i] <- comparison_vector
    pvalue_i <- sum(comparison_vector) / n_simu
    if (!is.na(pvalue_i)) {
      node_name <- rownames(res[["proba"]])[i]
      pvalues_table <- rbind(pvalues_table, data.frame(node = node_name, pvalue = pvalue_i))
    }
  }

  list(pvalues_table = pvalues_table, binary_matrix = binary_matrix)
}

calculate_corr_pvalues_perm <- function(corrected_matrix) {
  pval_corrected <- colSums(corrected_matrix) / nrow(corrected_matrix)
  data.frame(node = colnames(corrected_matrix), corr_pvalue = pval_corrected)
}

mutaphy_test <- function(tree, trait1, trait0, n_simu = 1000, alpha = 0.05) {
  cat("MutaPhy test...\n")

  df_obs <- table_info_subtrees_obs(tree, trait1)

  # --- HYPERGEOMETRIC ---
  pvals_hyper <- calculate_pvalues_hypergeom(df_obs)
  df_obs_pvalue_hyper <- merge(df_obs, pvals_hyper[, c("node", "pvalue")], by = "node")

  # --- PERMUTATIONS ---
  res_perm   <- random_trees_labels(tree, n_simu, trait1)
  pvals_perm <- calculate_pvalues_perm(res_perm, df_obs, n_simu)


  df_obs_pvalue_perm <- merge(df_obs, pvals_perm$pvalues_table, by = "node", all.x = TRUE)
  df_obs_pvalue_perm <- df_obs_pvalue_perm[order(df_obs_pvalue_perm$pvalue,
                                                 -df_obs_pvalue_perm$prop_trait1,
                                                 -df_obs_pvalue_perm$n
  ),]


  tree_structure   <- get_tree_structure(tree)


  vect_pos_nodes <- as.character(df_obs_pvalue_perm[df_obs_pvalue_perm$pvalue < alpha, ]$node)

  binary_matrix    <- pvals_perm[["binary_matrix"]]
  corrected_matrix <- binary_matrix
  cols_all         <- colnames(corrected_matrix)

  treated <- character(0)


  for (node in vect_pos_nodes) {
    if (node %in% treated) next

    cols_to_update <- unique(intersect(tree_structure[[node]], vect_pos_nodes))
    cols_to_update <- intersect(cols_to_update, cols_all)
    cols_to_update <- setdiff(cols_to_update, node)

    if (length(cols_to_update)) {
      z <- corrected_matrix[, cols_to_update, drop = FALSE] == 0L
      corrected_matrix[, cols_to_update][z] <- 1L
    }


    treated <- unique(c(treated, node, cols_to_update))
  }

  corr_perm <- calculate_corr_pvalues_perm(corrected_matrix)

  df_obs_pvalue_perm <- merge(df_obs_pvalue_perm, corr_perm, by = "node", all.x = TRUE)


  pos_nodes_raw <- df_obs_pvalue_perm$node[df_obs_pvalue_perm$pvalue < alpha]
  m_corr <- sum(df_obs_pvalue_perm$corr_pvalue < alpha, na.rm = TRUE)
  if (m_corr > 0) {
    alpha_star <- alpha / m_corr
    pos_nodes_corr <- df_obs_pvalue_perm$node[df_obs_pvalue_perm$corr_pvalue < alpha_star]
  } else {
    alpha_star <- NA_real_
    pos_nodes_corr <- numeric(0)
  }

  return(list(
    tree = list(
      min_raw_pvalue = min(pvals_perm$pvalues_table$pvalue, na.rm = TRUE),
      min_corr_pvalue = min(corr_perm$corr_pvalue, na.rm = TRUE),
      mean_raw_pvalue  = mean(pvals_perm$pvalues_table$pvalue, na.rm = TRUE),
      mean_corr_pvalue = mean(corr_perm$corr_pvalue, na.rm = TRUE)
    ),
    subtrees = list(
      hypergeometric = df_obs_pvalue_hyper,
      permutation    = df_obs_pvalue_perm
    ),
    permutation_matrices = list(
      binary    = pvals_perm$binary_matrix,
      corrected = corrected_matrix
    ),
    positifs = list(
      permutation_nodes            = pos_nodes_raw,
      permutation_nodes_corrected  = pos_nodes_corr,
      alpha                        = alpha,
      alpha_star                   = alpha_star,
      m_corr                       = m_corr
    )
  ))
}

get_tree_structure <- function(tree) {
  internal_nodes <- unique(tree$edge[, 1])
  structure_list <- list()
  for (node in internal_nodes) {
    children <- phangorn::Descendants(tree, node, type = "all")
    ancestor <- phangorn::Ancestors(tree, node, type = "parent")
    vals <- c(as.character(children), as.character(ancestor))
    if (length(vals) > 0) {
      structure_list[[as.character(node)]] <- vals
    }
  }
  structure_list
}

get_site_candidates <- function(nodes, tree, sequences, threshold = 0.3, verbose = FALSE) {
  vcat   <- function(...) { if (isTRUE(verbose)) cat(...) }
  vprint <- function(x)   { if (isTRUE(verbose)) print(x) }

  nodes <- unique(as.character(na.omit(nodes)))
  if (length(nodes) == 0) return(list(candidates_by_node = list()))


  n_tips  <- ape::Ntip(tree)
  n_nodes <- ape::Nnode(tree)
  node_ids <- as.character((n_tips + 1):(n_tips + n_nodes))


  nodes <- intersect(nodes, node_ids)
  if (length(nodes) == 0) return(list(candidates_by_node = list()))


  parent_map <- setNames(rep(NA_character_, length(nodes)), nodes)
  for (nd in nodes) {
    p <- phangorn::Ancestors(tree, as.integer(nd), type = "parent")
    parent_map[nd] <- if (length(p)) as.character(p[1]) else NA_character_
  }

  genome_length <- length(sequences[[1]])
  candidates_by_node <- setNames(vector("list", length(nodes)), nodes)

  for (site in seq_len(genome_length)) {
    vcat("\n==== Site", site, "====\n")


    site_states <- sapply(sequences, function(seq) seq[site])
    names(site_states) <- names(sequences)


    if (length(unique(site_states)) <= 1) {
      vcat("→ Non-variable site, skipped\n"); next
    }


    ace_res <- tryCatch(ape::ace(site_states, tree, type = "discrete", model = "ER"),
                        error = function(e) NULL)
    if (is.null(ace_res)) { vcat("→ ACE failed\n"); next }

    lik_anc <- ace_res$lik.anc
    rownames(lik_anc) <- node_ids


    for (nd in nodes) {
      parent_nd <- parent_map[[nd]]
      if (is.na(parent_nd)) next

      probs_best   <- lik_anc[nd, ]
      probs_parent <- lik_anc[parent_nd, ]

      nuc_best   <- names(which.max(probs_best))
      nuc_parent <- names(which.max(probs_parent))

      prob_best   <- probs_best[nuc_best]
      prob_parent <- probs_parent[nuc_parent]


      detect <- (nuc_best != nuc_parent) || (abs(prob_best - prob_parent) >= threshold)
      if (detect) {
        candidates_by_node[[nd]] <- c(candidates_by_node[[nd]], site)
      }
    }
  }

  candidates_by_node <- lapply(candidates_by_node, function(v) if (length(v)) sort(unique(v)) else integer(0))

  list(candidates_by_node = candidates_by_node)
}
