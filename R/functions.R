#' Compute subtree statistics for an observed phenotype (internal)
#'
#' @param tree A \code{phylo} with phenotype encoded in tip labels.
#' @param trait1 Phenotype of interest.
#' @return A data.frame with counts and proportions of trait1 for each internal node.
#' @importFrom stats phyper na.omit setNames
#' @export
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

#' Permute tip labels to generate null subtree statistics (internal)
#'
#' @param tree A \code{phylo} object.
#' @param n_simu Integer. Number of permutations.
#' @param trait1 Phenotype of interest.
#' @return A list of matrices containing simulated subtree proportions and counts.
#' @importFrom stats phyper na.omit setNames
#' @export
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

#' Compute hypergeometric p-values for subtree (internal)
#'
#' @param df_obs Data frame returned by \code{table_info_subtrees_obs()}.
#' @return A data.frame with columns \code{node} and \code{pvalue}.
#' @importFrom stats phyper na.omit setNames
#' @export
calculate_pvalues_hypergeom <- function(df_obs) {
  N_total <- max(df_obs$n)
  K_total <- df_obs$n_trait1[df_obs$n == N_total]

  p_raw <- mapply(function(k, n) {
    phyper(k - 1, K_total, N_total - K_total, n, lower.tail = FALSE)
  }, k = df_obs$n_trait1, n = df_obs$n)

  data.frame(node = df_obs$node, pvalue = p_raw)
}

#' Computes subtree-level permutation p-values by comparing the observed
#' proportion of \code{trait1} in each subtree to its null distribution
#' obtained by randomizing tip labels.
#'
#' @param res A list returned by \code{random_trees_labels()}, containing
#'   simulated subtree proportions under the null hypothesis.
#' @param df A data.frame returned by \code{table_info_subtrees_obs()},
#'   containing observed subtree statistics.
#' @param n_simu Integer. Number of permutations used to build the null distribution.
#'
#' @return A list with two elements:
#' \itemize{
#'   \item \code{pvalues_table}: data.frame with permutation p-values per node.
#'   \item \code{binary_matrix}: binary matrix used for hierarchical correction.
#' }
#' @importFrom stats phyper na.omit setNames
#' @export
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

#' Computes corrected permutation p-values after hierarchical correction
#' by aggregating the corrected binary permutation matrix.
#'
#' @param corrected_matrix A binary matrix after hierarchical correction,
#'   as produced within \code{mutaphy_test()}.
#'
#' @return A data.frame with corrected permutation p-values for each node.
#' @importFrom stats phyper na.omit setNames
#' @export
calculate_corr_pvalues_perm <- function(corrected_matrix) {
  pval_corrected <- colSums(corrected_matrix) / nrow(corrected_matrix)
  data.frame(node = colnames(corrected_matrix), corr_pvalue = pval_corrected)
}

#' Detect phenotype-associated clades using permutation and hypergeometric tests
#'
#' \code{mutaphy_test()} identifies subtrees enriched for a binary phenotype
#' (encoded in the tip labels of a \code{phylo} object). It computes both:
#' (i) hypergeometric p-values based on counts within each subtree, and
#' (ii) permutation p-values by randomizing tip labels \code{n_simu} times.
#' A hierarchical correction procedure is applied to permutation results to
#' account for nested (overlapping) subtrees.
#'
#' @param tree A phylogenetic tree of class \code{phylo}. Tip labels must encode
#'   the phenotype state (e.g., \code{trait1} vs \code{trait0}).
#' @param trait1 Character string. Name of the phenotype of interest.
#' @param trait0 Character string. Name of the reference phenotype.
#' @param n_simu Integer. Number of permutations used to estimate the null
#'   distribution for permutation p-values.
#' @param alpha Numeric. Significance threshold used to define positive subtrees.
#' @param verbose Logical. If \code{TRUE}, print progress and interpretation messages.
#'
#' @return A named list with the following components:
#' \itemize{
#'   \item \code{tree}: summary statistics at the tree level (min/mean raw and corrected p-values).
#'   \item \code{subtrees}: subtree-level results for both hypergeometric and permutation tests.
#'   \item \code{permutation_matrices}: binary and corrected permutation matrices.
#'   \item \code{positifs}: sets of significant nodes (raw and corrected) and correction parameters.
#' }
#'
#' @examples
#' \dontrun{
#' res <- mutaphy_test(
#'   tree,
#'   trait1 = "severe",
#'   trait0 = "non_severe",
#'   n_simu = 1000,
#'   alpha = 0.05,
#'   verbose = TRUE
#' )
#' res$positifs$permutation_nodes_corrected
#' }
#' @importFrom stats phyper na.omit setNames
#' @export
mutaphy_test <- function(tree, trait1, trait0, n_simu = 1000, alpha = 0.05, verbose = FALSE) {

  if (isTRUE(verbose)) {
    message(
      "Running MutaPhy: testing whether phenotype '", trait1,
      "' is over-represented relative to '", trait0, "'."
    )
  }

  # ---- observed subtree stats ----
  df_obs <- table_info_subtrees_obs(tree, trait1)

  # ---- hypergeometric p-values ----
  pvals_hyper <- calculate_pvalues_hypergeom(df_obs)
  df_obs_pvalue_hyper <- merge(df_obs, pvals_hyper[, c("node", "pvalue")], by = "node")

  # ---- permutation p-values (raw) ----
  res_perm   <- random_trees_labels(tree, n_simu, trait1)
  pvals_perm <- calculate_pvalues_perm(res_perm, df_obs, n_simu)

  df_obs_pvalue_perm <- merge(df_obs, pvals_perm$pvalues_table, by = "node", all.x = TRUE)

  df_obs_pvalue_perm$node <- as.character(df_obs_pvalue_perm$node)
  df_obs_pvalue_perm$pvalue <- as.numeric(df_obs_pvalue_perm$pvalue)

  # ---- determine significant nodes and enforce TOP-FIRST order ----
  sig_df <- df_obs_pvalue_perm[!is.na(df_obs_pvalue_perm$pvalue) & df_obs_pvalue_perm$pvalue < alpha, ]
  sig_df <- sig_df[order(sig_df$pvalue, -sig_df$prop_trait1, -sig_df$n), ]
  sig_nodes <- as.character(sig_df$node)

  # ---- hierarchical correction on permutation matrix ----
  tree_structure <- get_tree_structure(tree)

  corrected_matrix <- pvals_perm[["binary_matrix"]]

  selected_tops <- character(0)  # tops chosen (PROTECTED)
  neutralized   <- character(0)  # nodes neutralized (ignored as future tops)

  cols_all <- colnames(corrected_matrix)

  for (top in sig_nodes) {

    # already neutralized by a previous top -> cannot become top
    if (top %in% neutralized) next
    # safety
    if (top %in% selected_tops) next

    # select and protect this top
    selected_tops <- c(selected_tops, top)

    # neighbors to neutralize: descendants + ancestors that are significant
    neigh <- c(tree_structure[[top]]$descendants, tree_structure[[top]]$ancestors)
    to_neutralize <- intersect(neigh, sig_nodes)

    # never neutralize already-selected tops, and never neutralize the current top itself
    to_neutralize <- setdiff(to_neutralize, selected_tops)
    to_neutralize <- setdiff(to_neutralize, top)

    # keep only columns actually present
    to_neutralize <- intersect(to_neutralize, cols_all)

    if (length(to_neutralize)) {
      z <- corrected_matrix[, to_neutralize, drop = FALSE] == 0L
      corrected_matrix[, to_neutralize][z] <- 1L
      neutralized <- unique(c(neutralized, to_neutralize))
    }
  }

  # corrected p-values from corrected matrix
  corr_perm <- calculate_corr_pvalues_perm(corrected_matrix)
  corr_perm$node <- as.character(corr_perm$node)

  df_obs_pvalue_perm <- merge(df_obs_pvalue_perm, corr_perm, by = "node", all.x = TRUE)

  # order output (raw pvalue then enrichment then size)
  df_obs_pvalue_perm <- df_obs_pvalue_perm[order(df_obs_pvalue_perm$pvalue,
                                                 -df_obs_pvalue_perm$prop_trait1,
                                                 -df_obs_pvalue_perm$n), ]

  # ---- positives (raw + corrected) ----
  pos_nodes_raw <- df_obs_pvalue_perm$node[!is.na(df_obs_pvalue_perm$pvalue) &
                                             df_obs_pvalue_perm$pvalue < alpha]

  # “corrected positives” using alpha_star rule
  m_corr <- sum(df_obs_pvalue_perm$corr_pvalue < alpha, na.rm = TRUE)
  if (m_corr > 0) {
    alpha_star <- alpha / m_corr
    pos_nodes_corr <- df_obs_pvalue_perm$node[df_obs_pvalue_perm$corr_pvalue < alpha_star]
  } else {
    alpha_star <- NA_real_
    pos_nodes_corr <- character(0)
  }

  # ---- tree summary ----
  out_tree <- list(
    min_raw_pvalue  = min(df_obs_pvalue_perm$pvalue, na.rm = TRUE),
    min_corr_pvalue = min(df_obs_pvalue_perm$corr_pvalue, na.rm = TRUE),
    mean_raw_pvalue  = mean(df_obs_pvalue_perm$pvalue, na.rm = TRUE),
    mean_corr_pvalue = mean(df_obs_pvalue_perm$corr_pvalue, na.rm = TRUE),
    top_nodes_order = selected_tops  # <- this is the actual “top selection”
  )

  list(
    tree = out_tree,
    subtrees = list(
      hypergeometric = df_obs_pvalue_hyper,
      permutation    = df_obs_pvalue_perm
    ),
    permutation_matrices = list(
      binary    = pvals_perm$binary_matrix,
      corrected = corrected_matrix
    ),
    positifs = list(
      permutation_nodes           = pos_nodes_raw,
      permutation_nodes_corrected = pos_nodes_corr,
      alpha      = alpha,
      alpha_star = alpha_star,
      m_corr     = m_corr,
      selected_tops = selected_tops
    )
  )
}

#' Build the node neighborhood structure used for hierarchical correction (internal)
#'
#' @param tree A \code{phylo} object.
#' @return A named list mapping each internal node to its descendants and parent.
#' @importFrom stats phyper na.omit setNames
#' @export
get_tree_structure <- function(tree) {
  internal_nodes <- unique(tree$edge[, 1])
  structure_list <- vector("list", length(internal_nodes))
  names(structure_list) <- as.character(internal_nodes)

  for (node in internal_nodes) {
    structure_list[[as.character(node)]] <- list(
      descendants = as.character(phangorn::Descendants(tree, node, type = "all")),
      ancestors   = as.character(phangorn::Ancestors(tree, node, type = "all"))
    )
  }
  structure_list
}

#' Identify candidate mutations on branches leading to selected clades
#'
#' Given one or several internal nodes (typically detected by \code{mutaphy_test}),
#' this function performs ancestral state reconstruction site-by-site using
#' \code{ape::ace} (discrete model) and reports genomic positions where a change
#' is detected on the branch leading to each target node. Detection is controlled
#' by a probability threshold comparing parent vs child reconstructed states.
#'
#' @param nodes Integer or character vector of internal node identifiers in \code{tree}.
#' @param tree A phylogenetic tree of class \code{phylo}. Tip labels must match
#'   the names of \code{sequences}.
#' @param sequences Named list of aligned nucleotide sequences. Each element is a
#'   character vector of nucleotides ("A","C","G","T"), with optional \code{NA}
#'   for missing/ambiguous characters. Names must match \code{tree$tip.label}.
#' @param threshold Numeric. Minimum difference between parent and child posterior
#'   probabilities required to call a candidate mutation (default: 0.3).
#' @param verbose Logical. If \code{TRUE}, print diagnostic messages.
#'
#' @return A list with element \code{candidates_by_node}, a named list where each
#'   name is a node id and each value is an integer vector of candidate site
#'   positions (empty if none detected).
#'
#' @examples
#' \dontrun{
#' sites <- get_site_candidates(nodes = 360, tree = tree_ids, sequences = seqs, threshold = 0.3)
#' sites$candidates_by_node[["360"]]
#' }
#' @importFrom stats phyper na.omit setNames
#' @import ape
#' @export
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
      vcat("-> Non-variable site, skipped\n"); next
    }
    ace_res <- tryCatch(ape::ace(site_states, tree, type = "discrete", model = "ER"),
                        error = function(e) NULL)
    if (is.null(ace_res)) { vcat("-> ACE failed\n"); next }
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
