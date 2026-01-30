# generate_simulation_trees.R modification
generate_simulation_trees <- function(params, score, hypothesis, Mtot_prop, output_dir, trait1, trait0) {
  if (!hypothesis %in% c("H0", "H1")) {
    stop("Hypothesis must be 'H0' or 'H1'")
  }
  nucleotide <- params$nucleotide
  n_tips <- params$n_tips
  factor <- params$factor
  n_trees <- params$n_trees
  mutation_rate <- params$mutation_rate
  mutable_fraction <- params$mutable_fraction
  Mtot_prop <- params$Mtot_prop
  noise_type <- params$noise_type
  b <- params$b
  target_nb_polymorphic_sites <- params$target_nb_polymorphic_sites

  list_tree <- lapply(1:n_trees, function(x) {
    tree <- ape::rcoal(n_tips)
    tree$edge.length <- tree$edge.length * factor
    return(tree)
  })

  results_tree_list <- lapply(list_tree, function(tr) {
    list(
      tree = tr,
      simulation = simulate_shared_sequence_evolution( # nolint
        tree = tr,
        nucleotide = nucleotide,
        mutation_rate = mutation_rate,
        mutable_fraction = mutable_fraction,
        score = score,
        Mtot_prop = Mtot_prop,
        target_nb_polymorphic_sites = target_nb_polymorphic_sites
      )
    )
  })

  # Infectivity scores
  infectivity_score_matrix <- matrix(NA, nrow = n_trees, ncol = n_tips)
  for (i in 1:n_trees) {
    simu_n <- unlist(results_tree_list[[i]][["simulation"]][["si_scores"]])
    infectivity_score_matrix[i, ] <- simu_n
  }

  # Probas
  prob_matrix <- matrix(NA, nrow = n_trees, ncol = n_tips)
  for (i in 1:n_trees) {
    simu_n <- infectivity_score_matrix[i, ]
    Mtot <- results_tree_list[[i]][["simulation"]][["Mtot"]]
    k <- 20 / (Mtot + 1)
    c <- Mtot / 2
    r <- k * c - log((1 / b) - 1)

    prob_matrix[i, ] <- logistic_function( # nolint
      m = simu_n,
      k = k,
      c = c,
      r = r,
      b = b,
      noise_type = noise_type
    )
  }

  bernoulli_matrix <- matrix(
    rbinom(n = length(prob_matrix), size = 1, prob = prob_matrix),
    nrow = nrow(prob_matrix),
    ncol = ncol(prob_matrix)
  )
  tip_labels_df <- matrix(
    ifelse(bernoulli_matrix == 1, trait1, trait0),
    nrow = nrow(bernoulli_matrix),
    ncol = ncol(bernoulli_matrix)
  )
  colnames(tip_labels_df) <- paste("Tip", 1:n_tips, sep = "_")

  prop_trait1_k <- apply(
    tip_labels_df, 1,
    function(x) sum(x == trait1) / length(x)
  )
  prop_trait1_mean <- mean(prop_trait1_k)

  list_tree <- lapply(seq_along(results_tree_list), function(i) {
    tree_i <- results_tree_list[[i]]$tree
    labels <- tip_labels_df[i, ]
    tree_i$tip.label <- as.character(labels)
    return(tree_i)
  })

  save_path <- here::here(sprintf("%s/saves/01-simulation_trees_%s_%dseqs.RData", # nolint
                                  output_dir, hypothesis, n_tips))
  if (!dir.exists(dirname(save_path))) {
    dir.create(dirname(save_path), recursive = TRUE)
  }
  save(list_tree,
       results_tree_list,
       infectivity_score_matrix,
       prob_matrix,
       bernoulli_matrix,
       tip_labels_df,
       prop_trait1_k,
       prop_trait1_mean,
       file = save_path)

  return(list(
    list_tree = list_tree,
    results_tree_list = results_tree_list,
    infectivity_score_matrix = infectivity_score_matrix,
    prob_matrix = prob_matrix,
    bernoulli_matrix = bernoulli_matrix,
    tip_labels_df = tip_labels_df,
    prop_trait1_k = prop_trait1_k,
    prop_trait1_mean = prop_trait1_mean
  ))
}


# logistic function.R
logistic_function <- function(m, c, k, r, b, noise_type) {
  if (noise_type == "two-sided") {
    return((1 - b) / (1 + exp(-k * (m - c) - r)))
  } else if (noise_type == "one-sided") {
    return(1 / (1 + exp(-k * (m - c) - r)))
  } else if (noise_type == "no") {
    return(1 / (1 + exp(-k * (m - c) - r)))
  } else {
    stop('Invalid noise_type: "no", "one-sided" or "two-sided')
  }
}
proba_score_Mtot_is <- function(score, c, k, r, Mtot, b, noise_type) {
  m_values <- 0:Mtot
  is_values <- score * m_values
  proba_values <- sapply(is_values, logistic_function, c = c, k = k, r = r, b = b, noise_type = noise_type)
  df <- data.frame(
    score = rep(score, length(m_values)),
    m = m_values,
    infectivity_score = is_values,
    probability = proba_values
  )
  return(df)
}

# method_save_significant_subtrees.R
create_save_file <- function(output_dir, hypothesis, n_tips, alpha, n_trees) {
  save_path <- here::here(sprintf("%s/saves/02-method_subtrees_mutations_%s_%dseqs_alpha%g.txt",
                                  output_dir, hypothesis, n_tips, alpha))

  if (!file.exists(save_path)) {
    file.create(save_path)
    empty_lines <- rep("", n_trees)
    write.table(empty_lines, file = save_path, row.names = FALSE, col.names = FALSE, quote = FALSE)
  }

  return(save_path)
}
method_save_significant_subtrees <- function(pvalues_table, output_dir, hypothesis, n_tips, alpha, tree_number) {
  save_path <- here::here(sprintf("%s/saves/02-method_subtrees_mutations_%s_%dseqs_alpha%g.txt", # nolint
                                  output_dir, hypothesis, n_tips, alpha))

  lines <- readLines(save_path)

  significant_nodes <- pvalues_table[which(pvalues_table$pvalue <= alpha), ]$node
  node_line <- if (length(significant_nodes) == 0) "" else paste(significant_nodes, collapse = " ")

  lines[tree_number] <- node_line

  writeLines(lines, con = save_path)
}

# simulate_shared_sequence_evolution.R
random_sample_per_position <- function(df) {
  unique_positions <- unique(df$Position)
  sampled_rows <- lapply(unique_positions, function(pos) {
    rows_for_position <- df[df$Position == pos, ]
    sampled_row <- rows_for_position[sample(1:nrow(rows_for_position), 1), ] # nolint
    return(sampled_row)
  })
  return(do.call(rbind, sampled_rows))
}
simulate_shared_sequence_evolution <- function(tree, nucleotide, mutation_rate, mutable_fraction, score, Mtot_prop, target_nb_polymorphic_sites) {
  nb_tips <- length(tree$tip.label)
  sequences <- replicate(nb_tips, character(0), simplify = FALSE)
  mutations_seqs <- replicate(nb_tips, data.frame(), simplify = FALSE)

  all_mutations_full <- data.frame()

  genome_size <- 0
  repeat {
    genome_size <- genome_size + 1
    res_site <- simulate_site_only(tree, genome_size, nucleotide, mutation_rate, score)

    for (i in seq_along(res_site$nucs)) {
      sequences[[i]] <- c(sequences[[i]], res_site$nucs[[i]])
      mutations_seqs[[i]] <- rbind(mutations_seqs[[i]], res_site$mutations_seqs[[i]])
      all_mutations_full <- rbind(all_mutations_full, res_site$mutations_seqs[[i]])
    }


    nb_poly <- length(unique(all_mutations_full$Position))
    if (nb_poly >= target_nb_polymorphic_sites) break
  }

  names(sequences) <- paste0("Seq", 1:length(sequences)) # nolint
  names(mutations_seqs) <- paste0("Seq", 1:length(mutations_seqs)) # nolint

  all_mutations_full <- all_mutations_full[order(all_mutations_full$Position, -all_mutations_full$Node), ]
  all_mutations <- random_sample_per_position(all_mutations_full)
  unique_mutations <- all_mutations[, c("Position", "Node")]
  top_Mtot_mutated_sites <- head(unique_mutations, floor(Mtot_prop * nrow(unique_mutations)))

  si_scores <- setNames(as.list(rep(0, length(sequences))), names(sequences))
  for (seq_name in names(sequences)) {
    mutated_positions <- mutations_seqs[[seq_name]]
    if (!is.null(mutated_positions) && all(c("Position", "Node") %in% colnames(mutated_positions))) {
      mutated_positions_filtered <- mutated_positions[, c("Position", "Node")]
      mutations_in_top_Mtot <- merge(mutated_positions_filtered, top_Mtot_mutated_sites, by = c("Position", "Node"))
      si_scores[[seq_name]] <- nrow(mutations_in_top_Mtot) * score
    }
  }

  return(list(
    sequences = sequences,
    mutations_seqs = mutations_seqs,
    mutated_positions = lapply(mutations_seqs, function(x) sort(unique(x$Position))),
    #all_mutations_full = all_mutations_full,
    all_mutations = all_mutations,
    si_scores = si_scores,
    Mtot = floor(Mtot_prop * nrow(unique_mutations)),
    genome_size = genome_size
  ))
}

simulate_site_only <- function(tree,
                               site_position,
                               nucleotide,
                               mutation_rate,
                               score) {
  simulate_recursive <- function(node, nuc, mutations_seqs) {
    children <- which(tree$edge[, 1] == node)
    leaf_nucleotides <- list()
    all_mutation_records <- list()

    for (child in seq_along(children)) {
      child_node <- tree$edge[children[child], 2]
      branch_length <- tree$edge.length[children[child]]
      new_nuc <- nuc
      child_mutations_seqs <- mutations_seqs

      num_mutations <- rpois(1, mutation_rate * branch_length)
      if (num_mutations > 0) {
        for (i in 1:num_mutations) {
          old_nuc <- new_nuc
          new_nuc <- sample(setdiff(nucleotide, old_nuc), size = 1)

          scores <- c(A = 0, C = 0, G = 0, T = 0)
          scores[new_nuc] <- score

          mutation_info <- data.frame(
            Position = site_position,
            Old_Nucleotide = old_nuc,
            New_Nucleotide = new_nuc,
            Node = child_node,
            A = scores["A"],
            C = scores["C"],
            G = scores["G"],
            T = scores["T"],
            stringsAsFactors = FALSE
          )

          child_mutations_seqs <- rbind(child_mutations_seqs, mutation_info)
        }
      }

      results <- simulate_recursive(child_node, new_nuc, child_mutations_seqs)
      leaf_nucleotides <- c(leaf_nucleotides, results$nucs)
      all_mutation_records <- c(all_mutation_records, results$mutations_seqs)

    }


    if (length(children) == 0) {
      return(list(nucs = list(nuc), mutations_seqs = list(mutations_seqs)))
    }

    return(list(
      nucs = leaf_nucleotides,
      mutations_seqs = all_mutation_records
    ))
  }


  root_nuc <- sample(nucleotide, 1)
  root_node <- tree$edge[1, 1]
  return(simulate_recursive(root_node, root_nuc, data.frame()))
}


simulation_save_subtrees <- function(results_tree, hypothesis, n_tips, output_dir) {
  save_path <- here::here(sprintf("%s/saves/01-simulation_trees_all_mutations_%s_%dseqs.txt",
                                  output_dir, hypothesis, n_tips))
  file_conn <- file(save_path, open = "wt")

  for (i in seq_along(results_tree$results_tree_list)) {
    all_mutations_i <- results_tree$results_tree_list[[i]][["simulation"]][["all_mutations"]]
    Mtot_i <- results_tree$results_tree_list[[i]][["simulation"]][["Mtot"]]

    node_data <- all_mutations_i[["Node"]][1:Mtot_i]
    node_line <- paste(node_data, collapse = " ")
    writeLines(node_line, file_conn)
  }

  close(file_conn)
}

# tree_runner.R
tree_runner <- function(tree_number, results_h0, results_h1, params) {

  # --- H0 ---
  res_mutaphy_test_h0 <- mutaphy_test( # nolint
    tree = results_h0$list_tree[[tree_number]],
    #sequences = results_h0$results_tree_list[[tree_number]][["simulation"]][["sequences"]],
    trait1 = params$trait1,
    trait0 = params$trait0,
    n_simu = params$n_simu,
    alpha = params$alpha[1]
  )

  method_save_significant_subtrees(res_mutaphy_test_h0$subtrees$permutation, params$output_dir, # nolint
                                   params$null_hypothesis, params$n_tips, params$alpha[1], tree_number)
  method_save_significant_subtrees(res_mutaphy_test_h0$subtrees$permutation, params$output_dir, # nolint
                                   params$null_hypothesis, params$n_tips, params$alpha[2], tree_number)

  # --- H1 ---
  res_mutaphy_test_h1 <- mutaphy_test( # nolint
    tree = results_h1$list_tree[[tree_number]],
    #sequences = results_h0$results_tree_list[[tree_number]][["simulation"]][["sequences"]],
    trait1 = params$trait1,
    trait0 = params$trait0,
    n_simu = params$n_simu,
    alpha = params$alpha[1]
  )

  method_save_significant_subtrees(res_mutaphy_test_h1$subtrees$permutation, params$output_dir, # nolint
                                   params$alternative_hypothesis, params$n_tips, params$alpha[1], tree_number)
  method_save_significant_subtrees(res_mutaphy_test_h1$subtrees$permutation, params$output_dir, # nolint
                                   params$alternative_hypothesis, params$n_tips, params$alpha[2], tree_number)

  return(list(
    mutaphy_h0 = res_mutaphy_test_h0,
    mutaphy_h1 = res_mutaphy_test_h1
  ))
}
