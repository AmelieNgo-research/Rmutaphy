library(CRPTree)

## PS
random_PS_scores <- function(tree, trait, n_simu) {
  ps_scores <- numeric(n_simu)

  for (i in 1:n_simu) {
    permuted_trait <- sample(trait)
    ps_scores[i] <- calculate_PS(tree, permuted_trait)
  }

  list(ps_scores = ps_scores)
}
calculate_pvalue_for_PS <- function(ps_obs, res, n_simu) {
  ps_null <- res$ps_scores

  pvalue <- (1 + sum(ps_null <= ps_obs)) / (1 + n_simu)

  data.frame(statistic = ps_obs, pvalue = pvalue)
}
ps_test <- function(tree, trait, n_simu = 1000) {
  cat("Parsimony Score (PS)...\n")

  ps_obs <- calculate_PS(tree, trait)
  res <- random_PS_scores(tree, trait, n_simu)
  pval <- calculate_pvalue_for_PS(ps_obs, res, n_simu)

  return(list(ps_obs = ps_obs, pvalue = pval$pvalue, null_distribution = res$ps_scores))
}
calculate_PS <- function(tree, trait) {
  internal_nodes <- unique(tree$edge[,1])
  node_states <- vector("list", max(tree$edge))
  PS_value <- 0

  for (tip in 1:ape::Ntip(tree)) {
    node_states[[tip]] <- list(trait[tip])
  }

  for (node in rev(internal_nodes)) {
    children <- tree$edge[tree$edge[,1] == node, 2]

    state_left <- node_states[[children[1]]]
    state_right <- node_states[[children[2]]]

    intersection <- intersect(state_left, state_right)

    if (length(intersection) > 0) {
      node_states[[node]] <- intersection
    } else {
      node_states[[node]] <- union(state_left, state_right)
      PS_value <- PS_value + 1
    }
  }

  return(PS_value)
}

## AI
random_AI_scores <- function(tree, trait, n_simu = 1000) {
  ai_scores <- numeric(n_simu)

  for (i in 1:n_simu) {
    permuted_trait <- sample(trait)
    ai_scores[i] <- calculate_AI(tree, permuted_trait)
  }

  list(ai_scores = ai_scores)
}
calculate_pvalue_for_AI <- function(ai_obs, res, n_simu) {
  ai_null <- res$ai_scores
  pvalue <- (1 + sum(ai_null <= ai_obs)) / (1 + n_simu)

  data.frame(statistic = ai_obs, pvalue = pvalue)
}
ai_test <- function(tree, trait, n_simu = 1000) {
  cat("Association Index (AI)...\n")

  ai_obs <- calculate_AI(tree, trait)
  res <- random_AI_scores(tree, trait, n_simu)
  pval <- calculate_pvalue_for_AI(ai_obs, res, n_simu)

  return(list(ai_obs = ai_obs, pvalue = pval$pvalue, null_distribution = res$ai_scores))
}
calculate_AI <- function(tree, trait) {
  internal_nodes <- unique(tree$edge[,1])
  AI_value <- 0

  for (node in internal_nodes) {
    tips_descendants <- get_tips_descendants(tree, node)

    if (length(tips_descendants) > 1) {
      leaf_traits <- trait[tips_descendants]

      if (length(leaf_traits) == 0) next

      tabled_traits <- table(leaf_traits)

      f_i <- max(tabled_traits) / sum(tabled_traits)

      m_i <- length(tips_descendants)

      AI_i <- (1 - f_i) / (2^(m_i-1))

      AI_value <- AI_value + AI_i
    }
  }

  return(AI_value)
}

## MC
random_MC_scores <- function(tree, trait, trait0, trait1, n_simu = 1000) {
  mc_trait0 <- numeric(n_simu)
  mc_trait1 <- numeric(n_simu)

  for (i in 1:n_simu) {
    permuted_trait <- sample(trait)
    mc_vals <- calculate_MC(tree, permuted_trait, trait0, trait1)
    mc_trait0[i] <- mc_vals[trait0]
    mc_trait1[i] <- mc_vals[trait1]
  }

  list(mc_trait0 = mc_trait0, mc_trait1 = mc_trait1)
}
calculate_pvalue_for_MC <- function(mc_obs, res, n_simu) {
  pval0 <- (1 + sum(res$mc_trait0 >= mc_obs[1])) / (1 + n_simu)
  pval1 <- (1 + sum(res$mc_trait1 >= mc_obs[2])) / (1 + n_simu)

  data.frame(
    trait = c(names(mc_obs)[1], names(mc_obs)[2]),
    MC_obs = c(mc_obs[1], mc_obs[2]),
    pvalue = c(pval0, pval1)
  )
}
mc_test <- function(tree, trait, trait0, trait1, n_simu = 1000) {
  cat("Monophyletic Clade (MC)...\n")
  cat(paste0("Trait 1 : ", trait1, "\nTrait 0 : ", trait0, "\n"))

  mc_obs <- calculate_MC(tree, trait, trait0, trait1)
  res <- random_MC_scores(tree, trait, trait0, trait1, n_simu)
  pvals <- calculate_pvalue_for_MC(mc_obs, res, n_simu)

  return(list(mc_obs = mc_obs, null_distributions = res, pvalues = pvals))
}
calculate_MC <- function(tree, trait, trait0, trait1) {
  internal_nodes <- unique(tree$edge[, 1])
  all_traits <- c(trait1, trait0)
  MC_values <- setNames(rep(1, length(all_traits)), all_traits)

  for (state in all_traits) {
    if (!(state %in% trait)) {
      MC_values[state] <- 0
      next
    }

    max_clade_size <- 1

    for (node in internal_nodes) {
      tips_descendants <- get_tips_descendants(tree, node)
      leaf_traits <- trait[tips_descendants]

      if (length(unique(leaf_traits)) == 1 && unique(leaf_traits) == state) {
        max_clade_size <- max(max_clade_size, length(tips_descendants))
      }
    }

    MC_values[state] <- max_clade_size
  }

  return(MC_values)
}


phylo.stats <- function(tree, trait, trait0, trait1, n_simu = 1000, alpha = 0.05) {
  ai_result <- ai_test(tree = tree, trait = trait)
  ps_result <- ps_test(tree = tree, trait = trait, n_simu = n_simu)
  mc_result <- mc_test(tree = tree, trait = trait, trait0 = trait0, trait1 = trait1, n_simu = n_simu)

  results_all <- list(
    AI = list(
      statistic = ai_result$ai_obs,
      pvalue = ai_result$pvalue,
      null_distribution = ai_result$ai_scores
    ),
    PS = list(
      statistic = ps_result$ps_obs,
      pvalue = ps_result$pvalue,
      null_distribution = ps_result$null_distribution
    ),
    MC = list(
      statistics = mc_result$mc_obs,
      pvalues = mc_result$pvalues,
      null_distributions = mc_result$null_distributions
    )
  )

  return(results_all)
}
get_tips_descendants <- function(tree, node) {
  descendants <- phangorn::Descendants(tree, node, "all")
  internal_descendants <- descendants[descendants <= ape::Ntip(tree)]
  return(internal_descendants)
}
process_batch <- function(Mtot_pct, n_seqs, k = 20, noise, H) {

  folder <- glue::glue("results/results_{Mtot_pct}_{noise}_nsimu1000/saves")
  simulation_trees_ <- glue::glue("{folder}/01-simulation_trees_{H}_{n_seqs}")

  load(here::here(glue::glue("{simulation_trees_}.RData")))
  mutation_file <- glue::glue("{folder}/01-simulation_trees_all_mutations_{H}_{n_seqs}.txt")
  simu_subtrees <- readLines(here::here(mutation_file))

  simu_subtrees <- lapply(simu_subtrees, function(line) {
    if (nzchar(line)) as.numeric(unlist(strsplit(line, " "))) else numeric(0)
  })
  load(here::here(glue::glue("{folder}/tree_outputs_H0_H1_{n_seqs}.RData")))

  results <- lapply(seq_along(list_tree), function(i) { # nolint
    cat(paste0("\n=== Tree ", i, " ===\n"))

    tree <- list_tree[[i]] # nolint
    trait <- tree$tip.label

    if (length(unique(trait)) < 2) {
      random_tip <- sample(seq_along(trait), 1)
      trait[random_tip] <- if (trait[1] == "severe") "non severe" else "severe"
    }


    tree$tip.label <- trait

    res_phylo_stats <- phylo.stats(tree = tree, trait = trait, trait0 = "non severe", trait1 = "severe")

    tips_corresponding <- cbind(
      tree$tip.label,
      ifelse(trait == "severe", -1, -2)
    )

    cat("CRP-Tree... ")
    tree_processed <- CRPTree::process_tree(tree, tip_corresponding = tips_corresponding)
    crptree_res <- CRPTree::one_tree_all_methods(tree_processed)

    res_phylo_stats$CRPTree <- crptree_res

    if (H == "H1") {
      res_phylo_stats$MutaPhy <- tree_outputs[[i]][["mutaphy_h1"]] # nolint
    } else if (H == "H0") {
      res_phylo_stats$MutaPhy <- tree_outputs[[i]][["mutaphy_h0"]] # nolint
    }

    return(res_phylo_stats)
  })

  return(results)
}


# No noise
## H1
## Mtot1pct
res_all_n20_Mtot1pct_k20_no_noise <- process_batch(Mtot_pct = "Mtot1pct", n_seqs = "20seqs", noise = "no_background_noise", H = "H1") # fait
res_all_n50_Mtot1pct_k20_no_noise <- process_batch(Mtot_pct = "Mtot1pct", n_seqs = "50seqs", noise = "no_background_noise", H = "H1") # fait
res_all_n100_Mtot1pct_k20_no_noise <- process_batch(Mtot_pct = "Mtot1pct", n_seqs = "100seqs", noise = "no_background_noise", H = "H1") # fait
res_all_n300_Mtot1pct_k20_no_noise <- process_batch(Mtot_pct = "Mtot1pct", n_seqs = "300seqs", noise = "no_background_noise", H = "H1") # fait
## Mtot10pct
res_all_n20_Mtot10pct_k20_no_noise <- process_batch(Mtot_pct = "Mtot10pct", n_seqs = "20seqs", noise = "no_background_noise", H = "H1") # fait
res_all_n50_Mtot10pct_k20_no_noise <- process_batch(Mtot_pct = "Mtot10pct", n_seqs = "50seqs", noise = "no_background_noise", H = "H1") # fait
res_all_n100_Mtot10pct_k20_no_noise <- process_batch(Mtot_pct = "Mtot10pct", n_seqs = "100seqs", noise = "no_background_noise", H = "H1") # fait
res_all_n300_Mtot10pct_k20_no_noise <- process_batch(Mtot_pct = "Mtot10pct", n_seqs = "300seqs", noise = "no_background_noise", H = "H1") # fait
## Mtot50pct
res_all_n20_Mtot50pct_k20_no_noise <- process_batch(Mtot_pct = "Mtot50pct", n_seqs = "20seqs", noise = "no_background_noise", H = "H1") # fait
res_all_n50_Mtot50pct_k20_no_noise <- process_batch(Mtot_pct = "Mtot50pct", n_seqs = "50seqs", noise = "no_background_noise", H = "H1") # fait
res_all_n100_Mtot50pct_k20_no_noise <- process_batch(Mtot_pct = "Mtot50pct", n_seqs = "100seqs", noise = "no_background_noise", H = "H1") # fait
res_all_n300_Mtot50pct_k20_no_noise <- process_batch(Mtot_pct = "Mtot50pct", n_seqs = "300seqs", noise = "no_background_noise", H = "H1") # fait


## H0
## Mtot1pct
res_all_n20_Mtot1pct_k20_H0_no_noise <- process_batch(Mtot_pct = "Mtot1pct", n_seqs = "20seqs", noise = "no_background_noise", H = "H0")
res_all_n50_Mtot1pct_k20_H0_no_noise <- process_batch(Mtot_pct = "Mtot1pct", n_seqs = "50seqs", noise = "no_background_noise", H = "H0")
res_all_n100_Mtot1pct_k20_H0_no_noise <- process_batch(Mtot_pct = "Mtot1pct", n_seqs = "100seqs", noise = "no_background_noise", H = "H0")
res_all_n300_Mtot1pct_k20_H0_no_noise <- process_batch(Mtot_pct = "Mtot1pct", n_seqs = "300seqs", noise = "no_background_noise", H = "H0")
## Mtot10pct
res_all_n20_Mtot10pct_k20_H0_no_noise <- process_batch(Mtot_pct = "Mtot10pct", n_seqs = "20seqs", noise = "no_background_noise", H = "H0")
res_all_n50_Mtot10pct_k20_H0_no_noise <- process_batch(Mtot_pct = "Mtot10pct", n_seqs = "50seqs", noise = "no_background_noise", H = "H0")
res_all_n100_Mtot10pct_k20_H0_no_noise <- process_batch(Mtot_pct = "Mtot10pct", n_seqs = "100seqs", noise = "no_background_noise", H = "H0")
res_all_n300_Mtot10pct_k20_H0_no_noise <- process_batch(Mtot_pct = "Mtot10pct", n_seqs = "300seqs", noise = "no_background_noise", H = "H0")
## Mtot50pct
res_all_n20_Mtot50pct_k20_H0_no_noise <- process_batch(Mtot_pct = "Mtot50pct", n_seqs = "20seqs", noise = "no_background_noise", H = "H0")
res_all_n50_Mtot50pct_k20_H0_no_noise <- process_batch(Mtot_pct = "Mtot50pct", n_seqs = "50seqs", noise = "no_background_noise", H = "H0")
res_all_n100_Mtot50pct_k20_H0_no_noise <- process_batch(Mtot_pct = "Mtot50pct", n_seqs = "100seqs", noise = "no_background_noise", H = "H0")
res_all_n300_Mtot50pct_k20_H0_no_noise <- process_batch(Mtot_pct = "Mtot50pct", n_seqs = "300seqs", noise = "no_background_noise", H = "H0")


# Noise
## H1
## Mtot1pct
res_all_n20_Mtot1pct_k20_noise <- process_batch(Mtot_pct = "Mtot1pct", n_seqs = "20seqs", noise = "two_sided_background_noise", H = "H1")
res_all_n50_Mtot1pct_k20_noise <- process_batch(Mtot_pct = "Mtot1pct", n_seqs = "50seqs", noise = "two_sided_background_noise", H = "H1")
res_all_n100_Mtot1pct_k20_noise <- process_batch(Mtot_pct = "Mtot1pct", n_seqs = "100seqs", noise = "two_sided_background_noise", H = "H1")
res_all_n300_Mtot1pct_k20_noise <- process_batch(Mtot_pct = "Mtot1pct", n_seqs = "300seqs", noise = "two_sided_background_noise", H = "H1")
## Mtot10pct
res_all_n20_Mtot10pct_k20_noise <- process_batch(Mtot_pct = "Mtot10pct", n_seqs = "20seqs", noise = "two_sided_background_noise", H = "H1")
res_all_n50_Mtot10pct_k20_noise <- process_batch(Mtot_pct = "Mtot10pct", n_seqs = "50seqs", noise = "two_sided_background_noise", H = "H1")
res_all_n100_Mtot10pct_k20_noise <- process_batch(Mtot_pct = "Mtot10pct", n_seqs = "100seqs", noise = "two_sided_background_noise", H = "H1")
res_all_n300_Mtot10pct_k20_noise <- process_batch(Mtot_pct = "Mtot10pct", n_seqs = "300seqs", noise = "two_sided_background_noise", H = "H1")
## Mtot50pct
res_all_n20_Mtot50pct_k20_noise <- process_batch(Mtot_pct = "Mtot50pct", n_seqs = "20seqs", noise = "two_sided_background_noise", H = "H1")
res_all_n50_Mtot50pct_k20_noise <- process_batch(Mtot_pct = "Mtot50pct", n_seqs = "50seqs", noise = "two_sided_background_noise", H = "H1")
res_all_n100_Mtot50pct_k20_noise <- process_batch(Mtot_pct = "Mtot50pct", n_seqs = "100seqs", noise = "two_sided_background_noise", H = "H1")
res_all_n300_Mtot50pct_k20_noise <- process_batch(Mtot_pct = "Mtot50pct", n_seqs = "300seqs", noise = "two_sided_background_noise", H = "H1")


## H0
## Mtot1pct
res_all_n20_Mtot1pct_k20_H0_noise <- process_batch(Mtot_pct = "Mtot1pct", n_seqs = "20seqs", noise = "two_sided_background_noise", H = "H0")
res_all_n50_Mtot1pct_k20_H0_noise <- process_batch(Mtot_pct = "Mtot1pct", n_seqs = "50seqs", noise = "two_sided_background_noise", H = "H0")
res_all_n100_Mtot1pct_k20_H0_noise <- process_batch(Mtot_pct = "Mtot1pct", n_seqs = "100seqs", noise = "two_sided_background_noise", H = "H0")
res_all_n300_Mtot1pct_k20_H0_noise <- process_batch(Mtot_pct = "Mtot1pct", n_seqs = "300seqs", noise = "two_sided_background_noise", H = "H0")
## Mtot10pct
res_all_n20_Mtot10pct_k20_H0_noise <- process_batch(Mtot_pct = "Mtot10pct", n_seqs = "20seqs", noise = "two_sided_background_noise", H = "H0")
res_all_n50_Mtot10pct_k20_H0_noise <- process_batch(Mtot_pct = "Mtot10pct", n_seqs = "50seqs", noise = "two_sided_background_noise", H = "H0")
res_all_n100_Mtot10pct_k20_H0_noise <- process_batch(Mtot_pct = "Mtot10pct", n_seqs = "100seqs", noise = "two_sided_background_noise", H = "H0")
res_all_n300_Mtot10pct_k20_H0_noise <- process_batch(Mtot_pct = "Mtot10pct", n_seqs = "300seqs", noise = "two_sided_background_noise", H = "H0")
## Mtot50pct
res_all_n20_Mtot50pct_k20_H0_noise <- process_batch(Mtot_pct = "Mtot50pct", n_seqs = "20seqs", noise = "two_sided_background_noise", H = "H0")
res_all_n50_Mtot50pct_k20_H0_noise <- process_batch(Mtot_pct = "Mtot50pct", n_seqs = "50seqs", noise = "two_sided_background_noise", H = "H0")
res_all_n100_Mtot50pct_k20_H0_noise <- process_batch(Mtot_pct = "Mtot50pct", n_seqs = "100seqs", noise = "two_sided_background_noise", H = "H0")
res_all_n300_Mtot50pct_k20_H0_noise <- process_batch(Mtot_pct = "Mtot50pct", n_seqs = "300seqs", noise = "two_sided_background_noise", H = "H0")


save.image(file = "results/results_methods.RData")
