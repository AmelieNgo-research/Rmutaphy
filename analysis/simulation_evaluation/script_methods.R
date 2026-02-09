source(here::here("R/functions_other_statistics.R"))

process_batch <- function(Mtot_pct, n_seqs, k = 20, noise, H) {

  folder <- glue::glue("analysis/results/results_{Mtot_pct}_{noise}_nsimu1000/saves")
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


save.image(file = "analysis/results/results_methods.RData")
