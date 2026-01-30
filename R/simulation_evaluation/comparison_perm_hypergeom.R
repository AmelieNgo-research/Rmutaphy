get_aucpr_h1_testtype <- function(mtot_pct, n,
                                  noise_type = c("no_background_noise", "two_sided_background_noise"),
                                  pvalue_type = c("pvalue", "corr_pvalue"),
                                  test_type = c("permutation", "hypergeometric")) {
  noise_type <- match.arg(noise_type)
  pvalue_type <- match.arg(pvalue_type)
  test_type <- match.arg(test_type)

  base_path <- paste0("results/results_Mtot", mtot_pct, "pct_", noise_type, "_nsimu1000/saves/")

  simu_subtrees_file <- paste0(base_path, "01-simulation_trees_all_mutations_H1_", n, "seqs.txt")
  simu_subtrees <- readLines(simu_subtrees_file)
  simu_subtrees <- lapply(simu_subtrees, function(line) {
    if (nzchar(line)) as.numeric(unlist(strsplit(line, " "))) else numeric(0)
  })

  load(here::here(base_path, paste0("tree_outputs_H0_H1_", n, "seqs.RData")))
  load(here::here(base_path, paste0("01-simulation_trees_H1_", n, "seqs.RData")))

  all_data <- do.call(rbind, lapply(seq_along(list_tree), function(i) {
    tree <- list_tree[[i]]
    true_positives <- simu_subtrees[[i]]
    df_pvals <- tree_outputs[[i]][["mutaphy_h1"]][["subtrees"]][[test_type]]

    get_labels_and_pvals(tree, true_positives, df_pvals,
                         detection = "hierarchical",
                         pvalue_type = pvalue_type)
  }))

  pr <- pr.curve(
    scores.class0 = -all_data$pvalue[all_data$label == 1],
    scores.class1 = -all_data$pvalue[all_data$label == 0],
    curve = FALSE
  )

  return(pr$auc.integral)
}
test_types <- c("hypergeometric", "permutation")
pval_type_map <- c("hypergeometric" = "pvalue", "permutation" = "corr_pvalue")

df_aucpr_h1 <- data.frame()

for (test in test_types) {
  for (noise in noise_types) {
    for (mtot in mtot_list) {
      for (n_val in n_list) {
        cat("H1 → test =", test, ", noise =", noise, ", Mtot =", mtot, ", n =", n_val, "\n")

        pval_type <- pval_type_map[[test]]

        auc_val <- tryCatch({
          get_aucpr_h1_testtype(mtot_pct = mtot,
                                n = n_val,
                                noise_type = noise,
                                pvalue_type = pval_type,
                                test_type = test)
        }, error = function(e) NA)

        df_aucpr_h1 <- rbind(df_aucpr_h1, data.frame(
          Test_type = test,
          Pval_type = pval_type,
          Mtot = mtot,
          n = n_val,
          Noise = noise,
          AUC_PR = auc_val
        ))
      }
    }
  }
}

df_aucpr_h1 <- df_aucpr_h1 %>%
  mutate(
    Mtot = factor(Mtot, levels = c(1, 10, 50), labels = c("1%", "10%", "50%")),
    Noise = recode(Noise, "no_background_noise" = "No noise", "two_sided_background_noise" = "Noise"),
    Test_type = factor(Test_type, levels = c("hypergeometric", "permutation"),
                       labels = c("Hypergeometric", "Permutation")),
    Pval_label = recode(Pval_type,
                        "pvalue" = "Hypergeometric p-value",
                        "corr_pvalue" = "Permutation corrected p-value")
  )

ggplot(df_aucpr_h1, aes(x = n, y = AUC_PR, color = Test_type, linetype = Noise,
                        group = interaction(Test_type, Noise))) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  facet_wrap(~ Mtot, nrow = 1) +
  theme_minimal(base_size = 15) +
  labs(
    title = bquote("AUC-PR under" ~ H[1]),
    x = "Sample size (n)",
    y = "AUC-PR",
    color = "Test type",
    linetype = "Noise"
  ) +
  scale_linetype_manual(values = c("No noise" = "solid", "Noise" = "dashed")) +
  scale_color_manual(values = c("Hypergeometric" = "#009E73", "Permutation" = "#F53850")) +
  ylim(0, 1.05) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 12)
  )



get_specificity_h0_testtype <- function(mtot_pct, n,
                                        noise_type = c("no_background_noise", "two_sided_background_noise"),
                                        pvalue_type = c("pvalue", "corr_pvalue"),
                                        test_type = c("permutation", "hypergeometric"),
                                        alpha = 0.05) {
  noise_type <- match.arg(noise_type)
  pvalue_type <- match.arg(pvalue_type)
  test_type <- match.arg(test_type)

  base_path <- paste0("results/results_Mtot", mtot_pct, "pct_", noise_type, "_nsimu1000/saves/")

  load(here::here(base_path, paste0("tree_outputs_H0_H1_", n, "seqs.RData")))
  load(here::here(base_path, paste0("01-simulation_trees_H0_", n, "seqs.RData")))

  all_data <- do.call(rbind, lapply(seq_along(list_tree), function(i) {
    tree <- list_tree[[i]]
    df_pvals <- tree_outputs[[i]][["mutaphy_h0"]][["subtrees"]][[test_type]]
    get_labels_and_pvals(tree, numeric(0), df_pvals,
                         detection = "hierarchical",
                         pvalue_type = pvalue_type)
  }))

  false_positives <- sum(all_data$pvalue < alpha)
  total_tests <- nrow(all_data)
  specificity <- 1 - (false_positives / total_tests)
  return(specificity)
}

test_types <- c("hypergeometric", "permutation")
pval_type_map <- c("hypergeometric" = "pvalue", "permutation" = "corr_pvalue")

df_spec_h0 <- data.frame()

for (test in test_types) {
  for (noise in noise_types) {
    for (mtot in mtot_list) {
      for (n_val in n_list) {
        cat("H0 → test =", test, ", noise =", noise, ", Mtot =", mtot, ", n =", n_val, "\n")

        pval_type <- pval_type_map[[test]]

        spec_val <- tryCatch({
          get_specificity_h0_testtype(mtot_pct = mtot,
                                      n = n_val,
                                      noise_type = noise,
                                      pvalue_type = pval_type,
                                      test_type = test)
        }, error = function(e) NA)

        df_spec_h0 <- rbind(df_spec_h0, data.frame(
          Test_type = test,
          Pval_type = pval_type,
          Mtot = mtot,
          n = n_val,
          Noise = noise,
          Specificity = spec_val
        ))
      }
    }
  }
}

df_spec_h0 <- df_spec_h0 %>%
  mutate(
    Mtot = factor(Mtot, levels = c(1, 10, 50), labels = c("1%", "10%", "50%")),
    Noise = recode(Noise,
                   "no_background_noise" = "No noise",
                   "two_sided_background_noise" = "Noise"),
    Test_type = factor(Test_type,
                       levels = c("hypergeometric", "permutation"),
                       labels = c("Hypergeometric", "Permutation")),
    Pval_label = recode(Pval_type,
                        "pvalue" = "Hypergeometric p-value",
                        "corr_pvalue" = "Permutation corrected p-value")
  )

ggplot(df_spec_h0, aes(x = n, y = Specificity, color = Test_type, linetype = Noise,
                       group = interaction(Test_type, Noise))) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  facet_wrap(~ Mtot, nrow = 1) +
  theme_minimal(base_size = 15) +
  labs(
    title = bquote("Specificity under" ~ H[0]),
    x = "Sample size (n)",
    y = "Specificity (1 - FPR)",
    color = "Test type",
    linetype = "Noise"
  ) +
  scale_linetype_manual(values = c("No noise" = "solid", "Noise" = "dashed")) +
  scale_color_manual(values = c("Hypergeometric" = "#009E73", "Permutation" = "#F53850")) +
  coord_cartesian(ylim = c(0.97, 1)) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 12)
  )






library(patchwork)


p1 <- ggplot(df_spec_h0, aes(x = n, y = Specificity, color = Test_type, linetype = Noise,
                             group = interaction(Test_type, Noise))) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  facet_wrap(~ Mtot, nrow = 1) +
  theme_minimal(base_size = 15) +
  labs(
    title = bquote("(A) " ~ H[0]),
    x = "Sample size (n)",
    y = "Specificity (1 - FPR)",
    color = "Test type",
    linetype = "Noise"
  ) +
  scale_linetype_manual(values = c("No noise" = "solid", "Noise" = "dashed")) +
  scale_color_manual(values = c("Hypergeometric" = "#009E73", "Permutation" = "#F53850")) +
  coord_cartesian(ylim = c(0.97, 1)) +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold", size = 12)
  )


p2 <- ggplot(df_aucpr_h1, aes(x = n, y = AUC_PR, color = Test_type, linetype = Noise,
                              group = interaction(Test_type, Noise))) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  facet_wrap(~ Mtot, nrow = 1) +
  theme_minimal(base_size = 15) +
  labs(
    title = bquote("(B) " ~ H[1]),
    x = "Sample size (n)",
    y = "AUC-PR",
    color = "Test type",
    linetype = "Noise"
  ) +
  scale_linetype_manual(values = c("No noise" = "solid", "Noise" = "dashed")) +
  scale_color_manual(values = c("Hypergeometric" = "#009E73", "Permutation" = "#F53850")) +
  ylim(0, 1.05) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 12)
  )


combined_plot <- p1 / p2 +
  plot_layout(heights = c(1, 1)) &
  theme(legend.position = "bottom")


print(combined_plot)

