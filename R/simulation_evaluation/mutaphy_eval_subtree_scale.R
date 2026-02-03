# Subtree scale: Specificity (H0) + AUC-PR (H1) raw p-value and corrected p-value
library(pROC); library(dplyr); library(patchwork)
library(PRROC); library(here); library(ggplot2)


#### Subtree scale

get_internal_descendants <- function(tree, node) {
  descendants <- phangorn::Descendants(tree, node, "all")
  internal_descendants <- descendants[descendants > Ntip(tree)] # nolint
  return(internal_descendants)
}

get_labels_and_pvals <- function(tree, true_positives, df_pvals,
                                 detection = c("strict", "hierarchical"),
                                 pvalue_type = c("pvalue", "corr_pvalue")) {
  detection <- match.arg(detection)
  pvalue_type <- match.arg(pvalue_type)

  if (detection == "hierarchical") {
    all_tp_nodes <- unique(c(
      true_positives,
      unlist(lapply(true_positives, function(x) get_internal_descendants(tree, x)))
    ))
  } else {
    all_tp_nodes <- true_positives
  }

  node_ids <- df_pvals$node
  pval <- df_pvals[[pvalue_type]]
  labels <- ifelse(node_ids %in% all_tp_nodes, 1, 0)

  return(data.frame(node = node_ids, pvalue = pval, label = labels))
}


get_aucpr_h1 <- function(mtot_pct, n,
                         detection = c("strict", "hierarchical"),
                         noise_type = c("no_background_noise", "two_sided_background_noise"),
                         pvalue_type = c("pvalue", "corr_pvalue")) {
  detection <- match.arg(detection)
  noise_type <- match.arg(noise_type)
  pvalue_type <- match.arg(pvalue_type)

  base_path <- paste0("results/results_Mtot", mtot_pct, "pct_", noise_type, "_nsimu1000/saves/")

  simu_subtrees_file <- paste0(base_path, "01-simulation_trees_all_mutations_H1_", n, "seqs.txt")
  simu_subtrees <- readLines(simu_subtrees_file)
  simu_subtrees <- lapply(simu_subtrees, function(line) {
    if (nzchar(line)) as.numeric(unlist(strsplit(line, " "))) else numeric(0)
  })

  load(here::here(base_path, paste0("tree_outputs_H0_H1_", n, "seqs.RData")))
  load(here::here(base_path, paste0("01-simulation_trees_H1_", n, "seqs.RData")))

  all_data <- do.call(rbind, lapply(seq_along(list_tree), function(i) { # nolint
    tree <- list_tree[[i]] # nolint
    true_positives <- simu_subtrees[[i]]
    df_pvals <- tree_outputs[[i]][["mutaphy_h1"]][["subtrees"]][["permutation"]] # nolint

    get_labels_and_pvals(tree, true_positives, df_pvals,
                         detection = detection,
                         pvalue_type = pvalue_type)
  }))

  pr <- pr.curve(
    scores.class0 = -all_data$pvalue[all_data$label == 1],
    scores.class1 = -all_data$pvalue[all_data$label == 0],
    curve = FALSE
  )

  return(pr$auc.integral)
}

get_aucpr_h0 <- function(mtot_pct, n,
                         detection = c("strict", "hierarchical"),
                         noise_type = c("no_background_noise", "two_sided_background_noise"),
                         pvalue_type = c("pvalue", "corr_pvalue")) {
  detection <- match.arg(detection)
  noise_type <- match.arg(noise_type)
  pvalue_type <- match.arg(pvalue_type)

  base_path <- paste0("results/results_Mtot", mtot_pct, "pct_", noise_type, "_nsimu1000/saves/")

  load(here::here(base_path, paste0("tree_outputs_H0_H1_", n, "seqs.RData")))
  load(here::here(base_path, paste0("01-simulation_trees_H0_", n, "seqs.RData")))

  all_data <- do.call(rbind, lapply(seq_along(list_tree), function(i) { # nolint
    tree <- list_tree[[i]] # nolint
    true_positives <- numeric(0)
    df_pvals <- tree_outputs[[i]][["mutaphy_h0"]][["subtrees"]][["permutation"]] # nolint

    get_labels_and_pvals(tree, true_positives, df_pvals,
                         detection = detection,
                         pvalue_type = pvalue_type)
  }))

  pr <- pr.curve(
    scores.class0 = -all_data$pvalue[all_data$label == 1],
    scores.class1 = -all_data$pvalue[all_data$label == 0],
    curve = FALSE
  )

  return(pr$auc.integral)
}


mtot_list <- c(1, 10, 50)
n_list <- c(20, 50, 100, 300)
detection_list <- c("strict", "hierarchical")
noise_types <- c("no_background_noise", "two_sided_background_noise")

pvalue_types <- c("pvalue", "corr_pvalue")
df_aucpr_h1 <- data.frame()
df_aucpr_h0 <- data.frame()


for (pval_type in pvalue_types) {
  for (noise in noise_types) {
    for (mtot in mtot_list) {
      for (n_val in n_list) {
        for (detect in detection_list) {
          cat("H1 → pval =", pval_type, ", noise =", noise, ", Mtot =", mtot, ", n =", n_val, ", detection =", detect, "\n")
          auc_val <- tryCatch({
            get_aucpr_h1(
              mtot_pct = mtot,
              n = n_val,
              detection = detect,
              noise_type = noise,
              pvalue_type = pval_type
            )
          }, error = function(e) NA)

          df_aucpr_h1 <- rbind(df_aucpr_h1, data.frame(
            Pval_type = pval_type,
            Mtot = mtot,
            n = n_val,
            Detection = detect,
            Noise = noise,
            AUC_PR = auc_val
          ))

          cat("H0 → pval =", pval_type, ", noise =", noise, ", Mtot =", mtot, ", n =", n_val, ", detection =", detect, "\n")
          auc_val <- tryCatch({
            get_aucpr_h0(
              mtot_pct = mtot,
              n = n_val,
              detection = detect,
              noise_type = noise,
              pvalue_type = pval_type
            )
          }, error = function(e) NA)

          df_aucpr_h0 <- rbind(df_aucpr_h0, data.frame(
            Pval_type = pval_type,
            Mtot = mtot,
            n = n_val,
            Detection = detect,
            Noise = noise,
            AUC_PR = auc_val
          ))
        }
      }
    }
  }
}

df_aucpr_h1$H <- "H1"
df_aucpr_h0$H <- "H0"


df_aucpr_all <- rbind(df_aucpr_h1, df_aucpr_h0)


df_aucpr_all$Mtot <- factor(df_aucpr_all$Mtot, levels = c(1, 10, 50), labels = c("1%", "10%", "50%"))


df_aucpr_all$Pval_type <- factor(df_aucpr_all$Pval_type,
                                 levels = c("pvalue", "corr_pvalue"),
                                 labels = c("Raw p-value", "Corrected p-value"))


df_aucpr_all$Noise <- recode(df_aucpr_all$Noise,
                             "no_background_noise" = "No noise",
                             "two_sided_background_noise" = "Noise")


df_aucpr_all$FacetLabel <- paste0(df_aucpr_all$H, " – Mtot = ", df_aucpr_all$Mtot, " – ", df_aucpr_all$Pval_type)


# ggplot(df_aucpr_all, aes(x = n, y = AUC_PR, color = Detection, linetype = Noise)) +
#   geom_point(size = 3) +
#   geom_line(size = 1) +
#   facet_wrap(~ FacetLabel, nrow = 3) +
#   theme_minimal(base_size = 15) +
#   labs(
#     x = "Sample size (n)",
#     y = "AUC-PR",
#     color = "Detection",
#     linetype = "Noise"
#   ) +
#   scale_linetype_manual(values = c("No noise" = "solid", "Noise" = "dashed")) +
#   scale_color_manual(values = c("hierarchical" = "#D55E00", "strict" = "#56B4E9")) +
#   ylim(0, 1.05) +
#   theme(
#     legend.position = "bottom",
#     strip.text = element_text(face = "bold", size = 12)
#   )


#### specificity = 1 - fp de mutaphy
get_specificity_h0 <- function(mtot_pct, n,
                               detection = c("strict", "hierarchical"),
                               noise_type = c("no_background_noise", "two_sided_background_noise"),
                               pvalue_type = c("pvalue", "corr_pvalue"),
                               alpha = 0.05) {
  detection <- match.arg(detection)
  noise_type <- match.arg(noise_type)
  pvalue_type <- match.arg(pvalue_type)

  base_path <- paste0("results/results_Mtot", mtot_pct, "pct_", noise_type, "_nsimu1000/saves/")

  load(here::here(base_path, paste0("tree_outputs_H0_H1_", n, "seqs.RData")))
  load(here::here(base_path, paste0("01-simulation_trees_H0_", n, "seqs.RData")))

  all_data <- do.call(rbind, lapply(seq_along(list_tree), function(i) { # nolint
    tree <- list_tree[[i]] # nolint
    df_pvals <- tree_outputs[[i]][["mutaphy_h0"]][["subtrees"]][["permutation"]] # nolint
    get_labels_and_pvals(tree, numeric(0), df_pvals, detection = detection, pvalue_type = pvalue_type)
  }))

  false_positives <- sum(all_data$pvalue < alpha)
  total_tests <- nrow(all_data)
  specificity <- 1 - (false_positives / total_tests)
  return(specificity)
}

df_spec_h0 <- data.frame()
df_aucpr_h1 <- data.frame()
pvalue_types <- c("pvalue", "corr_pvalue")

for (pval_type in pvalue_types) {
  for (noise in noise_types) {
    for (mtot in mtot_list) {
      for (n_val in n_list) {
        for (detect in detection_list) {
          cat("H0 →", pval_type, ", noise =", noise, ", Mtot =", mtot, ", n =", n_val, ", detection =", detect, "\n")
          spec_val <- tryCatch({
            get_specificity_h0(mtot_pct = mtot, n = n_val,
                               detection = detect, noise_type = noise,
                               pvalue_type = pval_type)
          }, error = function(e) NA)

          df_spec_h0 <- rbind(df_spec_h0, data.frame(
            Pval_type = pval_type,
            Mtot = mtot,
            n = n_val,
            Detection = detect,
            Noise = noise,
            Specificity = spec_val
          ))

          cat("H1 →", pval_type, ", noise =", noise, ", Mtot =", mtot, ", n =", n_val, ", detection =", detect, "\n")
          auc_val <- tryCatch({
            get_aucpr_h1(mtot_pct = mtot, n = n_val,
                         detection = detect, noise_type = noise,
                         pvalue_type = pval_type)
          }, error = function(e) NA)

          df_aucpr_h1 <- rbind(df_aucpr_h1, data.frame(
            Pval_type = pval_type,
            Mtot = mtot,
            n = n_val,
            Detection = detect,
            Noise = noise,
            AUC_PR = auc_val
          ))
        }
      }
    }
  }
}


df_spec_h0$H <- "H0"
df_aucpr_h1$H <- "H1"

df_combined <- bind_rows(
  df_spec_h0 %>% rename(Value = Specificity),
  df_aucpr_h1 %>% rename(Value = AUC_PR)
)


df_combined <- df_combined %>%
  mutate(
    Mtot = factor(Mtot, levels = c(1, 10, 50), labels = c("1%", "10%", "50%")),
    Noise = recode(Noise, "no_background_noise" = "No noise", "two_sided_background_noise" = "Noise"),
    Pval_type = factor(Pval_type, levels = c("pvalue", "corr_pvalue"),
                       labels = c("Raw p-value", "Corrected p-value")),
    Detection = factor(Detection, levels = c("hierarchical", "strict"))
  )




for (pval_label in levels(df_combined$Pval_type)) {
  df_plot <- df_combined %>% filter(Pval_type == pval_label)

  p_spec <- df_plot %>% filter(H == "H0") %>%
    ggplot(aes(x = n, y = Value, color = Detection, linetype = Noise)) +
    geom_point(size = 3) +
    geom_line(size = 1) +
    facet_wrap(~ Mtot, nrow = 1) +
    theme_minimal(base_size = 15) +
    labs(
      title = bquote("(A) " ~ H[0]),
      x = "Sample size (n)",
      y = "Specificity (1 - FPR)"
    ) +
    scale_linetype_manual(values = c("No noise" = "solid", "Noise" = "dashed")) +
    coord_cartesian(ylim = c(0.97, 1)) +
    theme(legend.position = "none")

  p_aucpr <- df_plot %>% filter(H == "H1") %>%
    ggplot(aes(x = n, y = Value, color = Detection, linetype = Noise)) +
    geom_point(size = 3) +
    geom_line(size = 1) +
    facet_wrap(~ Mtot, nrow = 1) +
    theme_minimal(base_size = 15) +
    labs(
      title = bquote("(B) " ~ H[1]),
      x = "Sample size (n)",
      y = "AUC-PR",
      color = "Detection",
      linetype = "Noise"
    ) +
    scale_linetype_manual(values = c("No noise" = "solid", "Noise" = "dashed")) +
    ylim(0, 1.05) +
    theme(
      legend.position = "bottom",
      legend.box = "vertical",
      legend.key.width = unit(2, "cm"),
      legend.key.height = unit(0.4, "cm")
    ) +
    guides(
      color = guide_legend(override.aes = list(size = 2)),
      linetype = guide_legend(override.aes = list(size = 2))
    )

  combined_plot <- p_spec / p_aucpr + plot_annotation(title = paste("Results using", pval_label))
  print(combined_plot)
}
