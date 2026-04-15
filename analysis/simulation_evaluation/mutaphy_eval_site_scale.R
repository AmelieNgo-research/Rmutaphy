library(ggplot2)
library(dplyr)
library(RColorBrewer)

##### Site level detection

evaluate_site_detection <- function(i, results_tree_list, site_output, Mtot_prop) {
  all_mutations <- results_tree_list[[i]][["simulation"]][["all_mutations"]]

  nb_mutations_total <- nrow(all_mutations)
  nb_causal <- ceiling(Mtot_prop * nb_mutations_total)

  causal_positions <- all_mutations$Position[1:nb_causal]

  detected_positions <- unlist(site_output[[i]])

  TP <- length(intersect(causal_positions, detected_positions))
  FP <- length(setdiff(detected_positions, causal_positions))
  FN <- length(setdiff(causal_positions, detected_positions))

  precision <- ifelse((TP + FP) == 0, NA, TP / (TP + FP))
  recall <- ifelse((TP + FN) == 0, NA, TP / (TP + FN))

  return(data.frame(simulation = i,
                    TP = TP, FP = FP, FN = FN,
                    precision = precision,
                    recall = recall))
}

############## no noise
### Mtot1pct

## n20
# load(here::here("analysis/results/results_Mtot1pct_no_background_noise_nsimu1000/saves/01-simulation_trees_H1_20seqs.RData"))
# load(here::here("analysis/results/results_Mtot1pct_no_background_noise_nsimu1000/saves/tree_outputs_H0_H1_20seqs.RData"))
# load(here::here("analysis/results/results_Mtot1pct_no_background_noise_nsimu1000/saves/site_outputs_H0_H1_20seqs.RData"))
#
# site_n20_Mtot1pct_no_noise <- do.call(rbind, lapply(seq_along(results_tree_list), function(i) {
#   evaluate_site_detection(i, results_tree_list, site_output, Mtot_prop = 0.01)
# }))
#
# ## n50
# load(here::here("analysis/results/results_Mtot1pct_no_background_noise_nsimu1000/saves/01-simulation_trees_H1_50seqs.RData"))
# load(here::here("analysis/results/results_Mtot1pct_no_background_noise_nsimu1000/saves/tree_outputs_H0_H1_50seqs.RData"))
# load(here::here("analysis/results/results_Mtot1pct_no_background_noise_nsimu1000/saves/site_outputs_H0_H1_50seqs.RData"))
#
# site_n50_Mtot1pct_no_noise <- do.call(rbind, lapply(seq_along(results_tree_list), function(i) {
#   evaluate_site_detection(i, results_tree_list, site_output, Mtot_prop = 0.01)
# }))
#
# ## n100
# load(here::here("analysis/results/results_Mtot1pct_no_background_noise_nsimu1000/saves/01-simulation_trees_H1_100seqs.RData"))
# load(here::here("analysis/results/results_Mtot1pct_no_background_noise_nsimu1000/saves/tree_outputs_H0_H1_100seqs.RData"))
# load(here::here("analysis/results/results_Mtot1pct_no_background_noise_nsimu1000/saves/site_outputs_H0_H1_100seqs.RData"))
#
# site_n100_Mtot1pct_no_noise <- do.call(rbind, lapply(seq_along(results_tree_list), function(i) {
#   evaluate_site_detection(i, results_tree_list, site_output, Mtot_prop = 0.01)
# }))
#
# ## n300
# load(here::here("analysis/results/results_Mtot1pct_no_background_noise_nsimu1000/saves/01-simulation_trees_H1_300seqs.RData"))
# load(here::here("analysis/results/results_Mtot1pct_no_background_noise_nsimu1000/saves/tree_outputs_H0_H1_300seqs.RData"))
# load(here::here("analysis/results/results_Mtot1pct_no_background_noise_nsimu1000/saves/site_outputs_H0_H1_300seqs.RData"))
#
# site_n300_Mtot1pct_no_noise <- do.call(rbind, lapply(seq_along(results_tree_list), function(i) {
#   evaluate_site_detection(i, results_tree_list, site_output, Mtot_prop = 0.01)
# }))
#
# ### Mtot10pct
#
# ## n20
# load(here::here("analysis/results/results_Mtot10pct_no_background_noise_nsimu1000/saves/01-simulation_trees_H1_20seqs.RData"))
# load(here::here("analysis/results/results_Mtot10pct_no_background_noise_nsimu1000/saves/tree_outputs_H0_H1_20seqs.RData"))
# load(here::here("analysis/results/results_Mtot10pct_no_background_noise_nsimu1000/saves/site_outputs_H0_H1_20seqs.RData"))
#
# site_n20_Mtot10pct_no_noise <- do.call(rbind, lapply(seq_along(results_tree_list), function(i) {
#   evaluate_site_detection(i, results_tree_list, site_output, Mtot_prop = 0.1)
# }))
#
# ## n50
# load(here::here("analysis/results/results_Mtot10pct_no_background_noise_nsimu1000/saves/01-simulation_trees_H1_50seqs.RData"))
# load(here::here("analysis/results/results_Mtot10pct_no_background_noise_nsimu1000/saves/tree_outputs_H0_H1_50seqs.RData"))
# load(here::here("analysis/results/results_Mtot10pct_no_background_noise_nsimu1000/saves/site_outputs_H0_H1_50seqs.RData"))
#
# site_n50_Mtot10pct_no_noise <- do.call(rbind, lapply(seq_along(results_tree_list), function(i) {
#   evaluate_site_detection(i, results_tree_list, site_output, Mtot_prop = 0.1)
# }))
#
# ## n100
# load(here::here("analysis/results/results_Mtot10pct_no_background_noise_nsimu1000/saves/01-simulation_trees_H1_100seqs.RData"))
# load(here::here("analysis/results/results_Mtot10pct_no_background_noise_nsimu1000/saves/tree_outputs_H0_H1_100seqs.RData"))
# load(here::here("analysis/results/results_Mtot10pct_no_background_noise_nsimu1000/saves/site_outputs_H0_H1_100seqs.RData"))
#
# site_n100_Mtot10pct_no_noise <- do.call(rbind, lapply(seq_along(results_tree_list), function(i) {
#   evaluate_site_detection(i, results_tree_list, site_output, Mtot_prop = 0.1)
# }))
#
# ## n300
# load(here::here("analysis/results/results_Mtot10pct_no_background_noise_nsimu1000/saves/01-simulation_trees_H1_300seqs.RData"))
# load(here::here("analysis/results/results_Mtot10pct_no_background_noise_nsimu1000/saves/tree_outputs_H0_H1_300seqs.RData"))
# load(here::here("analysis/results/results_Mtot10pct_no_background_noise_nsimu1000/saves/site_outputs_H0_H1_300seqs.RData"))
#
# site_n300_Mtot10pct_no_noise <- do.call(rbind, lapply(seq_along(results_tree_list), function(i) {
#   evaluate_site_detection(i, results_tree_list, site_output, Mtot_prop = 0.1)
# }))
#
#
# ### Mtot50pct
#
# ## n20
# load(here::here("analysis/results/results_Mtot50pct_no_background_noise_nsimu1000/saves/01-simulation_trees_H1_20seqs.RData"))
# load(here::here("analysis/results/results_Mtot50pct_no_background_noise_nsimu1000/saves/tree_outputs_H0_H1_20seqs.RData"))
# load(here::here("analysis/results/results_Mtot50pct_no_background_noise_nsimu1000/saves/site_outputs_H0_H1_20seqs.RData"))
#
# site_n20_Mtot50pct_no_noise <- do.call(rbind, lapply(seq_along(results_tree_list), function(i) {
#   evaluate_site_detection(i, results_tree_list, site_output, Mtot_prop = 0.5)
# }))
#
# ## n50
# load(here::here("analysis/results/results_Mtot50pct_no_background_noise_nsimu1000/saves/01-simulation_trees_H1_50seqs.RData"))
# load(here::here("analysis/results/results_Mtot50pct_no_background_noise_nsimu1000/saves/tree_outputs_H0_H1_50seqs.RData"))
# load(here::here("analysis/results/results_Mtot50pct_no_background_noise_nsimu1000/saves/site_outputs_H0_H1_50seqs.RData"))
#
# site_n50_Mtot50pct_no_noise <- do.call(rbind, lapply(seq_along(results_tree_list), function(i) {
#   evaluate_site_detection(i, results_tree_list, site_output, Mtot_prop = 0.5)
# }))
#
# ## n100
# load(here::here("analysis/results/results_Mtot50pct_no_background_noise_nsimu1000/saves/01-simulation_trees_H1_100seqs.RData"))
# load(here::here("analysis/results/results_Mtot50pct_no_background_noise_nsimu1000/saves/tree_outputs_H0_H1_100seqs.RData"))
# load(here::here("analysis/results/results_Mtot50pct_no_background_noise_nsimu1000/saves/site_outputs_H0_H1_100seqs.RData"))
#
# site_n100_Mtot50pct_no_noise <- do.call(rbind, lapply(seq_along(results_tree_list), function(i) {
#   evaluate_site_detection(i, results_tree_list, site_output, Mtot_prop = 0.5)
# }))
#
# ## n300
# load(here::here("analysis/results/results_Mtot50pct_no_background_noise_nsimu1000/saves/01-simulation_trees_H1_300seqs.RData"))
# load(here::here("analysis/results/results_Mtot50pct_no_background_noise_nsimu1000/saves/tree_outputs_H0_H1_300seqs.RData"))
# load(here::here("analysis/results/results_Mtot50pct_no_background_noise_nsimu1000/saves/site_outputs_H0_H1_300seqs.RData"))
#
# site_n300_Mtot50pct_no_noise <- do.call(rbind, lapply(seq_along(results_tree_list), function(i) {
#   evaluate_site_detection(i, results_tree_list, site_output, Mtot_prop = 0.5)
# }))
#
# ############## noise
# ### Mtot1pct
#
# ## n20
# load(here::here("analysis/results/results_Mtot1pct_two_sided_background_noise_nsimu1000/saves/01-simulation_trees_H1_20seqs.RData"))
# load(here::here("analysis/results/results_Mtot1pct_two_sided_background_noise_nsimu1000/saves/tree_outputs_H0_H1_20seqs.RData"))
# load(here::here("analysis/results/results_Mtot1pct_two_sided_background_noise_nsimu1000/saves/site_outputs_H0_H1_20seqs.RData"))
#
# site_n20_Mtot1pct_noise <- do.call(rbind, lapply(seq_along(results_tree_list), function(i) {
#   evaluate_site_detection(i, results_tree_list, site_output, Mtot_prop = 0.01)
# }))
#
# ## n50
# load(here::here("analysis/results/results_Mtot1pct_two_sided_background_noise_nsimu1000/saves/01-simulation_trees_H1_50seqs.RData"))
# load(here::here("analysis/results/results_Mtot1pct_two_sided_background_noise_nsimu1000/saves/tree_outputs_H0_H1_50seqs.RData"))
# load(here::here("analysis/results/results_Mtot1pct_two_sided_background_noise_nsimu1000/saves/site_outputs_H0_H1_50seqs.RData"))
#
# site_n50_Mtot1pct_noise <- do.call(rbind, lapply(seq_along(results_tree_list), function(i) {
#   evaluate_site_detection(i, results_tree_list, site_output, Mtot_prop = 0.01)
# }))
#
# ## n100
# load(here::here("analysis/results/results_Mtot1pct_two_sided_background_noise_nsimu1000/saves/01-simulation_trees_H1_100seqs.RData"))
# load(here::here("analysis/results/results_Mtot1pct_two_sided_background_noise_nsimu1000/saves/tree_outputs_H0_H1_100seqs.RData"))
# load(here::here("analysis/results/results_Mtot1pct_two_sided_background_noise_nsimu1000/saves/site_outputs_H0_H1_100seqs.RData"))
#
# site_n100_Mtot1pct_noise <- do.call(rbind, lapply(seq_along(results_tree_list), function(i) {
#   evaluate_site_detection(i, results_tree_list, site_output, Mtot_prop = 0.01)
# }))
#
# ## n300
# load(here::here("analysis/results/results_Mtot1pct_two_sided_background_noise_nsimu1000/saves/01-simulation_trees_H1_300seqs.RData"))
# load(here::here("analysis/results/results_Mtot1pct_two_sided_background_noise_nsimu1000/saves/tree_outputs_H0_H1_300seqs.RData"))
# load(here::here("analysis/results/results_Mtot1pct_two_sided_background_noise_nsimu1000/saves/site_outputs_H0_H1_300seqs.RData"))
#
# site_n300_Mtot1pct_noise <- do.call(rbind, lapply(seq_along(results_tree_list), function(i) {
#   evaluate_site_detection(i, results_tree_list, site_output, Mtot_prop = 0.01)
# }))
#
# ### Mtot10pct
#
# ## n20
# load(here::here("analysis/results/results_Mtot10pct_two_sided_background_noise_nsimu1000/saves/01-simulation_trees_H1_20seqs.RData"))
# load(here::here("analysis/results/results_Mtot10pct_two_sided_background_noise_nsimu1000/saves/tree_outputs_H0_H1_20seqs.RData"))
# load(here::here("analysis/results/results_Mtot10pct_two_sided_background_noise_nsimu1000/saves/site_outputs_H0_H1_20seqs.RData"))
#
# site_n20_Mtot10pct_noise <- do.call(rbind, lapply(seq_along(results_tree_list), function(i) {
#   evaluate_site_detection(i, results_tree_list, site_output, Mtot_prop = 0.1)
# }))
#
# ## n50
# load(here::here("analysis/results/results_Mtot10pct_two_sided_background_noise_nsimu1000/saves/01-simulation_trees_H1_50seqs.RData"))
# load(here::here("analysis/results/results_Mtot10pct_two_sided_background_noise_nsimu1000/saves/tree_outputs_H0_H1_50seqs.RData"))
# load(here::here("analysis/results/results_Mtot10pct_two_sided_background_noise_nsimu1000/saves/site_outputs_H0_H1_50seqs.RData"))
#
# site_n50_Mtot10pct_noise <- do.call(rbind, lapply(seq_along(results_tree_list), function(i) {
#   evaluate_site_detection(i, results_tree_list, site_output, Mtot_prop = 0.1)
# }))
#
# ## n100
# load(here::here("analysis/results/results_Mtot10pct_two_sided_background_noise_nsimu1000/saves/01-simulation_trees_H1_100seqs.RData"))
# load(here::here("analysis/results/results_Mtot10pct_two_sided_background_noise_nsimu1000/saves/tree_outputs_H0_H1_100seqs.RData"))
# load(here::here("analysis/results/results_Mtot10pct_two_sided_background_noise_nsimu1000/saves/site_outputs_H0_H1_100seqs.RData"))
#
# site_n100_Mtot10pct_noise <- do.call(rbind, lapply(seq_along(results_tree_list), function(i) {
#   evaluate_site_detection(i, results_tree_list, site_output, Mtot_prop = 0.1)
# }))
#
# ## n300
# load(here::here("analysis/results/results_Mtot10pct_two_sided_background_noise_nsimu1000/saves/01-simulation_trees_H1_300seqs.RData"))
# load(here::here("analysis/results/results_Mtot10pct_two_sided_background_noise_nsimu1000/saves/tree_outputs_H0_H1_300seqs.RData"))
# load(here::here("analysis/results/results_Mtot10pct_two_sided_background_noise_nsimu1000/saves/site_outputs_H0_H1_300seqs.RData"))
#
# site_n300_Mtot10pct_noise <- do.call(rbind, lapply(seq_along(results_tree_list), function(i) {
#   evaluate_site_detection(i, results_tree_list, site_output, Mtot_prop = 0.1)
# }))
#
#
# ### Mtot50pct
#
# ## n20
# load(here::here("analysis/results/results_Mtot50pct_two_sided_background_noise_nsimu1000/saves/01-simulation_trees_H1_20seqs.RData"))
# load(here::here("analysis/results/results_Mtot50pct_two_sided_background_noise_nsimu1000/saves/tree_outputs_H0_H1_20seqs.RData"))
# load(here::here("analysis/results/results_Mtot50pct_two_sided_background_noise_nsimu1000/saves/site_outputs_H0_H1_20seqs.RData"))
#
# site_n20_Mtot50pct_noise <- do.call(rbind, lapply(seq_along(results_tree_list), function(i) {
#   evaluate_site_detection(i, results_tree_list, site_output, Mtot_prop = 0.5)
# }))
#
# ## n50
# load(here::here("analysis/results/results_Mtot50pct_two_sided_background_noise_nsimu1000/saves/01-simulation_trees_H1_50seqs.RData"))
# load(here::here("analysis/results/results_Mtot50pct_two_sided_background_noise_nsimu1000/saves/tree_outputs_H0_H1_50seqs.RData"))
# load(here::here("analysis/results/results_Mtot50pct_two_sided_background_noise_nsimu1000/saves/site_outputs_H0_H1_50seqs.RData"))
#
# site_n50_Mtot50pct_noise <- do.call(rbind, lapply(seq_along(results_tree_list), function(i) {
#   evaluate_site_detection(i, results_tree_list, site_output, Mtot_prop = 0.5)
# }))
#
# ## n100
# load(here::here("analysis/results/results_Mtot50pct_two_sided_background_noise_nsimu1000/saves/01-simulation_trees_H1_100seqs.RData"))
# load(here::here("analysis/results/results_Mtot50pct_two_sided_background_noise_nsimu1000/saves/tree_outputs_H0_H1_100seqs.RData"))
# load(here::here("analysis/results/results_Mtot50pct_two_sided_background_noise_nsimu1000/saves/site_outputs_H0_H1_100seqs.RData"))
#
# site_n100_Mtot50pct_noise <- do.call(rbind, lapply(seq_along(results_tree_list), function(i) {
#   evaluate_site_detection(i, results_tree_list, site_output, Mtot_prop = 0.5)
# }))
#
# ## n300
# load(here::here("analysis/results/results_Mtot50pct_two_sided_background_noise_nsimu1000/saves/01-simulation_trees_H1_300seqs.RData"))
# load(here::here("analysis/results/results_Mtot50pct_two_sided_background_noise_nsimu1000/saves/tree_outputs_H0_H1_300seqs.RData"))
# load(here::here("analysis/results/results_Mtot50pct_two_sided_background_noise_nsimu1000/saves/site_outputs_H0_H1_300seqs.RData"))
#
# site_n300_Mtot50pct_noise <- do.call(rbind, lapply(seq_along(results_tree_list), function(i) {
#   evaluate_site_detection(i, results_tree_list, site_output, Mtot_prop = 0.5)
# }))
#
#
#
# site_all <- dplyr::bind_rows(
#   site_n20_Mtot1pct_no_noise %>% mutate(Mtot = "1%", Noise = "no noise", n = 20),
#   site_n50_Mtot1pct_no_noise %>% mutate(Mtot = "1%", Noise = "no noise", n = 50),
#   site_n100_Mtot1pct_no_noise %>% mutate(Mtot = "1%", Noise = "no noise", n = 100),
#   site_n300_Mtot1pct_no_noise %>% mutate(Mtot = "1%", Noise = "no noise", n = 300),
#
#   site_n20_Mtot10pct_no_noise %>% mutate(Mtot = "10%", Noise = "no noise", n = 20),
#   site_n50_Mtot10pct_no_noise %>% mutate(Mtot = "10%", Noise = "no noise", n = 50),
#   site_n100_Mtot10pct_no_noise %>% mutate(Mtot = "10%", Noise = "no noise", n = 100),
#   site_n300_Mtot10pct_no_noise %>% mutate(Mtot = "10%", Noise = "no noise", n = 300),
#
#   site_n20_Mtot50pct_no_noise %>% mutate(Mtot = "50%", Noise = "no noise", n = 20),
#   site_n50_Mtot50pct_no_noise %>% mutate(Mtot = "50%", Noise = "no noise", n = 50),
#   site_n100_Mtot50pct_no_noise %>% mutate(Mtot = "50%", Noise = "no noise", n = 100),
#   site_n300_Mtot50pct_no_noise %>% mutate(Mtot = "50%", Noise = "no noise", n = 300),
#
#   site_n20_Mtot1pct_noise %>% mutate(Mtot = "1%", Noise = "noise", n = 20),
#   site_n50_Mtot1pct_noise %>% mutate(Mtot = "1%", Noise = "noise", n = 50),
#   site_n100_Mtot1pct_noise %>% mutate(Mtot = "1%", Noise = "noise", n = 100),
#   site_n300_Mtot1pct_noise %>% mutate(Mtot = "1%", Noise = "noise", n = 300),
#
#   site_n20_Mtot10pct_noise %>% mutate(Mtot = "10%", Noise = "noise", n = 20),
#   site_n50_Mtot10pct_noise %>% mutate(Mtot = "10%", Noise = "noise", n = 50),
#   site_n100_Mtot10pct_noise %>% mutate(Mtot = "10%", Noise = "noise", n = 100),
#   site_n300_Mtot10pct_noise %>% mutate(Mtot = "10%", Noise = "noise", n = 300),
#
#   site_n20_Mtot50pct_noise %>% mutate(Mtot = "50%", Noise = "noise", n = 20),
#   site_n50_Mtot50pct_noise %>% mutate(Mtot = "50%", Noise = "noise", n = 50),
#   site_n100_Mtot50pct_noise %>% mutate(Mtot = "50%", Noise = "noise", n = 100),
#   site_n300_Mtot50pct_noise %>% mutate(Mtot = "50%", Noise = "noise", n = 300)
# )
#
# site_all <- site_all %>% filter(!is.na(precision), !is.na(recall))
#
#
# cols <- brewer.pal(n = 3, name = "Set2")[1:2]

# ggplot(site_all, aes(x = factor(n), y = precision, fill = Noise)) +
#   geom_boxplot(alpha = 0.8, outlier.shape = NA, width = 0.6, color = "black") +
#   geom_jitter(width = 0.2, alpha = 0.15, color = "black", size = 0.5) +
#   facet_wrap(~ Mtot, labeller = label_bquote("Mtot: "*.(Mtot)*"")) +
#   scale_fill_manual(values = cols, labels = c("No noise", "Noise")) +
#   labs(
#     x = "Sample size (n)",
#     y = "Precision",
#     fill = "Noise"
#   ) +
#   theme_bw(base_size = 13) +
#   theme(
#     plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
#     axis.title = element_text(face = "bold"),
#     legend.position = "top",
#     strip.background = element_rect(fill = "grey90", color = NA),
#     strip.text = element_text(face = "bold")
#   )

# ggplot(site_all, aes(x = factor(n), y = recall, fill = Noise)) +
#   geom_boxplot(alpha = 0.8, outlier.shape = NA, width = 0.6, color = "black") +
#   geom_jitter(width = 0.1, alpha = 0.15, color = "black", size = 0.5) +
#   facet_wrap(~ Mtot, labeller = label_bquote("Mtot: "*.(Mtot)*"%")) +
#   scale_fill_manual(values = cols, labels = c("No noise", "Noise")) +
#   labs( x = "Sample size (n)", y = "Recall", fill = "Noise" ) +
#   theme_bw(base_size = 13) +
#   theme( plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
#          axis.title = element_text(face = "bold"), legend.position = "top",
#          strip.background = element_rect(fill = "grey90", color = NA),
#          strip.text = element_text(face = "bold")
#   )


# Precision
site_precision <- ggplot(site_all, aes(x = factor(n), y = precision, fill = Noise)) +

  geom_boxplot(
    data = site_all,
    alpha = 0.8, outlier.shape = NA, width = 0.6, color = "black",
    position = position_dodge(width = 0.7)
  ) +

  geom_point(
    aes(color = Noise),
    position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.7),
    alpha = 0.2, size = 2, show.legend = FALSE
  ) +

  facet_wrap(~ Mtot, labeller = label_bquote("Mtot: "*.(Mtot)), drop = TRUE) +
  scale_fill_manual(values = cols, labels = c("No noise", "Noise")) +
  scale_color_manual(values = cols, labels = c("No noise", "Noise")) +
  labs(x = "Sample size (n)", y = "Precision", fill = "Noise") +
  theme_bw(base_size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.title = element_text(face = "bold"),
    legend.position = "top",
    strip.background = element_rect(fill = "grey90", color = NA),
    legend.title = element_text(size = 22, face = "bold"),
    legend.text  = element_text(size = 20),
    legend.key.size = unit(0.9, "cm"),
    legend.key.width = unit(0.9, "cm"),
    legend.key.height = unit(0.9, "cm"),
    strip.text = element_text(size = 18, face = "bold"),
    axis.title.x = element_text(size = 20, face = "bold"),
    axis.title.y = element_text(size = 20, face = "bold"),
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16)
  )
site_precision

# Recall
ggplot(site_all, aes(x = factor(n), y = recall, fill = Noise)) +

  geom_boxplot(
    data = subset(site_all, Mtot != "1%"),
    alpha = 0.8, outlier.shape = NA, width = 0.6, color = "black",
    position = position_dodge(width = 0.7)
  ) +

  geom_point(
    aes(color = Noise),
    position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.7),
    alpha = 0.2, size = 2, show.legend = FALSE
  ) +

  facet_wrap(~ Mtot, labeller = label_bquote("Mtot: "*.(Mtot)), drop = TRUE) +
  scale_fill_manual(values = cols, labels = c("No noise", "Noise")) +
  scale_color_manual(values = cols, labels = c("No noise", "Noise")) +
  labs(x = "Sample size (n)", y = "Recall", fill = "Noise") +
  theme_bw(base_size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.title = element_text(face = "bold"),
    legend.position = "top",
    strip.background = element_rect(fill = "grey90", color = NA),
    legend.title = element_text(size = 22, face = "bold"),
    legend.text  = element_text(size = 20),
    legend.key.size = unit(0.9, "cm"),
    legend.key.width = unit(0.9, "cm"),
    legend.key.height = unit(0.9, "cm"),
    strip.text = element_text(size = 18, face = "bold"),
    axis.title.x = element_text(size = 20, face = "bold"),
    axis.title.y = element_text(size = 20, face = "bold"),
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16)
  )



site_mtot1 <- site_all %>%
  filter(Mtot == "1%") %>%
  group_by(n, Noise, Mtot) %>%
  summarise(prop_detected = mean(recall), .groups = "drop")

site_recall <- ggplot() +

  geom_boxplot(
    data = subset(site_all, Mtot != "1%"),
    aes(x = factor(n), y = recall, fill = Noise),
    alpha = 0.8, outlier.shape = NA, width = 0.6, color = "black",
    position = position_dodge(width = 0.7)
  ) +

  geom_point(
    data = subset(site_all, Mtot != "1%"),
    aes(x = factor(n), y = recall, color = Noise),
    position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.7),
    alpha = 0.2, size = 2, show.legend = FALSE
  ) +

  geom_col(
    data = site_mtot1,
    aes(x = factor(n), y = prop_detected, fill = Noise),
    position = position_dodge(width = 0.7),
    width = 0.6, color = "black", alpha = 0.8
  ) +

  facet_wrap(~ Mtot, labeller = label_bquote("Mtot: "*.(Mtot))) +

  scale_fill_manual(values = cols, labels = c("No noise", "Noise")) +
  scale_color_manual(values = cols, guide = "none") +

  labs(
    x = "Sample size (n)",
    y = "Recall / Detection proportion",
    fill = "Noise"
  ) +

  theme_bw(base_size = 13) +
  theme(
    axis.title = element_text(face = "bold"),
    legend.position = "top",
    strip.background = element_rect(fill = "grey90", color = NA),
    legend.title = element_text(size = 22, face = "bold"),
    legend.text  = element_text(size = 20),
    legend.key.size = unit(0.9, "cm"),
    legend.key.width = unit(0.9, "cm"),
    legend.key.height = unit(0.9, "cm"),
    strip.text = element_text(size = 18, face = "bold"),
    axis.title.x = element_text(size = 20, face = "bold"),
    axis.title.y = element_text(size = 20, face = "bold"),
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16)
  )

site_recall



#=============
#= GWAS seul
#=============

test_nt_position <- function(sequences, phenotype, pos, min_count = 1) {

  nt_vec <- sapply(sequences, `[`, pos)
  nt_vec <- toupper(nt_vec)

  valid_nt <- !is.na(nt_vec) & !nt_vec %in% c("-", "N")
  nt_vec <- nt_vec[valid_nt]
  pheno  <- phenotype[names(nt_vec)]

  valid_pheno <- !is.na(pheno)
  nt_vec <- nt_vec[valid_pheno]
  pheno  <- pheno[valid_pheno]

  if (length(unique(pheno)) < 2) return(NULL)

  nts_present <- sort(unique(nt_vec))
  nts_present <- nts_present[nts_present %in% c("A", "C", "G", "T")]

  pheno_levels <- unique(pheno)
  if (length(pheno_levels) != 2) return(NULL)

  group1 <- pheno_levels[1]
  group2 <- pheno_levels[2]

  res <- list()
  k <- 1

  for (nt in nts_present) {

    mut <- nt_vec == nt

    if (sum(mut) < min_count) next
    if (sum(!mut) < min_count) next

    tab <- table(mut, pheno)

    if (nrow(tab) < 2 || ncol(tab) < 2) next

    ft <- fisher.test(tab)

    n_nt_group1 <- sum(mut & pheno == group1, na.rm = TRUE)
    n_nt_group2 <- sum(mut & pheno == group2, na.rm = TRUE)
    n_other_group1 <- sum(!mut & pheno == group1, na.rm = TRUE)
    n_other_group2 <- sum(!mut & pheno == group2, na.rm = TRUE)

    res[[k]] <- data.frame(
      position = pos,
      nt_tested = nt,
      n_nt = sum(mut),
      n_other = sum(!mut),
      group1 = group1,
      group2 = group2,
      n_nt_group1 = n_nt_group1,
      n_nt_group2 = n_nt_group2,
      n_other_group1 = n_other_group1,
      n_other_group2 = n_other_group2,
      p_value = ft$p.value,
      odds_ratio = if (!is.null(ft$estimate)) unname(ft$estimate) else NA_real_,
      stringsAsFactors = FALSE
    )
    k <- k + 1
  }

  if (length(res) == 0) return(NULL)
  do.call(rbind, res)
}
run_gwas_nt_restricted <- function(sequences, phenotype, candidate_positions,
                                   min_count = 1, verbose = TRUE) {

  candidate_positions <- sort(unique(na.omit(candidate_positions)))

  results_list <- vector("list", length(candidate_positions))

  for (i in seq_along(candidate_positions)) {
    pos <- candidate_positions[i]

    if (verbose && i %% 50 == 0) {
      cat("Position", i, "/", length(candidate_positions), "- nt pos =", pos, "\n")
    }

    results_list[[i]] <- test_nt_position(
      sequences = sequences,
      phenotype = phenotype,
      pos = pos,
      min_count = min_count
    )
  }

  res <- do.call(rbind, results_list)

  if (is.null(res) || nrow(res) == 0) return(NULL)

  res$p_bonf <- p.adjust(res$p_value, method = "bonferroni")
  res$p_fdr  <- p.adjust(res$p_value, method = "fdr")

  res <- res[order(res$p_value), ]
  rownames(res) <- NULL

  res
}
############## no noise
### Mtot1pct

## n100
load(here::here("analysis/results/results_Mtot50pct_two_sided_background_noise_nsimu1000/saves/01-simulation_trees_H1_20seqs.RData"))
load(here::here("analysis/results/results_Mtot50pct_two_sided_background_noise_nsimu1000/saves/tree_outputs_H0_H1_20seqs.RData"))
load(here::here("analysis/results/results_Mtot50pct_two_sided_background_noise_nsimu1000/saves/site_outputs_H0_H1_20seqs.RData"))

## global
gwas_simu_global_output_Mtot50pct_noise <- list()

for (i in seq_along(results_tree_list)) {
  sequences <- results_tree_list[[i]][["simulation"]][["sequences"]]
  phenotype <- list_tree[[i]][["tip.label"]]
  names(phenotype) <- names(sequences)

  gwas_simu_global_output_Mtot50pct_noise[[i]] <- run_gwas_nt_restricted(
    sequences = sequences,
    phenotype = phenotype,
    candidate_positions = seq_len(length(sequences[[1]])),
    min_count = 1,
    verbose = FALSE
  )
}

## Mutaphy + GWAS
gwas_mutaphy_output_Mtot50pct_noise <- vector("list", length(results_tree_list))

for (i in seq_along(results_tree_list)) {

  sequences <- results_tree_list[[i]][["simulation"]][["sequences"]]
  phenotype <- list_tree[[i]][["tip.label"]]
  names(phenotype) <- names(sequences)

  if (is.null(site_output[[i]]) || length(site_output[[i]]) == 0) {
    gwas_mutaphy_output_Mtot50pct_noise[i] <- list(NULL)
    next
  }

  candidate_positions <- unique(as.numeric(unlist(site_output[[i]])))

  if (length(candidate_positions) == 0) {
    gwas_mutaphy_output_Mtot50pct_noise[i] <- list(NULL)
    next
  }

  gwas_mutaphy_output_Mtot50pct_noise[[i]] <- run_gwas_nt_restricted(
    sequences = sequences,
    phenotype = phenotype,
    candidate_positions = candidate_positions,
    min_count = 1,
    verbose = FALSE
  )
}


#####
evaluate_gwas_positions <- function(i, results_tree_list, gwas_output,
                                    Mtot_prop, threshold = 0.10) {

  all_mutations <- results_tree_list[[i]][["simulation"]][["all_mutations"]]

  nb_mutations_total <- nrow(all_mutations)
  nb_causal <- ceiling(Mtot_prop * nb_mutations_total)

  causal_positions <- unique(all_mutations$Position[1:nb_causal])

  gwas_res <- gwas_output[[i]]

  if (is.null(gwas_res) || nrow(gwas_res) == 0) {
    detected_positions <- c()
  } else {
    gwas_sig <- gwas_res[gwas_res$p_fdr < threshold, , drop = FALSE]
    detected_positions <- unique(gwas_sig$position)
  }

  TP <- length(intersect(causal_positions, detected_positions))
  FP <- length(setdiff(detected_positions, causal_positions))
  FN <- length(setdiff(causal_positions, detected_positions))

  data.frame(
    simulation = i,
    TP = TP,
    FP = FP,
    FN = FN
  )
}

run_one_setting_gwas <- function(Mtot_label,
                                 Mtot_prop,
                                 noise_label,
                                 noise_folder,
                                 n_seqs,
                                 min_count_global = 3,
                                 min_count_mutaphy = 3,
                                 threshold = 0.10) {

  base_path <- here::here(
    paste0("analysis/results/results_Mtot", Mtot_label, "_", noise_folder, "_nsimu1000/saves")
  )

  load(file.path(base_path, paste0("01-simulation_trees_H1_", n_seqs, "seqs.RData")))
  load(file.path(base_path, paste0("tree_outputs_H0_H1_", n_seqs, "seqs.RData")))
  load(file.path(base_path, paste0("site_outputs_H0_H1_", n_seqs, "seqs.RData")))

  ## -----------------------------
  ## GWAS global
  ## -----------------------------
  gwas_global_output <- vector("list", length(results_tree_list))

  for (i in seq_along(results_tree_list)) {
    sequences <- results_tree_list[[i]][["simulation"]][["sequences"]]
    phenotype <- list_tree[[i]][["tip.label"]]
    names(phenotype) <- names(sequences)

    gwas_global_output[[i]] <- run_gwas_nt_restricted(
      sequences = sequences,
      phenotype = phenotype,
      candidate_positions = seq_len(length(sequences[[1]])),
      min_count = min_count_global,
      verbose = FALSE
    )
  }

  gwas_global_eval <- do.call(
    rbind,
    lapply(seq_along(results_tree_list), function(i) {
      evaluate_gwas_positions(
        i = i,
        results_tree_list = results_tree_list,
        gwas_output = gwas_global_output,
        Mtot_prop = Mtot_prop,
        threshold = threshold
      )
    })
  )

  gwas_global_eval <- gwas_global_eval %>%
    dplyr::mutate(
      Mtot = Mtot_label,
      Noise = noise_label,
      n = n_seqs,
      Method = "GWAS global"
    )

  ## -----------------------------
  ## MutaPhy + GWAS
  ## -----------------------------
  gwas_mutaphy_output <- lapply(seq_along(results_tree_list), function(i) {

    sequences <- results_tree_list[[i]][["simulation"]][["sequences"]]
    phenotype <- list_tree[[i]][["tip.label"]]
    names(phenotype) <- names(sequences)

    if (is.null(site_output[[i]]) || length(site_output[[i]]) == 0) {
      return(NULL)
    }

    candidate_positions <- unique(as.numeric(unlist(site_output[[i]])))

    if (length(candidate_positions) == 0) {
      return(NULL)
    }

    run_gwas_nt_restricted(
      sequences = sequences,
      phenotype = phenotype,
      candidate_positions = candidate_positions,
      min_count = min_count_mutaphy,
      verbose = FALSE
    )
  })

  gwas_mutaphy_eval <- do.call(
    rbind,
    lapply(seq_along(results_tree_list), function(i) {
      evaluate_gwas_positions(
        i = i,
        results_tree_list = results_tree_list,
        gwas_output = gwas_mutaphy_output,
        Mtot_prop = Mtot_prop,
        threshold = threshold
      )
    })
  )

  gwas_mutaphy_eval <- gwas_mutaphy_eval %>%
    dplyr::mutate(
      Mtot = Mtot_label,
      Noise = noise_label,
      n = n_seqs,
      Method = "MutaPhy + GWAS"
    )

  dplyr::bind_rows(gwas_global_eval, gwas_mutaphy_eval)
}

Mtot_grid <- data.frame(
  Mtot_label = c("1pct", "10pct", "50pct"),
  Mtot_prop  = c(0.01, 0.10, 0.50),
  stringsAsFactors = FALSE
)

noise_grid <- data.frame(
  noise_label  = c("no noise", "two-sided background noise"),
  noise_folder = c("no_background_noise", "two_sided_background_noise"),
  stringsAsFactors = FALSE
)

n_grid <- c(20, 50, 100, 300)


simu_all_results_list_gwas_mutaphy <- list()
k <- 1

for (m in seq_len(nrow(Mtot_grid))) {
  for (z in seq_len(nrow(noise_grid))) {
    for (n_seqs in n_grid) {

      cat("Running:",
          "Mtot =", Mtot_grid$Mtot_label[m],
          "| Noise =", noise_grid$noise_folder[z],
          "| n =", n_seqs, "\n")

      simu_all_results_list_gwas_mutaphy[[k]] <- run_one_setting_gwas(
        Mtot_label   = Mtot_grid$Mtot_label[m],
        Mtot_prop    = Mtot_grid$Mtot_prop[m],
        noise_label  = noise_grid$noise_label[z],
        noise_folder = noise_grid$noise_folder[z],
        n_seqs       = n_seqs,
        min_count_global = 3,
        min_count_mutaphy = 3,
        threshold = 0.05
      )

      k <- k + 1
    }
  }
}



simu_all_results_gwas_mutaphy <- dplyr::bind_rows(simu_all_results_list_gwas_mutaphy)

summary_gwas_mutaphy <- simu_all_results_gwas_mutaphy %>%
  dplyr::select(simulation, Mtot, Noise, n, Method, TP, FP) %>%
  tidyr::pivot_wider(
    names_from = Method,
    values_from = c(TP, FP)
  )

comparison <- simu_all_results_gwas_mutaphy %>%
  dplyr::select(simulation, Mtot, Noise, n, Method, TP, FP) %>%
  tidyr::pivot_wider(
    names_from = Method,
    values_from = c(TP, FP)
  )

comparison <- comparison %>%
  dplyr::mutate(
    FP_reduction = ifelse(`FP_GWAS global` == 0, NA,
                          (`FP_GWAS global` - `FP_MutaPhy + GWAS`))
  )

comparison <- comparison %>%
  dplyr::mutate(
    precision_gwas = ifelse(
      (`TP_GWAS global` + `FP_GWAS global`) == 0,
      NA,
      `TP_GWAS global` / (`TP_GWAS global` + `FP_GWAS global`)
    ),
    precision_mutaphy = ifelse(
      (`TP_MutaPhy + GWAS` + `FP_MutaPhy + GWAS`) == 0,
      NA,
      `TP_MutaPhy + GWAS` / (`TP_MutaPhy + GWAS` + `FP_MutaPhy + GWAS`)
    )
  )

summary_metrics <- comparison %>%
  dplyr::group_by(Mtot, Noise, n) %>%
  dplyr::summarise(
    mean_FP_gwas = mean(`FP_GWAS global`, na.rm = TRUE),
    mean_FP_mutaphy = mean(`FP_MutaPhy + GWAS`, na.rm = TRUE),
    mean_TP_gwas = mean(`TP_GWAS global`, na.rm = TRUE),
    mean_TP_mutaphy = mean(`TP_MutaPhy + GWAS`, na.rm = TRUE),
    mean_precision_gwas = mean(precision_gwas, na.rm = TRUE),
    mean_precision_mutaphy = mean(precision_mutaphy, na.rm = TRUE),
    .groups = "drop"
  )

summary_metrics <- summary_metrics %>%
  dplyr::mutate(
    Noise = dplyr::case_when(
      Noise == "no noise" ~ "No noise",
      Noise == "two-sided background noise" ~ "Noise"
    ),
    Mtot = dplyr::case_when(
      Mtot == "1pct" ~ "Mtot: 1%",
      Mtot == "10pct" ~ "Mtot: 10%",
      Mtot == "50pct" ~ "Mtot: 50%"
    )
  )

## plot 1
plot_fp <- summary_metrics %>%
  dplyr::select(Mtot, Noise, n, mean_FP_gwas, mean_FP_mutaphy) %>%
  tidyr::pivot_longer(
    cols = c(mean_FP_gwas, mean_FP_mutaphy),
    names_to = "Method",
    values_to = "Value"
  ) %>%
  dplyr::mutate(
    Method = dplyr::case_when(
      Method == "mean_FP_gwas" ~ "GWAS global",
      Method == "mean_FP_mutaphy" ~ "MutaPhy + GWAS"
    ),
    Metric = "False positives"
  )

plot_tp <- summary_metrics %>%
  dplyr::select(Mtot, Noise, n, mean_TP_gwas, mean_TP_mutaphy) %>%
  tidyr::pivot_longer(
    cols = c(mean_TP_gwas, mean_TP_mutaphy),
    names_to = "Method",
    values_to = "Value"
  ) %>%
  dplyr::mutate(
    Method = dplyr::case_when(
      Method == "mean_TP_gwas" ~ "GWAS global",
      Method == "mean_TP_mutaphy" ~ "MutaPhy + GWAS"
    ),
    Metric = "True positives"
  )

plot_precision <- summary_metrics %>%
  dplyr::select(Mtot, Noise, n, mean_precision_gwas, mean_precision_mutaphy) %>%
  tidyr::pivot_longer(
    cols = c(mean_precision_gwas, mean_precision_mutaphy),
    names_to = "Method",
    values_to = "Value"
  ) %>%
  dplyr::mutate(
    Method = dplyr::case_when(
      Method == "mean_precision_gwas" ~ "GWAS global",
      Method == "mean_precision_mutaphy" ~ "MutaPhy + GWAS"
    ),
    Metric = "Precision"
  )

plot_all <- dplyr::bind_rows(plot_fp, plot_tp, plot_precision)

plot_all <- plot_all %>%
  dplyr::mutate(
    Metric = factor(Metric,
                    levels = c("False positives", "True positives", "Precision"))
  )



library(ggplot2)

ggplot(plot_all, aes(x = factor(n), y = Value, fill = Method)) +
  geom_col(position = "dodge", width = 0.7) +
  facet_grid(Metric + Noise ~ Mtot, scales = "free_y") +
  labs(
    x = "Number of sequences",
    y = NULL,
    fill = "Method"
  ) +
  scale_fill_manual(
    values = c(
      "GWAS global" = "#F8766D",
      "MutaPhy + GWAS" = "#00BFC4"
    )
  ) +
  theme_bw() +
  theme(
    strip.background = element_rect(fill = "grey90"),
    strip.text = element_text(face = "bold"),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    panel.grid.major = element_line(color = "grey85"),
    panel.grid.minor = element_blank()
  )


## plot 2
plot_fp <- summary_metrics %>%
  dplyr::select(Mtot, Noise, n, mean_FP_gwas, mean_FP_mutaphy) %>%
  tidyr::pivot_longer(
    cols = c(mean_FP_gwas, mean_FP_mutaphy),
    names_to = "Method",
    values_to = "Value"
  ) %>%
  dplyr::mutate(
    Method = dplyr::case_when(
      Method == "mean_FP_gwas" ~ "GWAS global",
      Method == "mean_FP_mutaphy" ~ "MutaPhy + GWAS"
    ),
    Metric = "False positives"
  )

plot_tp <- summary_metrics %>%
  dplyr::select(Mtot, Noise, n, mean_TP_gwas, mean_TP_mutaphy) %>%
  tidyr::pivot_longer(
    cols = c(mean_TP_gwas, mean_TP_mutaphy),
    names_to = "Method",
    values_to = "Value"
  ) %>%
  dplyr::mutate(
    Method = dplyr::case_when(
      Method == "mean_TP_gwas" ~ "GWAS global",
      Method == "mean_TP_mutaphy" ~ "MutaPhy + GWAS"
    ),
    Metric = "True positives"
  )

plot_precision <- summary_metrics %>%
  dplyr::select(Mtot, Noise, n, mean_precision_gwas, mean_precision_mutaphy) %>%
  tidyr::pivot_longer(
    cols = c(mean_precision_gwas, mean_precision_mutaphy),
    names_to = "Method",
    values_to = "Value"
  ) %>%
  dplyr::mutate(
    Method = dplyr::case_when(
      Method == "mean_precision_gwas" ~ "GWAS global",
      Method == "mean_precision_mutaphy" ~ "MutaPhy + GWAS"
    ),
    Metric = "Precision"
  )

plot_all <- dplyr::bind_rows(plot_fp, plot_tp, plot_precision)

plot_all <- plot_all %>%
  dplyr::mutate(
    Metric = factor(
      Metric,
      levels = c("False positives", "True positives", "Precision")
    ),
    Fill_group = dplyr::case_when(
      Method == "GWAS global" & Noise == "No noise" ~ "GWAS global - No noise",
      Method == "GWAS global" & Noise == "Noise" ~ "GWAS global - Noise",
      Method == "MutaPhy + GWAS" & Noise == "No noise" ~ "MutaPhy + Fisher Test - No noise",
      Method == "MutaPhy + GWAS" & Noise == "Noise" ~ "MutaPhy + Fisher Test - Noise"
    )
  )

ggplot(plot_all, aes(x = factor(n), y = Value, fill = Fill_group)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  facet_grid(Metric ~ Mtot, scales = "free_y") +
  labs(
    x = "Number of sequences",
    y = NULL,
    fill = NULL
  ) +
  scale_fill_manual(
    values = c(
      "GWAS global - No noise" = "#B22222",
      "GWAS global - Noise" = "#F4A6A6",
      "MutaPhy + Fisher Test - No noise" = "#1F4E79",
      "MutaPhy + Fisher Test - Noise" = "#9CC3E6"
    )
  ) +
  theme_bw() +
  theme(
    strip.background = element_rect(fill = "grey90"),
    strip.text = element_text(face = "bold"),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    panel.grid.major = element_line(color = "grey85"),
    panel.grid.minor = element_blank()
  )

### plot 3

library(dplyr)
library(tidyr)

summary_metrics_pct <- summary_metrics %>%
  dplyr::mutate(
    n_causal = dplyr::case_when(
      Mtot == "Mtot: 1%"  ~ 1,
      Mtot == "Mtot: 10%" ~ 10,
      Mtot == "Mtot: 50%" ~ 50,
      TRUE ~ NA_real_
    ),
    n_noncausal = 100 - n_causal,

    FP_gwas_pct = 100 * mean_FP_gwas / n_noncausal,
    FP_mutaphy_pct = 100 * mean_FP_mutaphy / n_noncausal,

    TP_gwas_pct = 100 * mean_TP_gwas / n_causal,
    TP_mutaphy_pct = 100 * mean_TP_mutaphy / n_causal,

    precision_gwas_pct = 100 * mean_precision_gwas,
    precision_mutaphy_pct = 100 * mean_precision_mutaphy
  )

plot_fp <- summary_metrics_pct %>%
  dplyr::select(Mtot, Noise, n, FP_gwas_pct, FP_mutaphy_pct) %>%
  tidyr::pivot_longer(
    cols = c(FP_gwas_pct, FP_mutaphy_pct),
    names_to = "Method",
    values_to = "Value"
  ) %>%
  dplyr::mutate(
    Method = dplyr::case_when(
      Method == "FP_gwas_pct" ~ "GWAS global",
      Method == "FP_mutaphy_pct" ~ "MutaPhy + GWAS"
    ),
    Metric = "False positives (%)"
  )

plot_tp <- summary_metrics_pct %>%
  dplyr::select(Mtot, Noise, n, TP_gwas_pct, TP_mutaphy_pct) %>%
  tidyr::pivot_longer(
    cols = c(TP_gwas_pct, TP_mutaphy_pct),
    names_to = "Method",
    values_to = "Value"
  ) %>%
  dplyr::mutate(
    Method = dplyr::case_when(
      Method == "TP_gwas_pct" ~ "GWAS global",
      Method == "TP_mutaphy_pct" ~ "MutaPhy + GWAS"
    ),
    Metric = "True positives (%)"
  )

plot_precision <- summary_metrics_pct %>%
  dplyr::select(Mtot, Noise, n, precision_gwas_pct, precision_mutaphy_pct) %>%
  tidyr::pivot_longer(
    cols = c(precision_gwas_pct, precision_mutaphy_pct),
    names_to = "Method",
    values_to = "Value"
  ) %>%
  dplyr::mutate(
    Method = dplyr::case_when(
      Method == "precision_gwas_pct" ~ "GWAS global",
      Method == "precision_mutaphy_pct" ~ "MutaPhy + GWAS"
    ),
    Metric = "Precision (%)"
  )
plot_all <- dplyr::bind_rows(plot_fp, plot_tp, plot_precision)

plot_all <- plot_all %>%
  dplyr::mutate(
    Metric = factor(
      Metric,
      levels = c("False positives (%)", "True positives (%)", "Precision (%)")
    ),
    Fill_group = dplyr::case_when(
      Method == "GWAS global" & Noise == "No noise" ~ "GWAS global - No noise",
      Method == "GWAS global" & Noise == "Noise" ~ "GWAS global - Noise",
      Method == "MutaPhy + GWAS" & Noise == "No noise" ~ "MutaPhy + Fisher Test - No noise",
      Method == "MutaPhy + GWAS" & Noise == "Noise" ~ "MutaPhy + Fisher Test - Noise"
    )
  )

ggplot(plot_all, aes(x = factor(n), y = Value, fill = Fill_group)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  facet_grid(Metric ~ Mtot, scales = "free_y") +
  labs(
    x = "Number of sequences",
    y = "Percentage",
    fill = NULL
  ) +
  scale_fill_manual(
    values = c(
      "GWAS global - No noise" = "#B22222",
      "GWAS global - Noise" = "#F4A6A6",
      "MutaPhy + Fisher Test - No noise" = "#1F4E79",
      "MutaPhy + Fisher Test - Noise" = "#9CC3E6"
    )
  ) +
  theme_bw() +
  theme(
    strip.background = element_rect(fill = "grey90"),
    strip.text = element_text(face = "bold"),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    panel.grid.major = element_line(color = "grey85"),
    panel.grid.minor = element_blank()
  )

### plot 4

summary_metrics_pct <- summary_metrics %>%
  dplyr::filter(Noise == "No noise") %>%
  dplyr::mutate(
    n_causal = dplyr::case_when(
      Mtot == "Mtot: 1%"  ~ 1,
      Mtot == "Mtot: 10%" ~ 10,
      Mtot == "Mtot: 50%" ~ 50,
      TRUE ~ NA_real_
    ),
    n_noncausal = 100 - n_causal,

    FP_gwas_pct = 100 * mean_FP_gwas / n_noncausal,
    FP_mutaphy_pct = 100 * mean_FP_mutaphy / n_noncausal,

    TP_gwas_pct = 100 * mean_TP_gwas / n_causal,
    TP_mutaphy_pct = 100 * mean_TP_mutaphy / n_causal,

    precision_gwas_pct = 100 * mean_precision_gwas,
    precision_mutaphy_pct = 100 * mean_precision_mutaphy
  )

plot_fp <- summary_metrics_pct %>%
  dplyr::select(Mtot, n, FP_gwas_pct, FP_mutaphy_pct) %>%
  tidyr::pivot_longer(
    cols = c(FP_gwas_pct, FP_mutaphy_pct),
    names_to = "Method",
    values_to = "Value"
  ) %>%
  dplyr::mutate(
    Method = dplyr::case_when(
      Method == "FP_gwas_pct" ~ "GWAS global",
      Method == "FP_mutaphy_pct" ~ "MutaPhy + Fisher test"
    ),
    Metric = "False positives (%)"
  )

plot_tp <- summary_metrics_pct %>%
  dplyr::select(Mtot, n, TP_gwas_pct, TP_mutaphy_pct) %>%
  tidyr::pivot_longer(
    cols = c(TP_gwas_pct, TP_mutaphy_pct),
    names_to = "Method",
    values_to = "Value"
  ) %>%
  dplyr::mutate(
    Method = dplyr::case_when(
      Method == "TP_gwas_pct" ~ "GWAS global",
      Method == "TP_mutaphy_pct" ~ "MutaPhy + Fisher test"
    ),
    Metric = "True positives (%)"
  )

plot_precision <- summary_metrics_pct %>%
  dplyr::select(Mtot, n, precision_gwas_pct, precision_mutaphy_pct) %>%
  tidyr::pivot_longer(
    cols = c(precision_gwas_pct, precision_mutaphy_pct),
    names_to = "Method",
    values_to = "Value"
  ) %>%
  dplyr::mutate(
    Method = dplyr::case_when(
      Method == "precision_gwas_pct" ~ "GWAS global",
      Method == "precision_mutaphy_pct" ~ "MutaPhy + Fisher test"
    ),
    Metric = "Precision (%)"
  )

plot_all <- dplyr::bind_rows(plot_fp, plot_tp, plot_precision) %>%
  dplyr::mutate(
    Metric = factor(
      Metric,
      levels = c("False positives (%)", "True positives (%)", "Precision (%)")
    )
  )

ggplot(plot_all, aes(x = factor(n), y = Value, fill = Method)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  facet_grid(Metric ~ Mtot, scales = "free_y") +
  labs(
    x = "Number of sequences",
    y = "Percentage",
    fill = NULL
  ) +
  scale_fill_manual(
    values = c(
      "GWAS global" = "#F94449",
      "MutaPhy + Fisher test" = "#00B4D8"
    )
  ) +
  theme_bw() +
  theme(
    strip.background = element_rect(fill = "grey90"),
    legend.position = "right",
    axis.title = element_text(size = 12),
    panel.grid.major = element_line(color = "grey85"),
    panel.grid.minor = element_blank(),
    legend.title = element_text(size = 22, face = "bold"),
    legend.text  = element_text(size = 16),
    legend.key.size = unit(0.9, "cm"),
    legend.key.width = unit(0.9, "cm"),
    legend.key.height = unit(0.9, "cm"),
    strip.text = element_text(size = 14, face = "bold"),
    axis.title.x = element_text(size = 18, face = "bold"),
    axis.title.y = element_text(size = 18, face = "bold"),
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13)
  )

##### n = 20, 50, 100, 300
# -50% de faux positifs
total_FP_gwas <- sum(comparison$`FP_GWAS global`)
total_FP_mutaphy <- sum(comparison$`FP_MutaPhy + GWAS`)
100 * (total_FP_gwas - total_FP_mutaphy) / total_FP_gwas #[1] 49.96282
total_FP_gwas
total_FP_mutaphy
nrow(comparison)

# conserve 63% des vrais positifs identifiés par le GWAS global
total_TP_gwas <- sum(comparison$`TP_GWAS global`)
total_TP_mutaphy <- sum(comparison$`TP_MutaPhy + GWAS`)
total_TP_gwas
total_TP_mutaphy

total_TP_mutaphy / total_TP_gwas
(total_TP_mutaphy / total_TP_gwas)*100

# +3 points de précisoon
precision_gwas = total_TP_gwas / (total_TP_gwas + total_FP_gwas)
precision_mutaphy = total_TP_mutaphy / (total_TP_mutaphy + total_FP_mutaphy)
precision_gwas*100
precision_mutaphy*100

##### n = 300
comparison_n300 <- subset(comparison, n == 300)


####
# - % de faux positifs
total_FP_gwas_n300 <- sum(comparison_n300$`FP_GWAS global`)
total_FP_mutaphy_n300 <- sum(comparison_n300$`FP_MutaPhy + GWAS`)

100 * (total_FP_gwas_n300 - total_FP_mutaphy_n300) / total_FP_gwas_n300

total_FP_gwas_n300
total_FP_mutaphy_n300
nrow(comparison_n300)

# précision
total_TP_gwas_n300 <- sum(comparison_n300$`TP_GWAS global`)
total_TP_mutaphy_n300 <- sum(comparison_n300$`TP_MutaPhy + GWAS`)

precision_gwas_n300 <- total_TP_gwas_n300 / (total_TP_gwas_n300 + total_FP_gwas_n300)
precision_mutaphy_n300 <- total_TP_mutaphy_n300 / (total_TP_mutaphy_n300 + total_FP_mutaphy_n300)

precision_gwas_n300 * 100
precision_mutaphy_n300 * 100

# % de vrais positifs conservés
total_TP_gwas_n300
total_TP_mutaphy_n300

total_TP_mutaphy_n300 / total_TP_gwas_n300
(total_TP_mutaphy_n300 / total_TP_gwas_n300) * 100


sum(comparison %>%
      dplyr::filter(
        Noise == "two-sided background noise",
        n == 20,
        Mtot == "50pct"
        ) %>%
      dplyr::pull(FP_reduction), na.rm = TRUE)

sum(comparison %>%
      dplyr::filter(
        Noise == "two-sided background noise",
        n == 20,
        Mtot == "50pct"
      ) %>%
      dplyr::pull(`FP_GWAS global`), na.rm = TRUE)



View(comparison %>%
  dplyr::filter(
    Noise == "two-sided background noise",
    n == 100,
    Mtot == "10pct"
  ))



sum(comparison %>%
      dplyr::filter(
        n == 20,
        Mtot == "50pct"
      ) %>%
      dplyr::pull(FP_reduction), na.rm = TRUE)

sum(comparison %>%
      dplyr::filter(
        Noise == "two-sided background noise",
        n == 20,
        Mtot == "50pct"
      ) %>%
      dplyr::pull(`FP_GWAS global`), na.rm = TRUE)


total_FP_gwas <- sum(comparison$`FP_GWAS global`)
total_FP_mutaphy <- sum(comparison$`FP_MutaPhy + GWAS`)

100 * (total_FP_gwas - total_FP_mutaphy) / total_FP_gwas
