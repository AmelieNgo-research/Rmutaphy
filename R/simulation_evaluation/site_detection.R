library(ggplot2)
library(dplyr)

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
load(here::here("results/results_Mtot1pct_no_background_noise_nsimu1000/saves/01-simulation_trees_H1_20seqs.RData"))
load(here::here("results/results_Mtot1pct_no_background_noise_nsimu1000/saves/tree_outputs_H0_H1_20seqs.RData"))
load(here::here("results/results_Mtot1pct_no_background_noise_nsimu1000/saves/site_outputs_H0_H1_20seqs.RData"))

site_n20_Mtot1pct_no_noise <- do.call(rbind, lapply(seq_along(results_tree_list), function(i) {
  evaluate_site_detection(i, results_tree_list, site_output, Mtot_prop = 0.01)
}))

## n50
load(here::here("results/results_Mtot1pct_no_background_noise_nsimu1000/saves/01-simulation_trees_H1_50seqs.RData"))
load(here::here("results/results_Mtot1pct_no_background_noise_nsimu1000/saves/tree_outputs_H0_H1_50seqs.RData"))
load(here::here("results/results_Mtot1pct_no_background_noise_nsimu1000/saves/site_outputs_H0_H1_50seqs.RData"))

site_n50_Mtot1pct_no_noise <- do.call(rbind, lapply(seq_along(results_tree_list), function(i) {
  evaluate_site_detection(i, results_tree_list, site_output, Mtot_prop = 0.01)
}))

## n100
load(here::here("results/results_Mtot1pct_no_background_noise_nsimu1000/saves/01-simulation_trees_H1_100seqs.RData"))
load(here::here("results/results_Mtot1pct_no_background_noise_nsimu1000/saves/tree_outputs_H0_H1_100seqs.RData"))
load(here::here("results/results_Mtot1pct_no_background_noise_nsimu1000/saves/site_outputs_H0_H1_100seqs.RData"))

site_n100_Mtot1pct_no_noise <- do.call(rbind, lapply(seq_along(results_tree_list), function(i) {
  evaluate_site_detection(i, results_tree_list, site_output, Mtot_prop = 0.01)
}))

## n300
load(here::here("results/results_Mtot1pct_no_background_noise_nsimu1000/saves/01-simulation_trees_H1_300seqs.RData"))
load(here::here("results/results_Mtot1pct_no_background_noise_nsimu1000/saves/tree_outputs_H0_H1_300seqs.RData"))
load(here::here("results/results_Mtot1pct_no_background_noise_nsimu1000/saves/site_outputs_H0_H1_300seqs.RData"))

site_n300_Mtot1pct_no_noise <- do.call(rbind, lapply(seq_along(results_tree_list), function(i) {
  evaluate_site_detection(i, results_tree_list, site_output, Mtot_prop = 0.01)
}))

### Mtot10pct

## n20
load(here::here("results/results_Mtot10pct_no_background_noise_nsimu1000/saves/01-simulation_trees_H1_20seqs.RData"))
load(here::here("results/results_Mtot10pct_no_background_noise_nsimu1000/saves/tree_outputs_H0_H1_20seqs.RData"))
load(here::here("results/results_Mtot10pct_no_background_noise_nsimu1000/saves/site_outputs_H0_H1_20seqs.RData"))

site_n20_Mtot10pct_no_noise <- do.call(rbind, lapply(seq_along(results_tree_list), function(i) {
  evaluate_site_detection(i, results_tree_list, site_output, Mtot_prop = 0.1)
}))

## n50
load(here::here("results/results_Mtot10pct_no_background_noise_nsimu1000/saves/01-simulation_trees_H1_50seqs.RData"))
load(here::here("results/results_Mtot10pct_no_background_noise_nsimu1000/saves/tree_outputs_H0_H1_50seqs.RData"))
load(here::here("results/results_Mtot10pct_no_background_noise_nsimu1000/saves/site_outputs_H0_H1_50seqs.RData"))

site_n50_Mtot10pct_no_noise <- do.call(rbind, lapply(seq_along(results_tree_list), function(i) {
  evaluate_site_detection(i, results_tree_list, site_output, Mtot_prop = 0.1)
}))

## n100
load(here::here("results/results_Mtot10pct_no_background_noise_nsimu1000/saves/01-simulation_trees_H1_100seqs.RData"))
load(here::here("results/results_Mtot10pct_no_background_noise_nsimu1000/saves/tree_outputs_H0_H1_100seqs.RData"))
load(here::here("results/results_Mtot10pct_no_background_noise_nsimu1000/saves/site_outputs_H0_H1_100seqs.RData"))

site_n100_Mtot10pct_no_noise <- do.call(rbind, lapply(seq_along(results_tree_list), function(i) {
  evaluate_site_detection(i, results_tree_list, site_output, Mtot_prop = 0.1)
}))

## n300
load(here::here("results/results_Mtot10pct_no_background_noise_nsimu1000/saves/01-simulation_trees_H1_300seqs.RData"))
load(here::here("results/results_Mtot10pct_no_background_noise_nsimu1000/saves/tree_outputs_H0_H1_300seqs.RData"))
load(here::here("results/results_Mtot10pct_no_background_noise_nsimu1000/saves/site_outputs_H0_H1_300seqs.RData"))

site_n300_Mtot10pct_no_noise <- do.call(rbind, lapply(seq_along(results_tree_list), function(i) {
  evaluate_site_detection(i, results_tree_list, site_output, Mtot_prop = 0.1)
}))


### Mtot50pct

## n20
load(here::here("results/results_Mtot50pct_no_background_noise_nsimu1000/saves/01-simulation_trees_H1_20seqs.RData"))
load(here::here("results/results_Mtot50pct_no_background_noise_nsimu1000/saves/tree_outputs_H0_H1_20seqs.RData"))
load(here::here("results/results_Mtot50pct_no_background_noise_nsimu1000/saves/site_outputs_H0_H1_20seqs.RData"))

site_n20_Mtot50pct_no_noise <- do.call(rbind, lapply(seq_along(results_tree_list), function(i) {
  evaluate_site_detection(i, results_tree_list, site_output, Mtot_prop = 0.5)
}))

## n50
load(here::here("results/results_Mtot50pct_no_background_noise_nsimu1000/saves/01-simulation_trees_H1_50seqs.RData"))
load(here::here("results/results_Mtot50pct_no_background_noise_nsimu1000/saves/tree_outputs_H0_H1_50seqs.RData"))
load(here::here("results/results_Mtot50pct_no_background_noise_nsimu1000/saves/site_outputs_H0_H1_50seqs.RData"))

site_n50_Mtot50pct_no_noise <- do.call(rbind, lapply(seq_along(results_tree_list), function(i) {
  evaluate_site_detection(i, results_tree_list, site_output, Mtot_prop = 0.5)
}))

## n100
load(here::here("results/results_Mtot50pct_no_background_noise_nsimu1000/saves/01-simulation_trees_H1_100seqs.RData"))
load(here::here("results/results_Mtot50pct_no_background_noise_nsimu1000/saves/tree_outputs_H0_H1_100seqs.RData"))
load(here::here("results/results_Mtot50pct_no_background_noise_nsimu1000/saves/site_outputs_H0_H1_100seqs.RData"))

site_n100_Mtot50pct_no_noise <- do.call(rbind, lapply(seq_along(results_tree_list), function(i) {
  evaluate_site_detection(i, results_tree_list, site_output, Mtot_prop = 0.5)
}))

## n300
load(here::here("results/results_Mtot50pct_no_background_noise_nsimu1000/saves/01-simulation_trees_H1_300seqs.RData"))
load(here::here("results/results_Mtot50pct_no_background_noise_nsimu1000/saves/tree_outputs_H0_H1_300seqs.RData"))
load(here::here("results/results_Mtot50pct_no_background_noise_nsimu1000/saves/site_outputs_H0_H1_300seqs.RData"))

site_n300_Mtot50pct_no_noise <- do.call(rbind, lapply(seq_along(results_tree_list), function(i) {
  evaluate_site_detection(i, results_tree_list, site_output, Mtot_prop = 0.5)
}))

############## noise
### Mtot1pct

## n20
load(here::here("results/results_Mtot1pct_two_sided_background_noise_nsimu1000/saves/01-simulation_trees_H1_20seqs.RData"))
load(here::here("results/results_Mtot1pct_two_sided_background_noise_nsimu1000/saves/tree_outputs_H0_H1_20seqs.RData"))
load(here::here("results/results_Mtot1pct_two_sided_background_noise_nsimu1000/saves/site_outputs_H0_H1_20seqs.RData"))

site_n20_Mtot1pct_noise <- do.call(rbind, lapply(seq_along(results_tree_list), function(i) {
  evaluate_site_detection(i, results_tree_list, site_output, Mtot_prop = 0.01)
}))

## n50
load(here::here("results/results_Mtot1pct_two_sided_background_noise_nsimu1000/saves/01-simulation_trees_H1_50seqs.RData"))
load(here::here("results/results_Mtot1pct_two_sided_background_noise_nsimu1000/saves/tree_outputs_H0_H1_50seqs.RData"))
load(here::here("results/results_Mtot1pct_two_sided_background_noise_nsimu1000/saves/site_outputs_H0_H1_50seqs.RData"))

site_n50_Mtot1pct_noise <- do.call(rbind, lapply(seq_along(results_tree_list), function(i) {
  evaluate_site_detection(i, results_tree_list, site_output, Mtot_prop = 0.01)
}))

## n100
load(here::here("results/results_Mtot1pct_two_sided_background_noise_nsimu1000/saves/01-simulation_trees_H1_100seqs.RData"))
load(here::here("results/results_Mtot1pct_two_sided_background_noise_nsimu1000/saves/tree_outputs_H0_H1_100seqs.RData"))
load(here::here("results/results_Mtot1pct_two_sided_background_noise_nsimu1000/saves/site_outputs_H0_H1_100seqs.RData"))

site_n100_Mtot1pct_noise <- do.call(rbind, lapply(seq_along(results_tree_list), function(i) {
  evaluate_site_detection(i, results_tree_list, site_output, Mtot_prop = 0.01)
}))

## n300
load(here::here("results/results_Mtot1pct_two_sided_background_noise_nsimu1000/saves/01-simulation_trees_H1_300seqs.RData"))
load(here::here("results/results_Mtot1pct_two_sided_background_noise_nsimu1000/saves/tree_outputs_H0_H1_300seqs.RData"))
load(here::here("results/results_Mtot1pct_two_sided_background_noise_nsimu1000/saves/site_outputs_H0_H1_300seqs.RData"))

site_n300_Mtot1pct_noise <- do.call(rbind, lapply(seq_along(results_tree_list), function(i) {
  evaluate_site_detection(i, results_tree_list, site_output, Mtot_prop = 0.01)
}))

### Mtot10pct

## n20
load(here::here("results/results_Mtot10pct_two_sided_background_noise_nsimu1000/saves/01-simulation_trees_H1_20seqs.RData"))
load(here::here("results/results_Mtot10pct_two_sided_background_noise_nsimu1000/saves/tree_outputs_H0_H1_20seqs.RData"))
load(here::here("results/results_Mtot10pct_two_sided_background_noise_nsimu1000/saves/site_outputs_H0_H1_20seqs.RData"))

site_n20_Mtot10pct_noise <- do.call(rbind, lapply(seq_along(results_tree_list), function(i) {
  evaluate_site_detection(i, results_tree_list, site_output, Mtot_prop = 0.1)
}))

## n50
load(here::here("results/results_Mtot10pct_two_sided_background_noise_nsimu1000/saves/01-simulation_trees_H1_50seqs.RData"))
load(here::here("results/results_Mtot10pct_two_sided_background_noise_nsimu1000/saves/tree_outputs_H0_H1_50seqs.RData"))
load(here::here("results/results_Mtot10pct_two_sided_background_noise_nsimu1000/saves/site_outputs_H0_H1_50seqs.RData"))

site_n50_Mtot10pct_noise <- do.call(rbind, lapply(seq_along(results_tree_list), function(i) {
  evaluate_site_detection(i, results_tree_list, site_output, Mtot_prop = 0.1)
}))

## n100
load(here::here("results/results_Mtot10pct_two_sided_background_noise_nsimu1000/saves/01-simulation_trees_H1_100seqs.RData"))
load(here::here("results/results_Mtot10pct_two_sided_background_noise_nsimu1000/saves/tree_outputs_H0_H1_100seqs.RData"))
load(here::here("results/results_Mtot10pct_two_sided_background_noise_nsimu1000/saves/site_outputs_H0_H1_100seqs.RData"))

site_n100_Mtot10pct_noise <- do.call(rbind, lapply(seq_along(results_tree_list), function(i) {
  evaluate_site_detection(i, results_tree_list, site_output, Mtot_prop = 0.1)
}))

## n300
load(here::here("results/results_Mtot10pct_two_sided_background_noise_nsimu1000/saves/01-simulation_trees_H1_300seqs.RData"))
load(here::here("results/results_Mtot10pct_two_sided_background_noise_nsimu1000/saves/tree_outputs_H0_H1_300seqs.RData"))
load(here::here("results/results_Mtot10pct_two_sided_background_noise_nsimu1000/saves/site_outputs_H0_H1_300seqs.RData"))

site_n300_Mtot10pct_noise <- do.call(rbind, lapply(seq_along(results_tree_list), function(i) {
  evaluate_site_detection(i, results_tree_list, site_output, Mtot_prop = 0.1)
}))


### Mtot50pct

## n20
load(here::here("results/results_Mtot50pct_two_sided_background_noise_nsimu1000/saves/01-simulation_trees_H1_20seqs.RData"))
load(here::here("results/results_Mtot50pct_two_sided_background_noise_nsimu1000/saves/tree_outputs_H0_H1_20seqs.RData"))
load(here::here("results/results_Mtot50pct_two_sided_background_noise_nsimu1000/saves/site_outputs_H0_H1_20seqs.RData"))

site_n20_Mtot50pct_noise <- do.call(rbind, lapply(seq_along(results_tree_list), function(i) {
  evaluate_site_detection(i, results_tree_list, site_output, Mtot_prop = 0.5)
}))

## n50
load(here::here("results/results_Mtot50pct_two_sided_background_noise_nsimu1000/saves/01-simulation_trees_H1_50seqs.RData"))
load(here::here("results/results_Mtot50pct_two_sided_background_noise_nsimu1000/saves/tree_outputs_H0_H1_50seqs.RData"))
load(here::here("results/results_Mtot50pct_two_sided_background_noise_nsimu1000/saves/site_outputs_H0_H1_50seqs.RData"))

site_n50_Mtot50pct_noise <- do.call(rbind, lapply(seq_along(results_tree_list), function(i) {
  evaluate_site_detection(i, results_tree_list, site_output, Mtot_prop = 0.5)
}))

## n100
load(here::here("results/results_Mtot50pct_two_sided_background_noise_nsimu1000/saves/01-simulation_trees_H1_100seqs.RData"))
load(here::here("results/results_Mtot50pct_two_sided_background_noise_nsimu1000/saves/tree_outputs_H0_H1_100seqs.RData"))
load(here::here("results/results_Mtot50pct_two_sided_background_noise_nsimu1000/saves/site_outputs_H0_H1_100seqs.RData"))

site_n100_Mtot50pct_noise <- do.call(rbind, lapply(seq_along(results_tree_list), function(i) {
  evaluate_site_detection(i, results_tree_list, site_output, Mtot_prop = 0.5)
}))

## n300
load(here::here("results/results_Mtot50pct_two_sided_background_noise_nsimu1000/saves/01-simulation_trees_H1_300seqs.RData"))
load(here::here("results/results_Mtot50pct_two_sided_background_noise_nsimu1000/saves/tree_outputs_H0_H1_300seqs.RData"))
load(here::here("results/results_Mtot50pct_two_sided_background_noise_nsimu1000/saves/site_outputs_H0_H1_300seqs.RData"))

site_n300_Mtot50pct_noise <- do.call(rbind, lapply(seq_along(results_tree_list), function(i) {
  evaluate_site_detection(i, results_tree_list, site_output, Mtot_prop = 0.5)
}))



site_all <- dplyr::bind_rows(
  site_n20_Mtot1pct_no_noise %>% mutate(Mtot = "1%", Noise = "no noise", n = 20),
  site_n50_Mtot1pct_no_noise %>% mutate(Mtot = "1%", Noise = "no noise", n = 50),
  site_n100_Mtot1pct_no_noise %>% mutate(Mtot = "1%", Noise = "no noise", n = 100),
  site_n300_Mtot1pct_no_noise %>% mutate(Mtot = "1%", Noise = "no noise", n = 300),

  site_n20_Mtot10pct_no_noise %>% mutate(Mtot = "10%", Noise = "no noise", n = 20),
  site_n50_Mtot10pct_no_noise %>% mutate(Mtot = "10%", Noise = "no noise", n = 50),
  site_n100_Mtot10pct_no_noise %>% mutate(Mtot = "10%", Noise = "no noise", n = 100),
  site_n300_Mtot10pct_no_noise %>% mutate(Mtot = "10%", Noise = "no noise", n = 300),

  site_n20_Mtot50pct_no_noise %>% mutate(Mtot = "50%", Noise = "no noise", n = 20),
  site_n50_Mtot50pct_no_noise %>% mutate(Mtot = "50%", Noise = "no noise", n = 50),
  site_n100_Mtot50pct_no_noise %>% mutate(Mtot = "50%", Noise = "no noise", n = 100),
  site_n300_Mtot50pct_no_noise %>% mutate(Mtot = "50%", Noise = "no noise", n = 300),

  site_n20_Mtot1pct_noise %>% mutate(Mtot = "1%", Noise = "noise", n = 20),
  site_n50_Mtot1pct_noise %>% mutate(Mtot = "1%", Noise = "noise", n = 50),
  site_n100_Mtot1pct_noise %>% mutate(Mtot = "1%", Noise = "noise", n = 100),
  site_n300_Mtot1pct_noise %>% mutate(Mtot = "1%", Noise = "noise", n = 300),

  site_n20_Mtot10pct_noise %>% mutate(Mtot = "10%", Noise = "noise", n = 20),
  site_n50_Mtot10pct_noise %>% mutate(Mtot = "10%", Noise = "noise", n = 50),
  site_n100_Mtot10pct_noise %>% mutate(Mtot = "10%", Noise = "noise", n = 100),
  site_n300_Mtot10pct_noise %>% mutate(Mtot = "10%", Noise = "noise", n = 300),

  site_n20_Mtot50pct_noise %>% mutate(Mtot = "50%", Noise = "noise", n = 20),
  site_n50_Mtot50pct_noise %>% mutate(Mtot = "50%", Noise = "noise", n = 50),
  site_n100_Mtot50pct_noise %>% mutate(Mtot = "50%", Noise = "noise", n = 100),
  site_n300_Mtot50pct_noise %>% mutate(Mtot = "50%", Noise = "noise", n = 300)
)

site_all <- site_all %>% filter(!is.na(precision), !is.na(recall))

ggplot(site_all, aes(x = factor(n), y = precision, fill = Noise)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA, width = 0.6) +
  geom_jitter(width = 0.2, alpha = 0.2, color = "black", size = 0.5) +
  facet_wrap(~ Mtot, labeller = label_both) +
  labs(title = "Precision by sample size",
       x = "Sample size (n)",
       y = "Precision",
       fill = "Noise") +
  theme_minimal()


ggplot(site_all, aes(x = factor(n), y = recall, fill = Noise)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA, width = 0.6) +
  geom_jitter(width = 0.2, alpha = 0.2, color = "black", size = 0.5) +
  facet_wrap(~ Mtot, labeller = label_both) +
  labs(title = "Recall by sample size",
       x = "Sample size (n)",
       y = "Recall",
       fill = "Noise") +
  theme_minimal()


library(RColorBrewer)
cols <- brewer.pal(n = 3, name = "Set2")[1:2]

ggplot(site_all, aes(x = factor(n), y = precision, fill = Noise)) +
  geom_boxplot(alpha = 0.8, outlier.shape = NA, width = 0.6, color = "black") +
  geom_jitter(width = 0.2, alpha = 0.15, color = "black", size = 0.5) +
  facet_wrap(~ Mtot, labeller = label_bquote("Mtot: "*.(Mtot)*"")) +
  scale_fill_manual(values = cols, labels = c("No noise", "Noise")) +
  labs(
    x = "Sample size (n)",
    y = "Precision",
    fill = "Noise"
  ) +
  theme_bw(base_size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.title = element_text(face = "bold"),
    legend.position = "top",
    strip.background = element_rect(fill = "grey90", color = NA),
    strip.text = element_text(face = "bold")
  )

ggplot(site_all, aes(x = factor(n), y = recall, fill = Noise)) +
  geom_boxplot(alpha = 0.8, outlier.shape = NA, width = 0.6, color = "black") +
  geom_jitter(width = 0.1, alpha = 0.15, color = "black", size = 0.5) +
  facet_wrap(~ Mtot, labeller = label_bquote("Mtot: "*.(Mtot)*"%")) +
  scale_fill_manual(values = cols, labels = c("No noise", "Noise")) +
  labs( x = "Sample size (n)", y = "Recall", fill = "Noise" ) +
  theme_bw(base_size = 13) +
  theme( plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
         axis.title = element_text(face = "bold"), legend.position = "top",
         strip.background = element_rect(fill = "grey90", color = NA),
         strip.text = element_text(face = "bold")
  )




ggplot(site_all, aes(x = factor(n), y = precision, fill = Noise)) +

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
    strip.text = element_text(face = "bold")
  )



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
    strip.text = element_text(face = "bold")
  )
