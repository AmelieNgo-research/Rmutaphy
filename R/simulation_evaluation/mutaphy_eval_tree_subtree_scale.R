# Tree scale: ROC curves (no noise + noise)
# Subtree scale: Specificity (H0) + AUC-PR (H1) raw p-value and corrected p-value

library(pROC); library(dplyr); library(patchwork)
library(PRROC); library(here); library(ggplot2)

#### Tree scale

##### No Noise

################## AI

#### H1

## Mtot1pct
# df_ai_h1_n20_Mtot1pct <- data.frame(
#   pvalue = sapply(res_all_n20_Mtot1pct_k20_no_noise, function(res) res$AI$pvalue),
#   true_label = 1,
#   n = 20,
#   Mtot = "1pct"
# )
#
# df_ai_h1_n50_Mtot1pct <- data.frame(
#   pvalue = sapply(res_all_n50_Mtot1pct_k20_no_noise, function(res) res$AI$pvalue),
#   true_label = 1,
#   n = 50,
#   Mtot = "1pct"
# )
#
# df_ai_h1_n100_Mtot1pct <- data.frame(
#   pvalue = sapply(res_all_n100_Mtot1pct_k20_no_noise, function(res) res$AI$pvalue),
#   true_label = 1,
#   n = 100,
#   Mtot = "1pct"
# )
#
# df_ai_h1_n300_Mtot1pct <- data.frame(
#   pvalue = sapply(res_all_n300_Mtot1pct_k20_no_noise, function(res) res$AI$pvalue),
#   true_label = 1,
#   n = 300,
#   Mtot = "1pct"
# )
#
# ## Mtot10pct
# df_ai_h1_n20_Mtot10pct <- data.frame(
#   pvalue = sapply(res_all_n20_Mtot10pct_k20_no_noise, function(res) res$AI$pvalue),
#   true_label = 1,
#   n = 20,
#   Mtot = "10pct"
# )
#
# df_ai_h1_n50_Mtot10pct <- data.frame(
#   pvalue = sapply(res_all_n50_Mtot10pct_k20_no_noise, function(res) res$AI$pvalue),
#   true_label = 1,
#   n = 50,
#   Mtot = "10pct"
# )
#
# df_ai_h1_n100_Mtot10pct <- data.frame(
#   pvalue = sapply(res_all_n100_Mtot10pct_k20_no_noise, function(res) res$AI$pvalue),
#   true_label = 1,
#   n = 100,
#   Mtot = "10pct"
# )
#
# df_ai_h1_n300_Mtot10pct <- data.frame(
#   pvalue = sapply(res_all_n300_Mtot10pct_k20_no_noise, function(res) res$AI$pvalue),
#   true_label = 1,
#   n = 300,
#   Mtot = "10pct"
# )
#
# ## Mtot50pct
# df_ai_h1_n20_Mtot50pct <- data.frame(
#   pvalue = sapply(res_all_n20_Mtot50pct_k20_no_noise, function(res) res$AI$pvalue),
#   true_label = 1,
#   n = 20,
#   Mtot = "50pct"
# )
#
# df_ai_h1_n50_Mtot50pct <- data.frame(
#   pvalue = sapply(res_all_n50_Mtot50pct_k20_no_noise, function(res) res$AI$pvalue),
#   true_label = 1,
#   n = 50,
#   Mtot = "50pct"
# )
#
# df_ai_h1_n100_Mtot50pct <- data.frame(
#   pvalue = sapply(res_all_n100_Mtot50pct_k20_no_noise, function(res) res$AI$pvalue),
#   true_label = 1,
#   n = 100,
#   Mtot = "50pct"
# )
#
# df_ai_h1_n300_Mtot50pct <- data.frame(
#   pvalue = sapply(res_all_n300_Mtot50pct_k20_no_noise, function(res) res$AI$pvalue),
#   true_label = 1,
#   n = 300,
#   Mtot = "50pct"
# )
#
# #### H0
#
# ## Mtot1pct
# df_ai_H0_n20_Mtot1pct <- data.frame(
#   pvalue = sapply(res_all_n20_Mtot1pct_k20_H0_no_noise, function(res) res$AI$pvalue),
#   true_label = 0,
#   n = 20,
#   Mtot = "1pct"
# )
#
# df_ai_H0_n50_Mtot1pct <- data.frame(
#   pvalue = sapply(res_all_n50_Mtot1pct_k20_H0_no_noise, function(res) res$AI$pvalue),
#   true_label = 0,
#   n = 50,
#   Mtot = "1pct"
# )
#
# df_ai_H0_n100_Mtot1pct <- data.frame(
#   pvalue = sapply(res_all_n100_Mtot1pct_k20_H0_no_noise, function(res) res$AI$pvalue),
#   true_label = 0,
#   n = 100,
#   Mtot = "1pct"
# )
#
# df_ai_H0_n300_Mtot1pct <- data.frame(
#   pvalue = sapply(res_all_n300_Mtot1pct_k20_H0_no_noise, function(res) res$AI$pvalue),
#   true_label = 0,
#   n = 300,
#   Mtot = "1pct"
# )
#
# ## Mtot10pct
# df_ai_H0_n20_Mtot10pct <- data.frame(
#   pvalue = sapply(res_all_n20_Mtot10pct_k20_H0_no_noise, function(res) res$AI$pvalue),
#   true_label = 0,
#   n = 20,
#   Mtot = "10pct"
# )
#
# df_ai_H0_n50_Mtot10pct <- data.frame(
#   pvalue = sapply(res_all_n50_Mtot10pct_k20_H0_no_noise, function(res) res$AI$pvalue),
#   true_label = 0,
#   n = 50,
#   Mtot = "10pct"
# )
#
# df_ai_H0_n100_Mtot10pct <- data.frame(
#   pvalue = sapply(res_all_n100_Mtot10pct_k20_H0_no_noise, function(res) res$AI$pvalue),
#   true_label = 0,
#   n = 100,
#   Mtot = "10pct"
# )
#
# df_ai_H0_n300_Mtot10pct <- data.frame(
#   pvalue = sapply(res_all_n300_Mtot10pct_k20_H0_no_noise, function(res) res$AI$pvalue),
#   true_label = 0,
#   n = 300,
#   Mtot = "10pct"
# )
#
# ## Mtot50pct
# df_ai_H0_n20_Mtot50pct <- data.frame(
#   pvalue = sapply(res_all_n20_Mtot50pct_k20_H0_no_noise, function(res) res$AI$pvalue),
#   true_label = 0,
#   n = 20,
#   Mtot = "50pct"
# )
#
# df_ai_H0_n50_Mtot50pct <- data.frame(
#   pvalue = sapply(res_all_n50_Mtot50pct_k20_H0_no_noise, function(res) res$AI$pvalue),
#   true_label = 0,
#   n = 50,
#   Mtot = "50pct"
# )
#
# df_ai_H0_n100_Mtot50pct <- data.frame(
#   pvalue = sapply(res_all_n100_Mtot50pct_k20_H0_no_noise, function(res) res$AI$pvalue),
#   true_label = 0,
#   n = 100,
#   Mtot = "50pct"
# )
#
# df_ai_H0_n300_Mtot50pct <- data.frame(
#   pvalue = sapply(res_all_n300_Mtot50pct_k20_H0_no_noise, function(res) res$AI$pvalue),
#   true_label = 0,
#   n = 300,
#   Mtot = "50pct"
# )
#
#
# df_ai_no_noise <- rbind(
#   df_ai_h1_n20_Mtot1pct,
#   df_ai_h1_n50_Mtot1pct,
#   df_ai_h1_n100_Mtot1pct,
#   df_ai_h1_n300_Mtot1pct,
#   df_ai_h1_n20_Mtot10pct,
#   df_ai_h1_n50_Mtot10pct,
#   df_ai_h1_n100_Mtot10pct,
#   df_ai_h1_n300_Mtot10pct,
#   df_ai_h1_n20_Mtot50pct,
#   df_ai_h1_n50_Mtot50pct,
#   df_ai_h1_n100_Mtot50pct,
#   df_ai_h1_n300_Mtot50pct,
#   df_ai_H0_n20_Mtot1pct,
#   df_ai_H0_n50_Mtot1pct,
#   df_ai_H0_n100_Mtot1pct,
#   df_ai_H0_n300_Mtot1pct,
#   df_ai_H0_n20_Mtot10pct,
#   df_ai_H0_n50_Mtot10pct,
#   df_ai_H0_n100_Mtot10pct,
#   df_ai_H0_n300_Mtot10pct,
#   df_ai_H0_n20_Mtot50pct,
#   df_ai_H0_n50_Mtot50pct,
#   df_ai_H0_n100_Mtot50pct,
#   df_ai_H0_n300_Mtot50pct
# )
#
#
#
# roc_ai_no_noise <- roc(response = df_ai_no_noise$true_label,
#                        predictor = df_ai_no_noise$pvalue)
#
# roc_df_ai_no_noise <- data.frame(
#   specificity = roc_ai_no_noise$specificities,
#   sensitivity = roc_ai_no_noise$sensitivities,
#   Method = "AI"
# )
#
# ################## PS
#
# #### H1
#
# ## Mtot1pct
# df_ps_h1_n20_Mtot1pct <- data.frame(
#   pvalue = sapply(res_all_n20_Mtot1pct_k20_no_noise, function(res) res$PS$pvalue),
#   true_label = 1,
#   n = 20,
#   Mtot = "1pct"
# )
#
# df_ps_h1_n50_Mtot1pct <- data.frame(
#   pvalue = sapply(res_all_n50_Mtot1pct_k20_no_noise, function(res) res$PS$pvalue),
#   true_label = 1,
#   n = 50,
#   Mtot = "1pct"
# )
#
# df_ps_h1_n100_Mtot1pct <- data.frame(
#   pvalue = sapply(res_all_n100_Mtot1pct_k20_no_noise, function(res) res$PS$pvalue),
#   true_label = 1,
#   n = 100,
#   Mtot = "1pct"
# )
#
# df_ps_h1_n300_Mtot1pct <- data.frame(
#   pvalue = sapply(res_all_n300_Mtot1pct_k20_no_noise, function(res) res$PS$pvalue),
#   true_label = 1,
#   n = 300,
#   Mtot = "1pct"
# )
#
# ## Mtot10pct
# df_ps_h1_n20_Mtot10pct <- data.frame(
#   pvalue = sapply(res_all_n20_Mtot10pct_k20_no_noise, function(res) res$PS$pvalue),
#   true_label = 1,
#   n = 20,
#   Mtot = "10pct"
# )
#
# df_ps_h1_n50_Mtot10pct <- data.frame(
#   pvalue = sapply(res_all_n50_Mtot10pct_k20_no_noise, function(res) res$PS$pvalue),
#   true_label = 1,
#   n = 50,
#   Mtot = "10pct"
# )
#
# df_ps_h1_n100_Mtot10pct <- data.frame(
#   pvalue = sapply(res_all_n100_Mtot10pct_k20_no_noise, function(res) res$PS$pvalue),
#   true_label = 1,
#   n = 100,
#   Mtot = "10pct"
# )
#
# df_ps_h1_n300_Mtot10pct <- data.frame(
#   pvalue = sapply(res_all_n300_Mtot10pct_k20_no_noise, function(res) res$PS$pvalue),
#   true_label = 1,
#   n = 300,
#   Mtot = "10pct"
# )
#
# ## Mtot50pct
# df_ps_h1_n20_Mtot50pct <- data.frame(
#   pvalue = sapply(res_all_n20_Mtot50pct_k20_no_noise, function(res) res$PS$pvalue),
#   true_label = 1,
#   n = 20,
#   Mtot = "50pct"
# )
#
# df_ps_h1_n50_Mtot50pct <- data.frame(
#   pvalue = sapply(res_all_n50_Mtot50pct_k20_no_noise, function(res) res$PS$pvalue),
#   true_label = 1,
#   n = 50,
#   Mtot = "50pct"
# )
#
# df_ps_h1_n100_Mtot50pct <- data.frame(
#   pvalue = sapply(res_all_n100_Mtot50pct_k20_no_noise, function(res) res$PS$pvalue),
#   true_label = 1,
#   n = 100,
#   Mtot = "50pct"
# )
#
# df_ps_h1_n300_Mtot50pct <- data.frame(
#   pvalue = sapply(res_all_n300_Mtot50pct_k20_no_noise, function(res) res$PS$pvalue),
#   true_label = 1,
#   n = 300,
#   Mtot = "50pct"
# )
#
# #### H0
#
# ## Mtot1pct
# df_ps_H0_n20_Mtot1pct <- data.frame(
#   pvalue = sapply(res_all_n20_Mtot1pct_k20_H0_no_noise, function(res) res$PS$pvalue),
#   true_label = 0,
#   n = 20,
#   Mtot = "1pct"
# )
#
# df_ps_H0_n50_Mtot1pct <- data.frame(
#   pvalue = sapply(res_all_n50_Mtot1pct_k20_H0_no_noise, function(res) res$PS$pvalue),
#   true_label = 0,
#   n = 50,
#   Mtot = "1pct"
# )
#
# df_ps_H0_n100_Mtot1pct <- data.frame(
#   pvalue = sapply(res_all_n100_Mtot1pct_k20_H0_no_noise, function(res) res$PS$pvalue),
#   true_label = 0,
#   n = 100,
#   Mtot = "1pct"
# )
#
# df_ps_H0_n300_Mtot1pct <- data.frame(
#   pvalue = sapply(res_all_n300_Mtot1pct_k20_H0_no_noise, function(res) res$PS$pvalue),
#   true_label = 0,
#   n = 300,
#   Mtot = "1pct"
# )
#
# ## Mtot10pct
# df_ps_H0_n20_Mtot10pct <- data.frame(
#   pvalue = sapply(res_all_n20_Mtot10pct_k20_H0_no_noise, function(res) res$PS$pvalue),
#   true_label = 0,
#   n = 20,
#   Mtot = "10pct"
# )
#
# df_ps_H0_n50_Mtot10pct <- data.frame(
#   pvalue = sapply(res_all_n50_Mtot10pct_k20_H0_no_noise, function(res) res$PS$pvalue),
#   true_label = 0,
#   n = 50,
#   Mtot = "10pct"
# )
#
# df_ps_H0_n100_Mtot10pct <- data.frame(
#   pvalue = sapply(res_all_n100_Mtot10pct_k20_H0_no_noise, function(res) res$PS$pvalue),
#   true_label = 0,
#   n = 100,
#   Mtot = "10pct"
# )
#
# df_ps_H0_n300_Mtot10pct <- data.frame(
#   pvalue = sapply(res_all_n300_Mtot10pct_k20_H0_no_noise, function(res) res$PS$pvalue),
#   true_label = 0,
#   n = 300,
#   Mtot = "10pct"
# )
#
# ## Mtot50pct
# df_ps_H0_n20_Mtot50pct <- data.frame(
#   pvalue = sapply(res_all_n20_Mtot50pct_k20_H0_no_noise, function(res) res$PS$pvalue),
#   true_label = 0,
#   n = 20,
#   Mtot = "50pct"
# )
#
# df_ps_H0_n50_Mtot50pct <- data.frame(
#   pvalue = sapply(res_all_n50_Mtot50pct_k20_H0_no_noise, function(res) res$PS$pvalue),
#   true_label = 0,
#   n = 50,
#   Mtot = "50pct"
# )
#
# df_ps_H0_n100_Mtot50pct <- data.frame(
#   pvalue = sapply(res_all_n100_Mtot50pct_k20_H0_no_noise, function(res) res$PS$pvalue),
#   true_label = 0,
#   n = 100,
#   Mtot = "50pct"
# )
#
# df_ps_H0_n300_Mtot50pct <- data.frame(
#   pvalue = sapply(res_all_n300_Mtot50pct_k20_H0_no_noise, function(res) res$PS$pvalue),
#   true_label = 0,
#   n = 300,
#   Mtot = "50pct"
# )
#
#
# df_ps_no_noise <- rbind(
#   df_ps_h1_n20_Mtot1pct,
#   df_ps_h1_n50_Mtot1pct,
#   df_ps_h1_n100_Mtot1pct,
#   df_ps_h1_n300_Mtot1pct,
#   df_ps_h1_n20_Mtot10pct,
#   df_ps_h1_n50_Mtot10pct,
#   df_ps_h1_n100_Mtot10pct,
#   df_ps_h1_n300_Mtot10pct,
#   df_ps_h1_n20_Mtot50pct,
#   df_ps_h1_n50_Mtot50pct,
#   df_ps_h1_n100_Mtot50pct,
#   df_ps_h1_n300_Mtot50pct,
#   df_ps_H0_n20_Mtot1pct,
#   df_ps_H0_n50_Mtot1pct,
#   df_ps_H0_n100_Mtot1pct,
#   df_ps_H0_n300_Mtot1pct,
#   df_ps_H0_n20_Mtot10pct,
#   df_ps_H0_n50_Mtot10pct,
#   df_ps_H0_n100_Mtot10pct,
#   df_ps_H0_n300_Mtot10pct,
#   df_ps_H0_n20_Mtot50pct,
#   df_ps_H0_n50_Mtot50pct,
#   df_ps_H0_n100_Mtot50pct,
#   df_ps_H0_n300_Mtot50pct
# )
#
# roc_ps_no_noise <- roc(response = df_ps_no_noise$true_label,
#                        predictor = df_ps_no_noise$pvalue + runif(nrow(df_ps_no_noise), min = 0, max = 0.001))
#
# roc_df_ps_no_noise <- data.frame(
#   specificity = roc_ps_no_noise$specificities,
#   sensitivity = roc_ps_no_noise$sensitivities,
#   Method = "PS"
# )
#
# ################## MC trait0
#
# #### H1
#
# ## Mtot1pct
# df_mc_trait0_h1_n20_Mtot1pct <- data.frame(
#   pvalue = sapply(res_all_n20_Mtot1pct_k20_no_noise, function(res) res$MC$pvalues$pvalue[1]),
#   true_label = 1,
#   n = 20,
#   Mtot = "1pct"
# )
#
# df_mc_trait0_h1_n50_Mtot1pct <- data.frame(
#   pvalue = sapply(res_all_n50_Mtot1pct_k20_no_noise, function(res) res$MC$pvalues$pvalue[1]),
#   true_label = 1,
#   n = 50,
#   Mtot = "1pct"
# )
#
# df_mc_trait0_h1_n100_Mtot1pct <- data.frame(
#   pvalue = sapply(res_all_n100_Mtot1pct_k20_no_noise, function(res) res$MC$pvalues$pvalue[1]),
#   true_label = 1,
#   n = 100,
#   Mtot = "1pct"
# )
#
# df_mc_trait0_h1_n300_Mtot1pct <- data.frame(
#   pvalue = sapply(res_all_n300_Mtot1pct_k20_no_noise, function(res) res$MC$pvalues$pvalue[1]),
#   true_label = 1,
#   n = 300,
#   Mtot = "1pct"
# )
#
# ## Mtot10pct
# df_mc_trait0_h1_n20_Mtot10pct <- data.frame(
#   pvalue = sapply(res_all_n20_Mtot10pct_k20_no_noise, function(res) res$MC$pvalues$pvalue[1]),
#   true_label = 1,
#   n = 20,
#   Mtot = "10pct"
# )
#
# df_mc_trait0_h1_n50_Mtot10pct <- data.frame(
#   pvalue = sapply(res_all_n50_Mtot10pct_k20_no_noise, function(res) res$MC$pvalues$pvalue[1]),
#   true_label = 1,
#   n = 50,
#   Mtot = "10pct"
# )
#
# df_mc_trait0_h1_n100_Mtot10pct <- data.frame(
#   pvalue = sapply(res_all_n100_Mtot10pct_k20_no_noise, function(res) res$MC$pvalues$pvalue[1]),
#   true_label = 1,
#   n = 100,
#   Mtot = "10pct"
# )
#
# df_mc_trait0_h1_n300_Mtot10pct <- data.frame(
#   pvalue = sapply(res_all_n300_Mtot10pct_k20_no_noise, function(res) res$MC$pvalues$pvalue[1]),
#   true_label = 1,
#   n = 300,
#   Mtot = "10pct"
# )
#
# ## Mtot50pct
# df_mc_trait0_h1_n20_Mtot50pct <- data.frame(
#   pvalue = sapply(res_all_n20_Mtot50pct_k20_no_noise, function(res) res$MC$pvalues$pvalue[1]),
#   true_label = 1,
#   n = 20,
#   Mtot = "50pct"
# )
#
# df_mc_trait0_h1_n50_Mtot50pct <- data.frame(
#   pvalue = sapply(res_all_n50_Mtot50pct_k20_no_noise, function(res) res$MC$pvalues$pvalue[1]),
#   true_label = 1,
#   n = 50,
#   Mtot = "50pct"
# )
#
# df_mc_trait0_h1_n100_Mtot50pct <- data.frame(
#   pvalue = sapply(res_all_n100_Mtot50pct_k20_no_noise, function(res) res$MC$pvalues$pvalue[1]),
#   true_label = 1,
#   n = 100,
#   Mtot = "50pct"
# )
#
# df_mc_trait0_h1_n300_Mtot50pct <- data.frame(
#   pvalue = sapply(res_all_n300_Mtot50pct_k20_no_noise, function(res) res$MC$pvalues$pvalue[1]),
#   true_label = 1,
#   n = 300,
#   Mtot = "50pct"
# )
#
# #### H0
#
# ## Mtot1pct
# df_mc_trait0_H0_n20_Mtot1pct <- data.frame(
#   pvalue = sapply(res_all_n20_Mtot1pct_k20_H0_no_noise, function(res) res$MC$pvalues$pvalue[1]),
#   true_label = 0,
#   n = 20,
#   Mtot = "1pct"
# )
#
# df_mc_trait0_H0_n50_Mtot1pct <- data.frame(
#   pvalue = sapply(res_all_n50_Mtot1pct_k20_H0_no_noise, function(res) res$MC$pvalues$pvalue[1]),
#   true_label = 0,
#   n = 50,
#   Mtot = "1pct"
# )
#
# df_mc_trait0_H0_n100_Mtot1pct <- data.frame(
#   pvalue = sapply(res_all_n100_Mtot1pct_k20_H0_no_noise, function(res) res$MC$pvalues$pvalue[1]),
#   true_label = 0,
#   n = 100,
#   Mtot = "1pct"
# )
#
# df_mc_trait0_H0_n300_Mtot1pct <- data.frame(
#   pvalue = sapply(res_all_n300_Mtot1pct_k20_H0_no_noise, function(res) res$MC$pvalues$pvalue[1]),
#   true_label = 0,
#   n = 300,
#   Mtot = "1pct"
# )
#
# ## Mtot10pct
# df_mc_trait0_H0_n20_Mtot10pct <- data.frame(
#   pvalue = sapply(res_all_n20_Mtot10pct_k20_H0_no_noise, function(res) res$MC$pvalues$pvalue[1]),
#   true_label = 0,
#   n = 20,
#   Mtot = "10pct"
# )
#
# df_mc_trait0_H0_n50_Mtot10pct <- data.frame(
#   pvalue = sapply(res_all_n50_Mtot10pct_k20_H0_no_noise, function(res) res$MC$pvalues$pvalue[1]),
#   true_label = 0,
#   n = 50,
#   Mtot = "10pct"
# )
#
# df_mc_trait0_H0_n100_Mtot10pct <- data.frame(
#   pvalue = sapply(res_all_n100_Mtot10pct_k20_H0_no_noise, function(res) res$MC$pvalues$pvalue[1]),
#   true_label = 0,
#   n = 100,
#   Mtot = "10pct"
# )
#
# df_mc_trait0_H0_n300_Mtot10pct <- data.frame(
#   pvalue = sapply(res_all_n300_Mtot10pct_k20_H0_no_noise, function(res) res$MC$pvalues$pvalue[1]),
#   true_label = 0,
#   n = 300,
#   Mtot = "10pct"
# )
#
# ## Mtot50pct
# df_mc_trait0_H0_n20_Mtot50pct <- data.frame(
#   pvalue = sapply(res_all_n20_Mtot50pct_k20_H0_no_noise, function(res) res$MC$pvalues$pvalue[1]),
#   true_label = 0,
#   n = 20,
#   Mtot = "50pct"
# )
#
# df_mc_trait0_H0_n50_Mtot50pct <- data.frame(
#   pvalue = sapply(res_all_n50_Mtot50pct_k20_H0_no_noise, function(res) res$MC$pvalues$pvalue[1]),
#   true_label = 0,
#   n = 50,
#   Mtot = "50pct"
# )
#
# df_mc_trait0_H0_n100_Mtot50pct <- data.frame(
#   pvalue = sapply(res_all_n100_Mtot50pct_k20_H0_no_noise, function(res) res$MC$pvalues$pvalue[1]),
#   true_label = 0,
#   n = 100,
#   Mtot = "50pct"
# )
#
# df_mc_trait0_H0_n300_Mtot50pct <- data.frame(
#   pvalue = sapply(res_all_n300_Mtot50pct_k20_H0_no_noise, function(res) res$MC$pvalues$pvalue[1]),
#   true_label = 0,
#   n = 300,
#   Mtot = "50pct"
# )
#
#
# df_mc_trait0_no_noise <- rbind(
#   df_mc_trait0_h1_n20_Mtot1pct,
#   df_mc_trait0_h1_n50_Mtot1pct,
#   df_mc_trait0_h1_n100_Mtot1pct,
#   df_mc_trait0_h1_n300_Mtot1pct,
#   df_mc_trait0_h1_n20_Mtot10pct,
#   df_mc_trait0_h1_n50_Mtot10pct,
#   df_mc_trait0_h1_n100_Mtot10pct,
#   df_mc_trait0_h1_n300_Mtot10pct,
#   df_mc_trait0_h1_n20_Mtot50pct,
#   df_mc_trait0_h1_n50_Mtot50pct,
#   df_mc_trait0_h1_n100_Mtot50pct,
#   df_mc_trait0_h1_n300_Mtot50pct,
#   df_mc_trait0_H0_n20_Mtot1pct,
#   df_mc_trait0_H0_n50_Mtot1pct,
#   df_mc_trait0_H0_n100_Mtot1pct,
#   df_mc_trait0_H0_n300_Mtot1pct,
#   df_mc_trait0_H0_n20_Mtot10pct,
#   df_mc_trait0_H0_n50_Mtot10pct,
#   df_mc_trait0_H0_n100_Mtot10pct,
#   df_mc_trait0_H0_n300_Mtot10pct,
#   df_mc_trait0_H0_n20_Mtot50pct,
#   df_mc_trait0_H0_n50_Mtot50pct,
#   df_mc_trait0_H0_n100_Mtot50pct,
#   df_mc_trait0_H0_n300_Mtot50pct
# )
#
# roc_mc_trait0_no_noise <- roc(response = df_mc_trait0_no_noise$true_label,
#                               predictor = df_mc_trait0_no_noise$pvalue + runif(nrow(df_mc_trait0_no_noise), min = 0, max = 0.001))
#
# roc_df_mc_trait0_no_noise <- data.frame(
#   specificity = roc_mc_trait0_no_noise$specificities,
#   sensitivity = roc_mc_trait0_no_noise$sensitivities,
#   Method = "MC trait0"
# )
#
# ################## MC trait1
#
# #### H1
#
# ## Mtot1pct
# df_mc_trait1_h1_n20_Mtot1pct <- data.frame(
#   pvalue = sapply(res_all_n20_Mtot1pct_k20_no_noise, function(res) res$MC$pvalues$pvalue[2]),
#   true_label = 1,
#   n = 20,
#   Mtot = "1pct"
# )
#
# df_mc_trait1_h1_n50_Mtot1pct <- data.frame(
#   pvalue = sapply(res_all_n50_Mtot1pct_k20_no_noise, function(res) res$MC$pvalues$pvalue[2]),
#   true_label = 1,
#   n = 50,
#   Mtot = "1pct"
# )
#
# df_mc_trait1_h1_n100_Mtot1pct <- data.frame(
#   pvalue = sapply(res_all_n100_Mtot1pct_k20_no_noise, function(res) res$MC$pvalues$pvalue[2]),
#   true_label = 1,
#   n = 100,
#   Mtot = "1pct"
# )
#
# df_mc_trait1_h1_n300_Mtot1pct <- data.frame(
#   pvalue = sapply(res_all_n300_Mtot1pct_k20_no_noise, function(res) res$MC$pvalues$pvalue[2]),
#   true_label = 1,
#   n = 300,
#   Mtot = "1pct"
# )
#
# ## Mtot10pct
# df_mc_trait1_h1_n20_Mtot10pct <- data.frame(
#   pvalue = sapply(res_all_n20_Mtot10pct_k20_no_noise, function(res) res$MC$pvalues$pvalue[2]),
#   true_label = 1,
#   n = 20,
#   Mtot = "10pct"
# )
#
# df_mc_trait1_h1_n50_Mtot10pct <- data.frame(
#   pvalue = sapply(res_all_n50_Mtot10pct_k20_no_noise, function(res) res$MC$pvalues$pvalue[2]),
#   true_label = 1,
#   n = 50,
#   Mtot = "10pct"
# )
#
# df_mc_trait1_h1_n100_Mtot10pct <- data.frame(
#   pvalue = sapply(res_all_n100_Mtot10pct_k20_no_noise, function(res) res$MC$pvalues$pvalue[2]),
#   true_label = 1,
#   n = 100,
#   Mtot = "10pct"
# )
#
# df_mc_trait1_h1_n300_Mtot10pct <- data.frame(
#   pvalue = sapply(res_all_n300_Mtot10pct_k20_no_noise, function(res) res$MC$pvalues$pvalue[2]),
#   true_label = 1,
#   n = 300,
#   Mtot = "10pct"
# )
#
# ## Mtot50pct
# df_mc_trait1_h1_n20_Mtot50pct <- data.frame(
#   pvalue = sapply(res_all_n20_Mtot50pct_k20_no_noise, function(res) res$MC$pvalues$pvalue[2]),
#   true_label = 1,
#   n = 20,
#   Mtot = "50pct"
# )
#
# df_mc_trait1_h1_n50_Mtot50pct <- data.frame(
#   pvalue = sapply(res_all_n50_Mtot50pct_k20_no_noise, function(res) res$MC$pvalues$pvalue[2]),
#   true_label = 1,
#   n = 50,
#   Mtot = "50pct"
# )
#
# df_mc_trait1_h1_n100_Mtot50pct <- data.frame(
#   pvalue = sapply(res_all_n100_Mtot50pct_k20_no_noise, function(res) res$MC$pvalues$pvalue[2]),
#   true_label = 1,
#   n = 100,
#   Mtot = "50pct"
# )
#
# df_mc_trait1_h1_n300_Mtot50pct <- data.frame(
#   pvalue = sapply(res_all_n300_Mtot50pct_k20_no_noise, function(res) res$MC$pvalues$pvalue[2]),
#   true_label = 1,
#   n = 300,
#   Mtot = "50pct"
# )
#
# #### H0
#
# ## Mtot1pct
# df_mc_trait1_H0_n20_Mtot1pct <- data.frame(
#   pvalue = sapply(res_all_n20_Mtot1pct_k20_H0_no_noise, function(res) res$MC$pvalues$pvalue[2]),
#   true_label = 0,
#   n = 20,
#   Mtot = "1pct"
# )
#
# df_mc_trait1_H0_n50_Mtot1pct <- data.frame(
#   pvalue = sapply(res_all_n50_Mtot1pct_k20_H0_no_noise, function(res) res$MC$pvalues$pvalue[2]),
#   true_label = 0,
#   n = 50,
#   Mtot = "1pct"
# )
#
# df_mc_trait1_H0_n100_Mtot1pct <- data.frame(
#   pvalue = sapply(res_all_n100_Mtot1pct_k20_H0_no_noise, function(res) res$MC$pvalues$pvalue[2]),
#   true_label = 0,
#   n = 100,
#   Mtot = "1pct"
# )
#
# df_mc_trait1_H0_n300_Mtot1pct <- data.frame(
#   pvalue = sapply(res_all_n300_Mtot1pct_k20_H0_no_noise, function(res) res$MC$pvalues$pvalue[2]),
#   true_label = 0,
#   n = 300,
#   Mtot = "1pct"
# )
#
# ## Mtot10pct
# df_mc_trait1_H0_n20_Mtot10pct <- data.frame(
#   pvalue = sapply(res_all_n20_Mtot10pct_k20_H0_no_noise, function(res) res$MC$pvalues$pvalue[2]),
#   true_label = 0,
#   n = 20,
#   Mtot = "10pct"
# )
#
# df_mc_trait1_H0_n50_Mtot10pct <- data.frame(
#   pvalue = sapply(res_all_n50_Mtot10pct_k20_H0_no_noise, function(res) res$MC$pvalues$pvalue[2]),
#   true_label = 0,
#   n = 50,
#   Mtot = "10pct"
# )
#
# df_mc_trait1_H0_n100_Mtot10pct <- data.frame(
#   pvalue = sapply(res_all_n100_Mtot10pct_k20_H0_no_noise, function(res) res$MC$pvalues$pvalue[2]),
#   true_label = 0,
#   n = 100,
#   Mtot = "10pct"
# )
#
# df_mc_trait1_H0_n300_Mtot10pct <- data.frame(
#   pvalue = sapply(res_all_n300_Mtot10pct_k20_H0_no_noise, function(res) res$MC$pvalues$pvalue[2]),
#   true_label = 0,
#   n = 300,
#   Mtot = "10pct"
# )
#
# ## Mtot50pct
# df_mc_trait1_H0_n20_Mtot50pct <- data.frame(
#   pvalue = sapply(res_all_n20_Mtot50pct_k20_H0_no_noise, function(res) res$MC$pvalues$pvalue[2]),
#   true_label = 0,
#   n = 20,
#   Mtot = "50pct"
# )
#
# df_mc_trait1_H0_n50_Mtot50pct <- data.frame(
#   pvalue = sapply(res_all_n50_Mtot50pct_k20_H0_no_noise, function(res) res$MC$pvalues$pvalue[2]),
#   true_label = 0,
#   n = 50,
#   Mtot = "50pct"
# )
#
# df_mc_trait1_H0_n100_Mtot50pct <- data.frame(
#   pvalue = sapply(res_all_n100_Mtot50pct_k20_H0_no_noise, function(res) res$MC$pvalues$pvalue[2]),
#   true_label = 0,
#   n = 100,
#   Mtot = "50pct"
# )
#
# df_mc_trait1_H0_n300_Mtot50pct <- data.frame(
#   pvalue = sapply(res_all_n300_Mtot50pct_k20_H0_no_noise, function(res) res$MC$pvalues$pvalue[2]),
#   true_label = 0,
#   n = 300,
#   Mtot = "50pct"
# )
#
#
# df_mc_trait1_no_noise <- rbind(
#   df_mc_trait1_h1_n20_Mtot1pct,
#   df_mc_trait1_h1_n50_Mtot1pct,
#   df_mc_trait1_h1_n100_Mtot1pct,
#   df_mc_trait1_h1_n300_Mtot1pct,
#   df_mc_trait1_h1_n20_Mtot10pct,
#   df_mc_trait1_h1_n50_Mtot10pct,
#   df_mc_trait1_h1_n100_Mtot10pct,
#   df_mc_trait1_h1_n300_Mtot10pct,
#   df_mc_trait1_h1_n20_Mtot50pct,
#   df_mc_trait1_h1_n50_Mtot50pct,
#   df_mc_trait1_h1_n100_Mtot50pct,
#   df_mc_trait1_h1_n300_Mtot50pct,
#   df_mc_trait1_H0_n20_Mtot1pct,
#   df_mc_trait1_H0_n50_Mtot1pct,
#   df_mc_trait1_H0_n100_Mtot1pct,
#   df_mc_trait1_H0_n300_Mtot1pct,
#   df_mc_trait1_H0_n20_Mtot10pct,
#   df_mc_trait1_H0_n50_Mtot10pct,
#   df_mc_trait1_H0_n100_Mtot10pct,
#   df_mc_trait1_H0_n300_Mtot10pct,
#   df_mc_trait1_H0_n20_Mtot50pct,
#   df_mc_trait1_H0_n50_Mtot50pct,
#   df_mc_trait1_H0_n100_Mtot50pct,
#   df_mc_trait1_H0_n300_Mtot50pct
# )
#
# roc_mc_trait1_no_noise <- roc(response = df_mc_trait1_no_noise$true_label,
#                               predictor = df_mc_trait1_no_noise$pvalue + runif(nrow(df_mc_trait1_no_noise), 0, 0.001))
#
# roc_df_mc_trait1_no_noise <- data.frame(
#   specificity = roc_mc_trait1_no_noise$specificities,
#   sensitivity = roc_mc_trait1_no_noise$sensitivities,
#   Method = "MC trait1"
# )
#
#
#
# ################## mutaphy
#
# #### H1
#
# ## Mtot1pct
# df_mutaphy_h1_n20_Mtot1pct <- data.frame(
#   pvalue = sapply(res_all_n20_Mtot1pct_k20_no_noise, function(res) {     res$MutaPhy$tree$min_corr_pvalue   }),
#   true_label = 1,
#   n = 20,
#   Mtot = "1pct"
# )
#
# df_mutaphy_h1_n50_Mtot1pct <- data.frame(
#   pvalue = sapply(res_all_n50_Mtot1pct_k20_no_noise, function(res) {     res$MutaPhy$tree$min_corr_pvalue   }),
#   true_label = 1,
#   n = 50,
#   Mtot = "1pct"
# )
#
# df_mutaphy_h1_n100_Mtot1pct <- data.frame(
#   pvalue = sapply(res_all_n100_Mtot1pct_k20_no_noise, function(res) {     res$MutaPhy$tree$min_corr_pvalue   }),
#   true_label = 1,
#   n = 100,
#   Mtot = "1pct"
# )
#
# df_mutaphy_h1_n300_Mtot1pct <- data.frame(
#   pvalue = sapply(res_all_n300_Mtot1pct_k20_no_noise, function(res) {     res$MutaPhy$tree$min_corr_pvalue   }),
#   true_label = 1,
#   n = 300,
#   Mtot = "1pct"
# )
#
# ## Mtot10pct
# df_mutaphy_h1_n20_Mtot10pct <- data.frame(
#   pvalue = sapply(res_all_n20_Mtot10pct_k20_no_noise, function(res) {     res$MutaPhy$tree$min_corr_pvalue   }),
#   true_label = 1,
#   n = 20,
#   Mtot = "10pct"
# )
#
# df_mutaphy_h1_n50_Mtot10pct <- data.frame(
#   pvalue = sapply(res_all_n50_Mtot10pct_k20_no_noise, function(res) {     res$MutaPhy$tree$min_corr_pvalue   }),
#   true_label = 1,
#   n = 50,
#   Mtot = "10pct"
# )
#
# df_mutaphy_h1_n100_Mtot10pct <- data.frame(
#   pvalue = sapply(res_all_n100_Mtot10pct_k20_no_noise, function(res) {     res$MutaPhy$tree$min_corr_pvalue   }),
#   true_label = 1,
#   n = 100,
#   Mtot = "10pct"
# )
#
# df_mutaphy_h1_n300_Mtot10pct <- data.frame(
#   pvalue = sapply(res_all_n300_Mtot10pct_k20_no_noise, function(res) {     res$MutaPhy$tree$min_corr_pvalue   }),
#   true_label = 1,
#   n = 300,
#   Mtot = "10pct"
# )
#
# ## Mtot50pct
# df_mutaphy_h1_n20_Mtot50pct <- data.frame(
#   pvalue = sapply(res_all_n20_Mtot50pct_k20_no_noise, function(res) {     res$MutaPhy$tree$min_corr_pvalue   }),
#   true_label = 1,
#   n = 20,
#   Mtot = "50pct"
# )
#
# df_mutaphy_h1_n50_Mtot50pct <- data.frame(
#   pvalue = sapply(res_all_n50_Mtot50pct_k20_no_noise, function(res) {     res$MutaPhy$tree$min_corr_pvalue   }),
#   true_label = 1,
#   n = 50,
#   Mtot = "50pct"
# )
#
# df_mutaphy_h1_n100_Mtot50pct <- data.frame(
#   pvalue = sapply(res_all_n100_Mtot50pct_k20_no_noise, function(res) {     res$MutaPhy$tree$min_corr_pvalue   }),
#   true_label = 1,
#   n = 100,
#   Mtot = "50pct"
# )
#
# df_mutaphy_h1_n300_Mtot50pct <- data.frame(
#   pvalue = sapply(res_all_n300_Mtot50pct_k20_no_noise, function(res) {     res$MutaPhy$tree$min_corr_pvalue   }),
#   true_label = 1,
#   n = 300,
#   Mtot = "50pct"
# )
#
# #### H0
#
# ## Mtot1pct
# df_mutaphy_H0_n20_Mtot1pct <- data.frame(
#   pvalue = sapply(res_all_n20_Mtot1pct_k20_H0_no_noise, function(res) {     res$MutaPhy$tree$min_corr_pvalue   }),
#   true_label = 0,
#   n = 20,
#   Mtot = "1pct"
# )
#
# df_mutaphy_H0_n50_Mtot1pct <- data.frame(
#   pvalue = sapply(res_all_n50_Mtot1pct_k20_H0_no_noise, function(res) {     res$MutaPhy$tree$min_corr_pvalue   }),
#   true_label = 0,
#   n = 50,
#   Mtot = "1pct"
# )
#
# df_mutaphy_H0_n100_Mtot1pct <- data.frame(
#   pvalue = sapply(res_all_n100_Mtot1pct_k20_H0_no_noise, function(res) {     res$MutaPhy$tree$min_corr_pvalue   }),
#   true_label = 0,
#   n = 100,
#   Mtot = "1pct"
# )
#
# df_mutaphy_H0_n300_Mtot1pct <- data.frame(
#   pvalue = sapply(res_all_n300_Mtot1pct_k20_H0_no_noise, function(res) {     res$MutaPhy$tree$min_corr_pvalue   }),
#   true_label = 0,
#   n = 300,
#   Mtot = "1pct"
# )
#
# ## Mtot10pct
# df_mutaphy_H0_n20_Mtot10pct <- data.frame(
#   pvalue = sapply(res_all_n20_Mtot10pct_k20_H0_no_noise, function(res) {     res$MutaPhy$tree$min_corr_pvalue   }),
#   true_label = 0,
#   n = 20,
#   Mtot = "10pct"
# )
#
# df_mutaphy_H0_n50_Mtot10pct <- data.frame(
#   pvalue = sapply(res_all_n50_Mtot10pct_k20_H0_no_noise, function(res) {     res$MutaPhy$tree$min_corr_pvalue   }),
#   true_label = 0,
#   n = 50,
#   Mtot = "10pct"
# )
#
# df_mutaphy_H0_n100_Mtot10pct <- data.frame(
#   pvalue = sapply(res_all_n100_Mtot10pct_k20_H0_no_noise, function(res) {     res$MutaPhy$tree$min_corr_pvalue   }),
#   true_label = 0,
#   n = 100,
#   Mtot = "10pct"
# )
#
# df_mutaphy_H0_n300_Mtot10pct <- data.frame(
#   pvalue = sapply(res_all_n300_Mtot10pct_k20_H0_no_noise, function(res) {     res$MutaPhy$tree$min_corr_pvalue   }),
#   true_label = 0,
#   n = 300,
#   Mtot = "10pct"
# )
#
# ## Mtot50pct
# df_mutaphy_H0_n20_Mtot50pct <- data.frame(
#   pvalue = sapply(res_all_n20_Mtot50pct_k20_H0_no_noise, function(res) {     res$MutaPhy$tree$min_corr_pvalue   }),
#   true_label = 0,
#   n = 20,
#   Mtot = "50pct"
# )
#
# df_mutaphy_H0_n50_Mtot50pct <- data.frame(
#   pvalue = sapply(res_all_n50_Mtot50pct_k20_H0_no_noise, function(res) {     res$MutaPhy$tree$min_corr_pvalue   }),
#   true_label = 0,
#   n = 50,
#   Mtot = "50pct"
# )
#
# df_mutaphy_H0_n100_Mtot50pct <- data.frame(
#   pvalue = sapply(res_all_n100_Mtot50pct_k20_H0_no_noise, function(res) {     res$MutaPhy$tree$min_corr_pvalue   }),
#   true_label = 0,
#   n = 100,
#   Mtot = "50pct"
# )
#
# df_mutaphy_H0_n300_Mtot50pct <- data.frame(
#   pvalue = sapply(res_all_n300_Mtot50pct_k20_H0_no_noise, function(res) {     res$MutaPhy$tree$min_corr_pvalue   }),
#   true_label = 0,
#   n = 300,
#   Mtot = "50pct"
# )
#
#
# df_mutaphy_no_noise <- rbind(
#   df_mutaphy_h1_n20_Mtot1pct,
#   df_mutaphy_h1_n50_Mtot1pct,
#   df_mutaphy_h1_n100_Mtot1pct,
#   df_mutaphy_h1_n300_Mtot1pct,
#   df_mutaphy_h1_n20_Mtot10pct,
#   df_mutaphy_h1_n50_Mtot10pct,
#   df_mutaphy_h1_n100_Mtot10pct,
#   df_mutaphy_h1_n300_Mtot10pct,
#   df_mutaphy_h1_n20_Mtot50pct,
#   df_mutaphy_h1_n50_Mtot50pct,
#   df_mutaphy_h1_n100_Mtot50pct,
#   df_mutaphy_h1_n300_Mtot50pct,
#   df_mutaphy_H0_n20_Mtot1pct,
#   df_mutaphy_H0_n50_Mtot1pct,
#   df_mutaphy_H0_n100_Mtot1pct,
#   df_mutaphy_H0_n300_Mtot1pct,
#   df_mutaphy_H0_n20_Mtot10pct,
#   df_mutaphy_H0_n50_Mtot10pct,
#   df_mutaphy_H0_n100_Mtot10pct,
#   df_mutaphy_H0_n300_Mtot10pct,
#   df_mutaphy_H0_n20_Mtot50pct,
#   df_mutaphy_H0_n50_Mtot50pct,
#   df_mutaphy_H0_n100_Mtot50pct,
#   df_mutaphy_H0_n300_Mtot50pct
# )
#
# roc_mutaphy_no_noise <- roc(response = df_mutaphy_no_noise$true_label,
#                             predictor = df_mutaphy_no_noise$pvalue )
#
# roc_df_mutaphy_no_noise <- data.frame(
#   specificity = roc_mutaphy_no_noise$specificities,
#   sensitivity = roc_mutaphy_no_noise$sensitivities,
#   Method = "MutaPhy"
# )
#
# ################## crp-tree
#
# #### H1
#
# ## Mtot1pct
# df_crp_tree_h1_n20_Mtot1pct <- data.frame(
#   pvalue = sapply(res_all_n20_Mtot1pct_k20_no_noise, function(res) {     tryCatch({       res[["CRPTree"]][["pval_tree"]]     }, error = function(e) NA)   }),
#   true_label = 1,
#   n = 20,
#   Mtot = "1pct"
# )
#
# df_crp_tree_h1_n50_Mtot1pct <- data.frame(
#   pvalue = sapply(res_all_n50_Mtot1pct_k20_no_noise, function(res) {     tryCatch({       res[["CRPTree"]][["pval_tree"]]     }, error = function(e) NA)   }),
#   true_label = 1,
#   n = 50,
#   Mtot = "1pct"
# )
#
# df_crp_tree_h1_n100_Mtot1pct <- data.frame(
#   pvalue = sapply(res_all_n100_Mtot1pct_k20_no_noise, function(res) {     tryCatch({       res[["CRPTree"]][["pval_tree"]]     }, error = function(e) NA)   }),
#   true_label = 1,
#   n = 100,
#   Mtot = "1pct"
# )
#
# df_crp_tree_h1_n300_Mtot1pct <- data.frame(
#   pvalue = sapply(res_all_n300_Mtot1pct_k20_no_noise, function(res) {     tryCatch({       res[["CRPTree"]][["pval_tree"]]     }, error = function(e) NA)   }),
#   true_label = 1,
#   n = 300,
#   Mtot = "1pct"
# )
#
# ## Mtot10pct
# df_crp_tree_h1_n20_Mtot10pct <- data.frame(
#   pvalue = sapply(res_all_n20_Mtot10pct_k20_no_noise, function(res) {     tryCatch({       res[["CRPTree"]][["pval_tree"]]     }, error = function(e) NA)   }),
#   true_label = 1,
#   n = 20,
#   Mtot = "10pct"
# )
#
#
# df_crp_tree_h1_n50_Mtot10pct <- data.frame(
#   pvalue = sapply(res_all_n50_Mtot10pct_k20_no_noise, function(res) {     tryCatch({       res[["CRPTree"]][["pval_tree"]]     }, error = function(e) NA)   }),
#   true_label = 1,
#   n = 50,
#   Mtot = "10pct"
# )
#
# df_crp_tree_h1_n100_Mtot10pct <- data.frame(
#   pvalue = sapply(res_all_n100_Mtot10pct_k20_no_noise, function(res) {     tryCatch({       res[["CRPTree"]][["pval_tree"]]     }, error = function(e) NA)   }),
#   true_label = 1,
#   n = 100,
#   Mtot = "10pct"
# )
#
# df_crp_tree_h1_n300_Mtot10pct <- data.frame(
#   pvalue = sapply(res_all_n300_Mtot10pct_k20_no_noise, function(res) {     tryCatch({       res[["CRPTree"]][["pval_tree"]]     }, error = function(e) NA)   }),
#   true_label = 1,
#   n = 300,
#   Mtot = "10pct"
# )
#
# ## Mtot50pct
# df_crp_tree_h1_n20_Mtot50pct <- data.frame(
#   pvalue = sapply(res_all_n20_Mtot50pct_k20_no_noise, function(res) {     tryCatch({       res[["CRPTree"]][["pval_tree"]]     }, error = function(e) NA)   }),
#   true_label = 1,
#   n = 20,
#   Mtot = "50pct"
# )
#
# df_crp_tree_h1_n50_Mtot50pct <- data.frame(
#   pvalue = sapply(res_all_n50_Mtot50pct_k20_no_noise, function(res) {     tryCatch({       res[["CRPTree"]][["pval_tree"]]     }, error = function(e) NA)   }),
#   true_label = 1,
#   n = 50,
#   Mtot = "50pct"
# )
#
# df_crp_tree_h1_n100_Mtot50pct <- data.frame(
#   pvalue = sapply(res_all_n100_Mtot50pct_k20_no_noise, function(res) {     tryCatch({       res[["CRPTree"]][["pval_tree"]]     }, error = function(e) NA)   }),
#   true_label = 1,
#   n = 100,
#   Mtot = "50pct"
# )
#
# df_crp_tree_h1_n300_Mtot50pct <- data.frame(
#   pvalue = sapply(res_all_n300_Mtot50pct_k20_no_noise, function(res) {     tryCatch({       res[["CRPTree"]][["pval_tree"]]     }, error = function(e) NA)   }),
#   true_label = 1,
#   n = 300,
#   Mtot = "50pct"
# )
#
# #### H0
#
# ## Mtot1pct
# df_crp_tree_H0_n20_Mtot1pct <- data.frame(
#   pvalue = sapply(res_all_n20_Mtot1pct_k20_H0_no_noise, function(res) {     tryCatch({       res[["CRPTree"]][["pval_tree"]]     }, error = function(e) NA)   }),
#   true_label = 0,
#   n = 20,
#   Mtot = "1pct"
# )
#
# df_crp_tree_H0_n50_Mtot1pct <- data.frame(
#   pvalue = sapply(res_all_n50_Mtot1pct_k20_H0_no_noise, function(res) {     tryCatch({       res[["CRPTree"]][["pval_tree"]]     }, error = function(e) NA)   }),
#   true_label = 0,
#   n = 50,
#   Mtot = "1pct"
# )
#
# df_crp_tree_H0_n100_Mtot1pct <- data.frame(
#   pvalue = sapply(res_all_n100_Mtot1pct_k20_H0_no_noise, function(res) {     tryCatch({       res[["CRPTree"]][["pval_tree"]]     }, error = function(e) NA)   }),
#   true_label = 0,
#   n = 100,
#   Mtot = "1pct"
# )
#
# df_crp_tree_H0_n300_Mtot1pct <- data.frame(
#   pvalue = sapply(res_all_n300_Mtot1pct_k20_H0_no_noise, function(res) {     tryCatch({       res[["CRPTree"]][["pval_tree"]]     }, error = function(e) NA)   }),
#   true_label = 0,
#   n = 300,
#   Mtot = "1pct"
# )
#
# ## Mtot10pct
# df_crp_tree_H0_n20_Mtot10pct <- data.frame(
#   pvalue = sapply(res_all_n20_Mtot10pct_k20_H0_no_noise, function(res) {     tryCatch({       res[["CRPTree"]][["pval_tree"]]     }, error = function(e) NA)   }),
#   true_label = 0,
#   n = 20,
#   Mtot = "10pct"
# )
#
# df_crp_tree_H0_n50_Mtot10pct <- data.frame(
#   pvalue = sapply(res_all_n50_Mtot10pct_k20_H0_no_noise, function(res) {     tryCatch({       res[["CRPTree"]][["pval_tree"]]     }, error = function(e) NA)   }),
#   true_label = 0,
#   n = 50,
#   Mtot = "10pct"
# )
#
# df_crp_tree_H0_n100_Mtot10pct <- data.frame(
#   pvalue = sapply(res_all_n100_Mtot10pct_k20_H0_no_noise, function(res) {     tryCatch({       res[["CRPTree"]][["pval_tree"]]     }, error = function(e) NA)   }),
#   true_label = 0,
#   n = 100,
#   Mtot = "10pct"
# )
#
# df_crp_tree_H0_n300_Mtot10pct <- data.frame(
#   pvalue = sapply(res_all_n300_Mtot10pct_k20_H0_no_noise, function(res) {     tryCatch({       res[["CRPTree"]][["pval_tree"]]     }, error = function(e) NA)   }),
#   true_label = 0,
#   n = 300,
#   Mtot = "10pct"
# )
#
# ## Mtot50pct
# df_crp_tree_H0_n20_Mtot50pct <- data.frame(
#   pvalue = sapply(res_all_n20_Mtot50pct_k20_H0_no_noise, function(res) {     tryCatch({       res[["CRPTree"]][["pval_tree"]]     }, error = function(e) NA)   }),
#   true_label = 0,
#   n = 20,
#   Mtot = "50pct"
# )
#
# df_crp_tree_H0_n50_Mtot50pct <- data.frame(
#   pvalue = sapply(res_all_n50_Mtot50pct_k20_H0_no_noise, function(res) {     tryCatch({       res[["CRPTree"]][["pval_tree"]]     }, error = function(e) NA)   }),
#   true_label = 0,
#   n = 50,
#   Mtot = "50pct"
# )
#
# df_crp_tree_H0_n100_Mtot50pct <- data.frame(
#   pvalue = sapply(res_all_n100_Mtot50pct_k20_H0_no_noise, function(res) {     tryCatch({       res[["CRPTree"]][["pval_tree"]]     }, error = function(e) NA)   }),
#   true_label = 0,
#   n = 100,
#   Mtot = "50pct"
# )
#
# df_crp_tree_H0_n300_Mtot50pct <- data.frame(
#   pvalue = sapply(res_all_n300_Mtot50pct_k20_H0_no_noise, function(res) {     tryCatch({       res[["CRPTree"]][["pval_tree"]]     }, error = function(e) NA)   }),
#   true_label = 0,
#   n = 300,
#   Mtot = "50pct"
# )
#
#
# df_crp_tree_no_noise <- rbind(
#   df_crp_tree_h1_n20_Mtot1pct,
#   df_crp_tree_h1_n50_Mtot1pct,
#   df_crp_tree_h1_n100_Mtot1pct,
#   df_crp_tree_h1_n300_Mtot1pct,
#   df_crp_tree_h1_n20_Mtot10pct,
#   df_crp_tree_h1_n50_Mtot10pct,
#   df_crp_tree_h1_n100_Mtot10pct,
#   df_crp_tree_h1_n300_Mtot10pct,
#   df_crp_tree_h1_n20_Mtot50pct,
#   df_crp_tree_h1_n50_Mtot50pct,
#   df_crp_tree_h1_n100_Mtot50pct,
#   df_crp_tree_h1_n300_Mtot50pct,
#   df_crp_tree_H0_n20_Mtot1pct,
#   df_crp_tree_H0_n50_Mtot1pct,
#   df_crp_tree_H0_n100_Mtot1pct,
#   df_crp_tree_H0_n300_Mtot1pct,
#   df_crp_tree_H0_n20_Mtot10pct,
#   df_crp_tree_H0_n50_Mtot10pct,
#   df_crp_tree_H0_n100_Mtot10pct,
#   df_crp_tree_H0_n300_Mtot10pct,
#   df_crp_tree_H0_n20_Mtot50pct,
#   df_crp_tree_H0_n50_Mtot50pct,
#   df_crp_tree_H0_n100_Mtot50pct,
#   df_crp_tree_H0_n300_Mtot50pct
# )
#
# roc_crp_tree_no_noise <- roc(response = df_crp_tree_no_noise$true_label,
#                              predictor = df_crp_tree_no_noise$pvalue)
#
# roc_df_crp_tree_no_noise <- data.frame(
#   specificity = roc_crp_tree_no_noise$specificities,
#   sensitivity = roc_crp_tree_no_noise$sensitivities,
#   Method = "CRP-Tree"
# )
#
#
#
# # roc_df_all_no_noise <- bind_rows(roc_df_ai_no_noise,
# #                                             #roc_df_ps_no_noise,
# #                                             #roc_df_mc_trait0_no_noise,
# #                                             #roc_df_mc_trait1_no_noise,
# #                                             roc_df_mutaphy_no_noise)
#
# auc_ai <- round(auc(roc_ai_no_noise),3)
# auc_mutaphy <- round(auc(roc_mutaphy_no_noise),3)
# auc_ps <- round(auc(roc_ps_no_noise),3)
# auc_mc_trait0 <- round(auc(roc_mc_trait0_no_noise),3)
# auc_mc_trait1 <- round(auc(roc_mc_trait1_no_noise),3)
# auc_crp_tree <- round(auc(roc_crp_tree_no_noise),3)
#
# roc_list <- list(
#   "AI" = roc_ai_no_noise,
#   "PS" = roc_ps_no_noise,
#   "MC trait0" = roc_mc_trait0_no_noise,
#   "MC trait1" = roc_mc_trait1_no_noise,
#   "CRP-Tree" = roc_crp_tree_no_noise,
#   "MutaPhy" = roc_mutaphy_no_noise
# )
#
#
#
# colors <- c("#009E60",
#             "#5E3C99",
#             "#ADD8E6",
#             "#6495ED",
#             "#FFAC1C",
#             "#C70039"
# )
#
# roc_plot <- ggroc(roc_list, size = 1.2) +
#   scale_color_manual(
#     name = "Method",
#     values = colors,
#     labels = c(
#       paste0("AI (AUC = ", auc_ai, ")"),
#       paste0("PS (AUC = ", auc_ps, ")"),
#       paste0("MC trait0 (AUC = ", auc_mc_trait0, ")"),
#       paste0("MC trait1 (AUC = ", auc_mc_trait1, ")"),
#       paste0("CRP-Tree (AUC = ", auc_crp_tree, ")"),
#       paste0("MutaPhy (AUC = ", auc_mutaphy, ")")
#     )
#   ) +
#   labs(
#     title = "ROC curves (no noise)",
#     x = "Specificity",
#     y = "Sensitivity"
#   ) +
#   theme_minimal(base_size = 14) +
#   theme(
#     plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
#     axis.title.x = element_text(size = 13, face = "bold"),
#     axis.title.y = element_text(size = 13, face = "bold"),
#     axis.text = element_text(size = 11),
#     legend.position = c(0.65, 0.25),
#     legend.title = element_text(size = 11),
#     legend.text = element_text(size = 10),
#     panel.grid.major = element_line(color = "grey90"),
#     panel.grid.minor = element_blank()
#   ) +
#   geom_abline(slope = 1, intercept = 1, linetype = "dashed", color = "grey40") +
#   coord_equal()

print(roc_plot)

##### Noise
#
# ################## AI
#
# #### H1
#
# ## Mtot1pct_noise
# df_ai_h1_n20_Mtot1pct_noise <- data.frame(
#   pvalue = sapply(res_all_n20_Mtot1pct_k20_noise, function(res) res$AI$pvalue),
#   true_label = 1,
#   n = 20,
#   Mtot = "1pct"
# )
#
# df_ai_h1_n50_Mtot1pct_noise <- data.frame(
#   pvalue = sapply(res_all_n50_Mtot1pct_k20_noise, function(res) res$AI$pvalue),
#   true_label = 1,
#   n = 50,
#   Mtot = "1pct"
# )
#
# df_ai_h1_n100_Mtot1pct_noise <- data.frame(
#   pvalue = sapply(res_all_n100_Mtot1pct_k20_noise, function(res) res$AI$pvalue),
#   true_label = 1,
#   n = 100,
#   Mtot = "1pct"
# )
#
# df_ai_h1_n300_Mtot1pct_noise <- data.frame(
#   pvalue = sapply(res_all_n300_Mtot1pct_k20_noise, function(res) res$AI$pvalue),
#   true_label = 1,
#   n = 300,
#   Mtot = "1pct"
# )
#
# ## Mtot10pct_noise
# df_ai_h1_n20_Mtot10pct_noise <- data.frame(
#   pvalue = sapply(res_all_n20_Mtot10pct_k20_noise, function(res) res$AI$pvalue),
#   true_label = 1,
#   n = 20,
#   Mtot = "10pct"
# )
#
# df_ai_h1_n50_Mtot10pct_noise <- data.frame(
#   pvalue = sapply(res_all_n50_Mtot10pct_k20_noise, function(res) res$AI$pvalue),
#   true_label = 1,
#   n = 50,
#   Mtot = "10pct"
# )
#
# df_ai_h1_n100_Mtot10pct_noise <- data.frame(
#   pvalue = sapply(res_all_n100_Mtot10pct_k20_noise, function(res) res$AI$pvalue),
#   true_label = 1,
#   n = 100,
#   Mtot = "10pct"
# )
#
# df_ai_h1_n300_Mtot10pct_noise <- data.frame(
#   pvalue = sapply(res_all_n300_Mtot10pct_k20_noise, function(res) res$AI$pvalue),
#   true_label = 1,
#   n = 300,
#   Mtot = "10pct"
# )
#
# ## Mtot50pct_noise
# df_ai_h1_n20_Mtot50pct_noise <- data.frame(
#   pvalue = sapply(res_all_n20_Mtot50pct_k20_noise, function(res) res$AI$pvalue),
#   true_label = 1,
#   n = 20,
#   Mtot = "50pct"
# )
#
# df_ai_h1_n50_Mtot50pct_noise <- data.frame(
#   pvalue = sapply(res_all_n50_Mtot50pct_k20_noise, function(res) res$AI$pvalue),
#   true_label = 1,
#   n = 50,
#   Mtot = "50pct"
# )
#
# df_ai_h1_n100_Mtot50pct_noise <- data.frame(
#   pvalue = sapply(res_all_n100_Mtot50pct_k20_noise, function(res) res$AI$pvalue),
#   true_label = 1,
#   n = 100,
#   Mtot = "50pct"
# )
#
# df_ai_h1_n300_Mtot50pct_noise <- data.frame(
#   pvalue = sapply(res_all_n300_Mtot50pct_k20_noise, function(res) res$AI$pvalue),
#   true_label = 1,
#   n = 300,
#   Mtot = "50pct"
# )
#
# #### H0
#
# ## Mtot1pct_noise
# df_ai_h0_n20_Mtot1pct_noise <- data.frame(
#   pvalue = sapply(res_all_n20_Mtot1pct_k20_H0_noise, function(res) res$AI$pvalue),
#   true_label = 0,
#   n = 20,
#   Mtot = "1pct"
# )
#
# df_ai_h0_n50_Mtot1pct_noise <- data.frame(
#   pvalue = sapply(res_all_n50_Mtot1pct_k20_H0_noise, function(res) res$AI$pvalue),
#   true_label = 0,
#   n = 50,
#   Mtot = "1pct"
# )
#
# df_ai_h0_n100_Mtot1pct_noise <- data.frame(
#   pvalue = sapply(res_all_n100_Mtot1pct_k20_H0_noise, function(res) res$AI$pvalue),
#   true_label = 0,
#   n = 100,
#   Mtot = "1pct"
# )
#
# df_ai_h0_n300_Mtot1pct_noise <- data.frame(
#   pvalue = sapply(res_all_n300_Mtot1pct_k20_H0_noise, function(res) res$AI$pvalue),
#   true_label = 0,
#   n = 300,
#   Mtot = "1pct"
# )
#
# ## Mtot10pct_noise
# df_ai_h0_n20_Mtot10pct_noise <- data.frame(
#   pvalue = sapply(res_all_n20_Mtot10pct_k20_H0_noise, function(res) res$AI$pvalue),
#   true_label = 0,
#   n = 20,
#   Mtot = "10pct"
# )
#
# df_ai_h0_n50_Mtot10pct_noise <- data.frame(
#   pvalue = sapply(res_all_n50_Mtot10pct_k20_H0_noise, function(res) res$AI$pvalue),
#   true_label = 0,
#   n = 50,
#   Mtot = "10pct"
# )
#
# df_ai_h0_n100_Mtot10pct_noise <- data.frame(
#   pvalue = sapply(res_all_n100_Mtot10pct_k20_H0_noise, function(res) res$AI$pvalue),
#   true_label = 0,
#   n = 100,
#   Mtot = "10pct"
# )
#
# df_ai_h0_n300_Mtot10pct_noise <- data.frame(
#   pvalue = sapply(res_all_n300_Mtot10pct_k20_H0_noise, function(res) res$AI$pvalue),
#   true_label = 0,
#   n = 300,
#   Mtot = "10pct"
# )
#
# ## Mtot50pct_noise
# df_ai_h0_n20_Mtot50pct_noise <- data.frame(
#   pvalue = sapply(res_all_n20_Mtot50pct_k20_H0_noise, function(res) res$AI$pvalue),
#   true_label = 0,
#   n = 20,
#   Mtot = "50pct"
# )
#
# df_ai_h0_n50_Mtot50pct_noise <- data.frame(
#   pvalue = sapply(res_all_n50_Mtot50pct_k20_H0_noise, function(res) res$AI$pvalue),
#   true_label = 0,
#   n = 50,
#   Mtot = "50pct"
# )
#
# df_ai_h0_n100_Mtot50pct_noise <- data.frame(
#   pvalue = sapply(res_all_n100_Mtot50pct_k20_H0_noise, function(res) res$AI$pvalue),
#   true_label = 0,
#   n = 100,
#   Mtot = "50pct"
# )
#
# df_ai_h0_n300_Mtot50pct_noise <- data.frame(
#   pvalue = sapply(res_all_n300_Mtot50pct_k20_H0_noise, function(res) res$AI$pvalue),
#   true_label = 0,
#   n = 300,
#   Mtot = "50pct"
# )
#
#
# df_ai_noise <- rbind(
#   df_ai_h1_n20_Mtot1pct_noise,
#   df_ai_h1_n50_Mtot1pct_noise,
#   df_ai_h1_n100_Mtot1pct_noise,
#   df_ai_h1_n300_Mtot1pct_noise,
#   df_ai_h1_n20_Mtot10pct_noise,
#   df_ai_h1_n50_Mtot10pct_noise,
#   df_ai_h1_n100_Mtot10pct_noise,
#   df_ai_h1_n300_Mtot10pct_noise,
#   df_ai_h1_n20_Mtot50pct_noise,
#   df_ai_h1_n50_Mtot50pct_noise,
#   df_ai_h1_n100_Mtot50pct_noise,
#   df_ai_h1_n300_Mtot50pct_noise,
#   df_ai_h0_n20_Mtot1pct_noise,
#   df_ai_h0_n50_Mtot1pct_noise,
#   df_ai_h0_n100_Mtot1pct_noise,
#   df_ai_h0_n300_Mtot1pct_noise,
#   df_ai_h0_n20_Mtot10pct_noise,
#   df_ai_h0_n50_Mtot10pct_noise,
#   df_ai_h0_n100_Mtot10pct_noise,
#   df_ai_h0_n300_Mtot10pct_noise,
#   df_ai_h0_n20_Mtot50pct_noise,
#   df_ai_h0_n50_Mtot50pct_noise,
#   df_ai_h0_n100_Mtot50pct_noise,
#   df_ai_h0_n300_Mtot50pct_noise
# )
#
#
#
# roc_ai_noise <- roc(response = df_ai_noise$true_label,
#                     predictor = df_ai_noise$pvalue)
#
# roc_df_ai_noise <- data.frame(
#   specificity = roc_ai_noise$specificities,
#   sensitivity = roc_ai_noise$sensitivities,
#   Method = "AI"
# )
#
# ################## PS
#
# #### H1
#
# ## Mtot1pct_noise
# df_ps_h1_n20_Mtot1pct_noise <- data.frame(
#   pvalue = sapply(res_all_n20_Mtot1pct_k20_noise, function(res) res$PS$pvalue),
#   true_label = 1,
#   n = 20,
#   Mtot = "1pct"
# )
#
# df_ps_h1_n50_Mtot1pct_noise <- data.frame(
#   pvalue = sapply(res_all_n50_Mtot1pct_k20_noise, function(res) res$PS$pvalue),
#   true_label = 1,
#   n = 50,
#   Mtot = "1pct"
# )
#
# df_ps_h1_n100_Mtot1pct_noise <- data.frame(
#   pvalue = sapply(res_all_n100_Mtot1pct_k20_noise, function(res) res$PS$pvalue),
#   true_label = 1,
#   n = 100,
#   Mtot = "1pct"
# )
#
# df_ps_h1_n300_Mtot1pct_noise <- data.frame(
#   pvalue = sapply(res_all_n300_Mtot1pct_k20_noise, function(res) res$PS$pvalue),
#   true_label = 1,
#   n = 300,
#   Mtot = "1pct"
# )
#
# ## Mtot10pct_noise
# df_ps_h1_n20_Mtot10pct_noise <- data.frame(
#   pvalue = sapply(res_all_n20_Mtot10pct_k20_noise, function(res) res$PS$pvalue),
#   true_label = 1,
#   n = 20,
#   Mtot = "10pct"
# )
#
# df_ps_h1_n50_Mtot10pct_noise <- data.frame(
#   pvalue = sapply(res_all_n50_Mtot10pct_k20_noise, function(res) res$PS$pvalue),
#   true_label = 1,
#   n = 50,
#   Mtot = "10pct"
# )
#
# df_ps_h1_n100_Mtot10pct_noise <- data.frame(
#   pvalue = sapply(res_all_n100_Mtot10pct_k20_noise, function(res) res$PS$pvalue),
#   true_label = 1,
#   n = 100,
#   Mtot = "10pct"
# )
#
# df_ps_h1_n300_Mtot10pct_noise <- data.frame(
#   pvalue = sapply(res_all_n300_Mtot10pct_k20_noise, function(res) res$PS$pvalue),
#   true_label = 1,
#   n = 300,
#   Mtot = "10pct"
# )
#
# ## Mtot50pct_noise
# df_ps_h1_n20_Mtot50pct_noise <- data.frame(
#   pvalue = sapply(res_all_n20_Mtot50pct_k20_noise, function(res) res$PS$pvalue),
#   true_label = 1,
#   n = 20,
#   Mtot = "50pct"
# )
#
# df_ps_h1_n50_Mtot50pct_noise <- data.frame(
#   pvalue = sapply(res_all_n50_Mtot50pct_k20_noise, function(res) res$PS$pvalue),
#   true_label = 1,
#   n = 50,
#   Mtot = "50pct"
# )
#
# df_ps_h1_n100_Mtot50pct_noise <- data.frame(
#   pvalue = sapply(res_all_n100_Mtot50pct_k20_noise, function(res) res$PS$pvalue),
#   true_label = 1,
#   n = 100,
#   Mtot = "50pct"
# )
#
# df_ps_h1_n300_Mtot50pct_noise <- data.frame(
#   pvalue = sapply(res_all_n300_Mtot50pct_k20_noise, function(res) res$PS$pvalue),
#   true_label = 1,
#   n = 300,
#   Mtot = "50pct"
# )
#
# #### H0
#
# ## Mtot1pct_noise
# df_ps_h0_n20_Mtot1pct_noise <- data.frame(
#   pvalue = sapply(res_all_n20_Mtot1pct_k20_H0_noise, function(res) res$PS$pvalue),
#   true_label = 0,
#   n = 20,
#   Mtot = "1pct"
# )
#
# df_ps_h0_n50_Mtot1pct_noise <- data.frame(
#   pvalue = sapply(res_all_n50_Mtot1pct_k20_H0_noise, function(res) res$PS$pvalue),
#   true_label = 0,
#   n = 50,
#   Mtot = "1pct"
# )
#
# df_ps_h0_n100_Mtot1pct_noise <- data.frame(
#   pvalue = sapply(res_all_n100_Mtot1pct_k20_H0_noise, function(res) res$PS$pvalue),
#   true_label = 0,
#   n = 100,
#   Mtot = "1pct"
# )
#
# df_ps_h0_n300_Mtot1pct_noise <- data.frame(
#   pvalue = sapply(res_all_n300_Mtot1pct_k20_H0_noise, function(res) res$PS$pvalue),
#   true_label = 0,
#   n = 300,
#   Mtot = "1pct"
# )
#
# ## Mtot10pct_noise
# df_ps_h0_n20_Mtot10pct_noise <- data.frame(
#   pvalue = sapply(res_all_n20_Mtot10pct_k20_H0_noise, function(res) res$PS$pvalue),
#   true_label = 0,
#   n = 20,
#   Mtot = "10pct"
# )
#
# df_ps_h0_n50_Mtot10pct_noise <- data.frame(
#   pvalue = sapply(res_all_n50_Mtot10pct_k20_H0_noise, function(res) res$PS$pvalue),
#   true_label = 0,
#   n = 50,
#   Mtot = "10pct"
# )
#
# df_ps_h0_n100_Mtot10pct_noise <- data.frame(
#   pvalue = sapply(res_all_n100_Mtot10pct_k20_H0_noise, function(res) res$PS$pvalue),
#   true_label = 0,
#   n = 100,
#   Mtot = "10pct"
# )
#
# df_ps_h0_n300_Mtot10pct_noise <- data.frame(
#   pvalue = sapply(res_all_n300_Mtot10pct_k20_H0_noise, function(res) res$PS$pvalue),
#   true_label = 0,
#   n = 300,
#   Mtot = "10pct"
# )
#
# ## Mtot50pct_noise
# df_ps_h0_n20_Mtot50pct_noise <- data.frame(
#   pvalue = sapply(res_all_n20_Mtot50pct_k20_H0_noise, function(res) res$PS$pvalue),
#   true_label = 0,
#   n = 20,
#   Mtot = "50pct"
# )
#
# df_ps_h0_n50_Mtot50pct_noise <- data.frame(
#   pvalue = sapply(res_all_n50_Mtot50pct_k20_H0_noise, function(res) res$PS$pvalue),
#   true_label = 0,
#   n = 50,
#   Mtot = "50pct"
# )
#
# df_ps_h0_n100_Mtot50pct_noise <- data.frame(
#   pvalue = sapply(res_all_n100_Mtot50pct_k20_H0_noise, function(res) res$PS$pvalue),
#   true_label = 0,
#   n = 100,
#   Mtot = "50pct"
# )
#
# df_ps_h0_n300_Mtot50pct_noise <- data.frame(
#   pvalue = sapply(res_all_n300_Mtot50pct_k20_H0_noise, function(res) res$PS$pvalue),
#   true_label = 0,
#   n = 300,
#   Mtot = "50pct"
# )
#
#
# df_ps_noise <- rbind(
#   df_ps_h1_n20_Mtot1pct_noise,
#   df_ps_h1_n50_Mtot1pct_noise,
#   df_ps_h1_n100_Mtot1pct_noise,
#   df_ps_h1_n300_Mtot1pct_noise,
#   df_ps_h1_n20_Mtot10pct_noise,
#   df_ps_h1_n50_Mtot10pct_noise,
#   df_ps_h1_n100_Mtot10pct_noise,
#   df_ps_h1_n300_Mtot10pct_noise,
#   df_ps_h1_n20_Mtot50pct_noise,
#   df_ps_h1_n50_Mtot50pct_noise,
#   df_ps_h1_n100_Mtot50pct_noise,
#   df_ps_h1_n300_Mtot50pct_noise,
#   df_ps_h0_n20_Mtot1pct_noise,
#   df_ps_h0_n50_Mtot1pct_noise,
#   df_ps_h0_n100_Mtot1pct_noise,
#   df_ps_h0_n300_Mtot1pct_noise,
#   df_ps_h0_n20_Mtot10pct_noise,
#   df_ps_h0_n50_Mtot10pct_noise,
#   df_ps_h0_n100_Mtot10pct_noise,
#   df_ps_h0_n300_Mtot10pct_noise,
#   df_ps_h0_n20_Mtot50pct_noise,
#   df_ps_h0_n50_Mtot50pct_noise,
#   df_ps_h0_n100_Mtot50pct_noise,
#   df_ps_h0_n300_Mtot50pct_noise
# )
#
# roc_ps_noise <- roc(response = df_ps_noise$true_label,
#                     predictor = df_ps_noise$pvalue + runif(nrow(df_ps_noise), min = 0, max = 0.001))
#
# roc_df_ps_noise <- data.frame(
#   specificity = roc_ps_noise$specificities,
#   sensitivity = roc_ps_noise$sensitivities,
#   Method = "PS"
# )
#
# ################## MC trait0
#
# #### H1
#
# ## Mtot1pct_noise
# df_mc_trait0_h1_n20_Mtot1pct_noise <- data.frame(
#   pvalue = sapply(res_all_n20_Mtot1pct_k20_noise, function(res) res$MC$pvalues$pvalue[1]),
#   true_label = 1,
#   n = 20,
#   Mtot = "1pct"
# )
#
# df_mc_trait0_h1_n50_Mtot1pct_noise <- data.frame(
#   pvalue = sapply(res_all_n50_Mtot1pct_k20_noise, function(res) res$MC$pvalues$pvalue[1]),
#   true_label = 1,
#   n = 50,
#   Mtot = "1pct"
# )
#
# df_mc_trait0_h1_n100_Mtot1pct_noise <- data.frame(
#   pvalue = sapply(res_all_n100_Mtot1pct_k20_noise, function(res) res$MC$pvalues$pvalue[1]),
#   true_label = 1,
#   n = 100,
#   Mtot = "1pct"
# )
#
# df_mc_trait0_h1_n300_Mtot1pct_noise <- data.frame(
#   pvalue = sapply(res_all_n300_Mtot1pct_k20_noise, function(res) res$MC$pvalues$pvalue[1]),
#   true_label = 1,
#   n = 300,
#   Mtot = "1pct"
# )
#
# ## Mtot10pct_noise
# df_mc_trait0_h1_n20_Mtot10pct_noise <- data.frame(
#   pvalue = sapply(res_all_n20_Mtot10pct_k20_noise, function(res) res$MC$pvalues$pvalue[1]),
#   true_label = 1,
#   n = 20,
#   Mtot = "10pct"
# )
#
# df_mc_trait0_h1_n50_Mtot10pct_noise <- data.frame(
#   pvalue = sapply(res_all_n50_Mtot10pct_k20_noise, function(res) res$MC$pvalues$pvalue[1]),
#   true_label = 1,
#   n = 50,
#   Mtot = "10pct"
# )
#
# df_mc_trait0_h1_n100_Mtot10pct_noise <- data.frame(
#   pvalue = sapply(res_all_n100_Mtot10pct_k20_noise, function(res) res$MC$pvalues$pvalue[1]),
#   true_label = 1,
#   n = 100,
#   Mtot = "10pct"
# )
#
# df_mc_trait0_h1_n300_Mtot10pct_noise <- data.frame(
#   pvalue = sapply(res_all_n300_Mtot10pct_k20_noise, function(res) res$MC$pvalues$pvalue[1]),
#   true_label = 1,
#   n = 300,
#   Mtot = "10pct"
# )
#
# ## Mtot50pct_noise
# df_mc_trait0_h1_n20_Mtot50pct_noise <- data.frame(
#   pvalue = sapply(res_all_n20_Mtot50pct_k20_noise, function(res) res$MC$pvalues$pvalue[1]),
#   true_label = 1,
#   n = 20,
#   Mtot = "50pct"
# )
#
# df_mc_trait0_h1_n50_Mtot50pct_noise <- data.frame(
#   pvalue = sapply(res_all_n50_Mtot50pct_k20_noise, function(res) res$MC$pvalues$pvalue[1]),
#   true_label = 1,
#   n = 50,
#   Mtot = "50pct"
# )
#
# df_mc_trait0_h1_n100_Mtot50pct_noise <- data.frame(
#   pvalue = sapply(res_all_n100_Mtot50pct_k20_noise, function(res) res$MC$pvalues$pvalue[1]),
#   true_label = 1,
#   n = 100,
#   Mtot = "50pct"
# )
#
# df_mc_trait0_h1_n300_Mtot50pct_noise <- data.frame(
#   pvalue = sapply(res_all_n300_Mtot50pct_k20_noise, function(res) res$MC$pvalues$pvalue[1]),
#   true_label = 1,
#   n = 300,
#   Mtot = "50pct"
# )
#
# #### H0
#
# ## Mtot1pct_noise
# df_mc_trait0_h0_n20_Mtot1pct_noise <- data.frame(
#   pvalue = sapply(res_all_n20_Mtot1pct_k20_H0_noise, function(res) res$MC$pvalues$pvalue[1]),
#   true_label = 0,
#   n = 20,
#   Mtot = "1pct"
# )
#
# df_mc_trait0_h0_n50_Mtot1pct_noise <- data.frame(
#   pvalue = sapply(res_all_n50_Mtot1pct_k20_H0_noise, function(res) res$MC$pvalues$pvalue[1]),
#   true_label = 0,
#   n = 50,
#   Mtot = "1pct"
# )
#
# df_mc_trait0_h0_n100_Mtot1pct_noise <- data.frame(
#   pvalue = sapply(res_all_n100_Mtot1pct_k20_H0_noise, function(res) res$MC$pvalues$pvalue[1]),
#   true_label = 0,
#   n = 100,
#   Mtot = "1pct"
# )
#
# df_mc_trait0_h0_n300_Mtot1pct_noise <- data.frame(
#   pvalue = sapply(res_all_n300_Mtot1pct_k20_H0_noise, function(res) res$MC$pvalues$pvalue[1]),
#   true_label = 0,
#   n = 300,
#   Mtot = "1pct"
# )
#
# ## Mtot10pct_noise
# df_mc_trait0_h0_n20_Mtot10pct_noise <- data.frame(
#   pvalue = sapply(res_all_n20_Mtot10pct_k20_H0_noise, function(res) res$MC$pvalues$pvalue[1]),
#   true_label = 0,
#   n = 20,
#   Mtot = "10pct"
# )
#
# df_mc_trait0_h0_n50_Mtot10pct_noise <- data.frame(
#   pvalue = sapply(res_all_n50_Mtot10pct_k20_H0_noise, function(res) res$MC$pvalues$pvalue[1]),
#   true_label = 0,
#   n = 50,
#   Mtot = "10pct"
# )
#
# df_mc_trait0_h0_n100_Mtot10pct_noise <- data.frame(
#   pvalue = sapply(res_all_n100_Mtot10pct_k20_H0_noise, function(res) res$MC$pvalues$pvalue[1]),
#   true_label = 0,
#   n = 100,
#   Mtot = "10pct"
# )
#
# df_mc_trait0_h0_n300_Mtot10pct_noise <- data.frame(
#   pvalue = sapply(res_all_n300_Mtot10pct_k20_H0_noise, function(res) res$MC$pvalues$pvalue[1]),
#   true_label = 0,
#   n = 300,
#   Mtot = "10pct"
# )
#
# ## Mtot50pct_noise
# df_mc_trait0_h0_n20_Mtot50pct_noise <- data.frame(
#   pvalue = sapply(res_all_n20_Mtot50pct_k20_H0_noise, function(res) res$MC$pvalues$pvalue[1]),
#   true_label = 0,
#   n = 20,
#   Mtot = "50pct"
# )
#
# df_mc_trait0_h0_n50_Mtot50pct_noise <- data.frame(
#   pvalue = sapply(res_all_n50_Mtot50pct_k20_H0_noise, function(res) res$MC$pvalues$pvalue[1]),
#   true_label = 0,
#   n = 50,
#   Mtot = "50pct"
# )
#
# df_mc_trait0_h0_n100_Mtot50pct_noise <- data.frame(
#   pvalue = sapply(res_all_n100_Mtot50pct_k20_H0_noise, function(res) res$MC$pvalues$pvalue[1]),
#   true_label = 0,
#   n = 100,
#   Mtot = "50pct"
# )
#
# df_mc_trait0_h0_n300_Mtot50pct_noise <- data.frame(
#   pvalue = sapply(res_all_n300_Mtot50pct_k20_H0_noise, function(res) res$MC$pvalues$pvalue[1]),
#   true_label = 0,
#   n = 300,
#   Mtot = "50pct"
# )
#
#
# df_mc_trait0_noise <- rbind(
#   df_mc_trait0_h1_n20_Mtot1pct_noise,
#   df_mc_trait0_h1_n50_Mtot1pct_noise,
#   df_mc_trait0_h1_n100_Mtot1pct_noise,
#   df_mc_trait0_h1_n300_Mtot1pct_noise,
#   df_mc_trait0_h1_n20_Mtot10pct_noise,
#   df_mc_trait0_h1_n50_Mtot10pct_noise,
#   df_mc_trait0_h1_n100_Mtot10pct_noise,
#   df_mc_trait0_h1_n300_Mtot10pct_noise,
#   df_mc_trait0_h1_n20_Mtot50pct_noise,
#   df_mc_trait0_h1_n50_Mtot50pct_noise,
#   df_mc_trait0_h1_n100_Mtot50pct_noise,
#   df_mc_trait0_h1_n300_Mtot50pct_noise,
#   df_mc_trait0_h0_n20_Mtot1pct_noise,
#   df_mc_trait0_h0_n50_Mtot1pct_noise,
#   df_mc_trait0_h0_n100_Mtot1pct_noise,
#   df_mc_trait0_h0_n300_Mtot1pct_noise,
#   df_mc_trait0_h0_n20_Mtot10pct_noise,
#   df_mc_trait0_h0_n50_Mtot10pct_noise,
#   df_mc_trait0_h0_n100_Mtot10pct_noise,
#   df_mc_trait0_h0_n300_Mtot10pct_noise,
#   df_mc_trait0_h0_n20_Mtot50pct_noise,
#   df_mc_trait0_h0_n50_Mtot50pct_noise,
#   df_mc_trait0_h0_n100_Mtot50pct_noise,
#   df_mc_trait0_h0_n300_Mtot50pct_noise
# )
#
# roc_mc_trait0_noise <- roc(response = df_mc_trait0_noise$true_label,
#                            predictor = df_mc_trait0_noise$pvalue + runif(nrow(df_mc_trait0_noise), min = 0, max = 0.001))
#
# roc_df_mc_trait0_noise <- data.frame(
#   specificity = roc_mc_trait0_noise$specificities,
#   sensitivity = roc_mc_trait0_noise$sensitivities,
#   Method = "MC trait0"
# )
#
# ################## MC trait0
#
# #### H1
#
# ## Mtot1pct_noise
# df_mc_trait1_h1_n20_Mtot1pct_noise <- data.frame(
#   pvalue = sapply(res_all_n20_Mtot1pct_k20_noise, function(res) res$MC$pvalues$pvalue[2]),
#   true_label = 1,
#   n = 20,
#   Mtot = "1pct"
# )
#
# df_mc_trait1_h1_n50_Mtot1pct_noise <- data.frame(
#   pvalue = sapply(res_all_n50_Mtot1pct_k20_noise, function(res) res$MC$pvalues$pvalue[2]),
#   true_label = 1,
#   n = 50,
#   Mtot = "1pct"
# )
#
# df_mc_trait1_h1_n100_Mtot1pct_noise <- data.frame(
#   pvalue = sapply(res_all_n100_Mtot1pct_k20_noise, function(res) res$MC$pvalues$pvalue[2]),
#   true_label = 1,
#   n = 100,
#   Mtot = "1pct"
# )
#
# df_mc_trait1_h1_n300_Mtot1pct_noise <- data.frame(
#   pvalue = sapply(res_all_n300_Mtot1pct_k20_noise, function(res) res$MC$pvalues$pvalue[2]),
#   true_label = 1,
#   n = 300,
#   Mtot = "1pct"
# )
#
# ## Mtot10pct_noise
# df_mc_trait1_h1_n20_Mtot10pct_noise <- data.frame(
#   pvalue = sapply(res_all_n20_Mtot10pct_k20_noise, function(res) res$MC$pvalues$pvalue[2]),
#   true_label = 1,
#   n = 20,
#   Mtot = "10pct"
# )
#
# df_mc_trait1_h1_n50_Mtot10pct_noise <- data.frame(
#   pvalue = sapply(res_all_n50_Mtot10pct_k20_noise, function(res) res$MC$pvalues$pvalue[2]),
#   true_label = 1,
#   n = 50,
#   Mtot = "10pct"
# )
#
# df_mc_trait1_h1_n100_Mtot10pct_noise <- data.frame(
#   pvalue = sapply(res_all_n100_Mtot10pct_k20_noise, function(res) res$MC$pvalues$pvalue[2]),
#   true_label = 1,
#   n = 100,
#   Mtot = "10pct"
# )
#
# df_mc_trait1_h1_n300_Mtot10pct_noise <- data.frame(
#   pvalue = sapply(res_all_n300_Mtot10pct_k20_noise, function(res) res$MC$pvalues$pvalue[2]),
#   true_label = 1,
#   n = 300,
#   Mtot = "10pct"
# )
#
# ## Mtot50pct_noise
# df_mc_trait1_h1_n20_Mtot50pct_noise <- data.frame(
#   pvalue = sapply(res_all_n20_Mtot50pct_k20_noise, function(res) res$MC$pvalues$pvalue[2]),
#   true_label = 1,
#   n = 20,
#   Mtot = "50pct"
# )
#
# df_mc_trait1_h1_n50_Mtot50pct_noise <- data.frame(
#   pvalue = sapply(res_all_n50_Mtot50pct_k20_noise, function(res) res$MC$pvalues$pvalue[2]),
#   true_label = 1,
#   n = 50,
#   Mtot = "50pct"
# )
#
# df_mc_trait1_h1_n100_Mtot50pct_noise <- data.frame(
#   pvalue = sapply(res_all_n100_Mtot50pct_k20_noise, function(res) res$MC$pvalues$pvalue[2]),
#   true_label = 1,
#   n = 100,
#   Mtot = "50pct"
# )
#
# df_mc_trait1_h1_n300_Mtot50pct_noise <- data.frame(
#   pvalue = sapply(res_all_n300_Mtot50pct_k20_noise, function(res) res$MC$pvalues$pvalue[2]),
#   true_label = 1,
#   n = 300,
#   Mtot = "50pct"
# )
#
# #### H0
#
# ## Mtot1pct_noise
# df_mc_trait1_h0_n20_Mtot1pct_noise <- data.frame(
#   pvalue = sapply(res_all_n20_Mtot1pct_k20_H0_noise, function(res) res$MC$pvalues$pvalue[2]),
#   true_label = 0,
#   n = 20,
#   Mtot = "1pct"
# )
#
# df_mc_trait1_h0_n50_Mtot1pct_noise <- data.frame(
#   pvalue = sapply(res_all_n50_Mtot1pct_k20_H0_noise, function(res) res$MC$pvalues$pvalue[2]),
#   true_label = 0,
#   n = 50,
#   Mtot = "1pct"
# )
#
# df_mc_trait1_h0_n100_Mtot1pct_noise <- data.frame(
#   pvalue = sapply(res_all_n100_Mtot1pct_k20_H0_noise, function(res) res$MC$pvalues$pvalue[2]),
#   true_label = 0,
#   n = 100,
#   Mtot = "1pct"
# )
#
# df_mc_trait1_h0_n300_Mtot1pct_noise <- data.frame(
#   pvalue = sapply(res_all_n300_Mtot1pct_k20_H0_noise, function(res) res$MC$pvalues$pvalue[2]),
#   true_label = 0,
#   n = 300,
#   Mtot = "1pct"
# )
#
# ## Mtot10pct_noise
# df_mc_trait1_h0_n20_Mtot10pct_noise <- data.frame(
#   pvalue = sapply(res_all_n20_Mtot10pct_k20_H0_noise, function(res) res$MC$pvalues$pvalue[2]),
#   true_label = 0,
#   n = 20,
#   Mtot = "10pct"
# )
#
# df_mc_trait1_h0_n50_Mtot10pct_noise <- data.frame(
#   pvalue = sapply(res_all_n50_Mtot10pct_k20_H0_noise, function(res) res$MC$pvalues$pvalue[2]),
#   true_label = 0,
#   n = 50,
#   Mtot = "10pct"
# )
#
# df_mc_trait1_h0_n100_Mtot10pct_noise <- data.frame(
#   pvalue = sapply(res_all_n100_Mtot10pct_k20_H0_noise, function(res) res$MC$pvalues$pvalue[2]),
#   true_label = 0,
#   n = 100,
#   Mtot = "10pct"
# )
#
# df_mc_trait1_h0_n300_Mtot10pct_noise <- data.frame(
#   pvalue = sapply(res_all_n300_Mtot10pct_k20_H0_noise, function(res) res$MC$pvalues$pvalue[2]),
#   true_label = 0,
#   n = 300,
#   Mtot = "10pct"
# )
#
# ## Mtot50pct_noise
# df_mc_trait1_h0_n20_Mtot50pct_noise <- data.frame(
#   pvalue = sapply(res_all_n20_Mtot50pct_k20_H0_noise, function(res) res$MC$pvalues$pvalue[2]),
#   true_label = 0,
#   n = 20,
#   Mtot = "50pct"
# )
#
# df_mc_trait1_h0_n50_Mtot50pct_noise <- data.frame(
#   pvalue = sapply(res_all_n50_Mtot50pct_k20_H0_noise, function(res) res$MC$pvalues$pvalue[2]),
#   true_label = 0,
#   n = 50,
#   Mtot = "50pct"
# )
#
# df_mc_trait1_h0_n100_Mtot50pct_noise <- data.frame(
#   pvalue = sapply(res_all_n100_Mtot50pct_k20_H0_noise, function(res) res$MC$pvalues$pvalue[2]),
#   true_label = 0,
#   n = 100,
#   Mtot = "50pct"
# )
#
# df_mc_trait1_h0_n300_Mtot50pct_noise <- data.frame(
#   pvalue = sapply(res_all_n300_Mtot50pct_k20_H0_noise, function(res) res$MC$pvalues$pvalue[2]),
#   true_label = 0,
#   n = 300,
#   Mtot = "50pct"
# )
#
#
# df_mc_trait1_noise <- rbind(
#   df_mc_trait1_h1_n20_Mtot1pct_noise,
#   df_mc_trait1_h1_n50_Mtot1pct_noise,
#   df_mc_trait1_h1_n100_Mtot1pct_noise,
#   df_mc_trait1_h1_n300_Mtot1pct_noise,
#   df_mc_trait1_h1_n20_Mtot10pct_noise,
#   df_mc_trait1_h1_n50_Mtot10pct_noise,
#   df_mc_trait1_h1_n100_Mtot10pct_noise,
#   df_mc_trait1_h1_n300_Mtot10pct_noise,
#   df_mc_trait1_h1_n20_Mtot50pct_noise,
#   df_mc_trait1_h1_n50_Mtot50pct_noise,
#   df_mc_trait1_h1_n100_Mtot50pct_noise,
#   df_mc_trait1_h1_n300_Mtot50pct_noise,
#   df_mc_trait1_h0_n20_Mtot1pct_noise,
#   df_mc_trait1_h0_n50_Mtot1pct_noise,
#   df_mc_trait1_h0_n100_Mtot1pct_noise,
#   df_mc_trait1_h0_n300_Mtot1pct_noise,
#   df_mc_trait1_h0_n20_Mtot10pct_noise,
#   df_mc_trait1_h0_n50_Mtot10pct_noise,
#   df_mc_trait1_h0_n100_Mtot10pct_noise,
#   df_mc_trait1_h0_n300_Mtot10pct_noise,
#   df_mc_trait1_h0_n20_Mtot50pct_noise,
#   df_mc_trait1_h0_n50_Mtot50pct_noise,
#   df_mc_trait1_h0_n100_Mtot50pct_noise,
#   df_mc_trait1_h0_n300_Mtot50pct_noise
# )
#
# roc_mc_trait1_noise <- roc(response = df_mc_trait1_noise$true_label,
#                            predictor = df_mc_trait1_noise$pvalue + runif(nrow(df_mc_trait1_noise), 0, 0.001))
#
# roc_df_mc_trait1_noise <- data.frame(
#   specificity = roc_mc_trait1_noise$specificities,
#   sensitivity = roc_mc_trait1_noise$sensitivities,
#   Method = "MC trait1"
# )
#
#
#
# ################## mutaphy
#
# #### H1
#
# ## Mtot1pct_noise
# df_mutaphy_h1_n20_Mtot1pct_noise <- data.frame(
#   pvalue = sapply(res_all_n20_Mtot1pct_k20_noise, function(res) {     res$MutaPhy$tree$min_corr_pvalue  }),
#   true_label = 1,
#   n = 20,
#   Mtot = "1pct"
# )
#
# df_mutaphy_h1_n50_Mtot1pct_noise <- data.frame(
#   pvalue = sapply(res_all_n50_Mtot1pct_k20_noise, function(res) {     res$MutaPhy$tree$min_corr_pvalue  }),
#   true_label = 1,
#   n = 50,
#   Mtot = "1pct"
# )
#
# df_mutaphy_h1_n100_Mtot1pct_noise <- data.frame(
#   pvalue = sapply(res_all_n100_Mtot1pct_k20_noise, function(res) {     res$MutaPhy$tree$min_corr_pvalue  }),
#   true_label = 1,
#   n = 100,
#   Mtot = "1pct"
# )
#
# df_mutaphy_h1_n300_Mtot1pct_noise <- data.frame(
#   pvalue = sapply(res_all_n300_Mtot1pct_k20_noise, function(res) {     res$MutaPhy$tree$min_corr_pvalue  }),
#   true_label = 1,
#   n = 300,
#   Mtot = "1pct"
# )
#
# ## Mtot10pct_noise
# df_mutaphy_h1_n20_Mtot10pct_noise <- data.frame(
#   pvalue = sapply(res_all_n20_Mtot10pct_k20_noise, function(res) {     res$MutaPhy$tree$min_corr_pvalue  }),
#   true_label = 1,
#   n = 20,
#   Mtot = "10pct"
# )
#
# df_mutaphy_h1_n50_Mtot10pct_noise <- data.frame(
#   pvalue = sapply(res_all_n50_Mtot10pct_k20_noise, function(res) {     res$MutaPhy$tree$min_corr_pvalue  }),
#   true_label = 1,
#   n = 50,
#   Mtot = "10pct"
# )
#
# df_mutaphy_h1_n100_Mtot10pct_noise <- data.frame(
#   pvalue = sapply(res_all_n100_Mtot10pct_k20_noise, function(res) {     res$MutaPhy$tree$min_corr_pvalue  }),
#   true_label = 1,
#   n = 100,
#   Mtot = "10pct"
# )
#
# df_mutaphy_h1_n300_Mtot10pct_noise <- data.frame(
#   pvalue = sapply(res_all_n300_Mtot10pct_k20_noise, function(res) {     res$MutaPhy$tree$min_corr_pvalue  }),
#   true_label = 1,
#   n = 300,
#   Mtot = "10pct"
# )
#
# ## Mtot50pct_noise
# df_mutaphy_h1_n20_Mtot50pct_noise <- data.frame(
#   pvalue = sapply(res_all_n20_Mtot50pct_k20_noise, function(res) {     res$MutaPhy$tree$min_corr_pvalue  }),
#   true_label = 1,
#   n = 20,
#   Mtot = "50pct"
# )
#
# df_mutaphy_h1_n50_Mtot50pct_noise <- data.frame(
#   pvalue = sapply(res_all_n50_Mtot50pct_k20_noise, function(res) {     res$MutaPhy$tree$min_corr_pvalue  }),
#   true_label = 1,
#   n = 50,
#   Mtot = "50pct"
# )
#
# df_mutaphy_h1_n100_Mtot50pct_noise <- data.frame(
#   pvalue = sapply(res_all_n100_Mtot50pct_k20_noise, function(res) {     res$MutaPhy$tree$min_corr_pvalue  }),
#   true_label = 1,
#   n = 100,
#   Mtot = "50pct"
# )
#
# df_mutaphy_h1_n300_Mtot50pct_noise <- data.frame(
#   pvalue = sapply(res_all_n300_Mtot50pct_k20_noise, function(res) {     res$MutaPhy$tree$min_corr_pvalue  }),
#   true_label = 1,
#   n = 300,
#   Mtot = "50pct"
# )
#
# #### H0
#
# ## Mtot1pct_noise
# df_mutaphy_h0_n20_Mtot1pct_noise <- data.frame(
#   pvalue = sapply(res_all_n20_Mtot1pct_k20_H0_noise, function(res) {     res$MutaPhy$tree$min_corr_pvalue  }),
#   true_label = 0,
#   n = 20,
#   Mtot = "1pct"
# )
#
# df_mutaphy_h0_n50_Mtot1pct_noise <- data.frame(
#   pvalue = sapply(res_all_n50_Mtot1pct_k20_H0_noise, function(res) {     res$MutaPhy$tree$min_corr_pvalue  }),
#   true_label = 0,
#   n = 50,
#   Mtot = "1pct"
# )
#
# df_mutaphy_h0_n100_Mtot1pct_noise <- data.frame(
#   pvalue = sapply(res_all_n100_Mtot1pct_k20_H0_noise, function(res) {     res$MutaPhy$tree$min_corr_pvalue  }),
#   true_label = 0,
#   n = 100,
#   Mtot = "1pct"
# )
#
# df_mutaphy_h0_n300_Mtot1pct_noise <- data.frame(
#   pvalue = sapply(res_all_n300_Mtot1pct_k20_H0_noise, function(res) {     res$MutaPhy$tree$min_corr_pvalue  }),
#   true_label = 0,
#   n = 300,
#   Mtot = "1pct"
# )
#
# ## Mtot10pct_noise
# df_mutaphy_h0_n20_Mtot10pct_noise <- data.frame(
#   pvalue = sapply(res_all_n20_Mtot10pct_k20_H0_noise, function(res) {     res$MutaPhy$tree$min_corr_pvalue  }),
#   true_label = 0,
#   n = 20,
#   Mtot = "10pct"
# )
#
# df_mutaphy_h0_n50_Mtot10pct_noise <- data.frame(
#   pvalue = sapply(res_all_n50_Mtot10pct_k20_H0_noise, function(res) {     res$MutaPhy$tree$min_corr_pvalue  }),
#   true_label = 0,
#   n = 50,
#   Mtot = "10pct"
# )
#
# df_mutaphy_h0_n100_Mtot10pct_noise <- data.frame(
#   pvalue = sapply(res_all_n100_Mtot10pct_k20_H0_noise, function(res) {     res$MutaPhy$tree$min_corr_pvalue  }),
#   true_label = 0,
#   n = 100,
#   Mtot = "10pct"
# )
#
# df_mutaphy_h0_n300_Mtot10pct_noise <- data.frame(
#   pvalue = sapply(res_all_n300_Mtot10pct_k20_H0_noise, function(res) {     res$MutaPhy$tree$min_corr_pvalue  }),
#   true_label = 0,
#   n = 300,
#   Mtot = "10pct"
# )
#
# ## Mtot50pct_noise
# df_mutaphy_h0_n20_Mtot50pct_noise <- data.frame(
#   pvalue = sapply(res_all_n20_Mtot50pct_k20_H0_noise, function(res) {     res$MutaPhy$tree$min_corr_pvalue  }),
#   true_label = 0,
#   n = 20,
#   Mtot = "50pct"
# )
#
# df_mutaphy_h0_n50_Mtot50pct_noise <- data.frame(
#   pvalue = sapply(res_all_n50_Mtot50pct_k20_H0_noise, function(res) {     res$MutaPhy$tree$min_corr_pvalue  }),
#   true_label = 0,
#   n = 50,
#   Mtot = "50pct"
# )
#
# df_mutaphy_h0_n100_Mtot50pct_noise <- data.frame(
#   pvalue = sapply(res_all_n100_Mtot50pct_k20_H0_noise, function(res) {     res$MutaPhy$tree$min_corr_pvalue  }),
#   true_label = 0,
#   n = 100,
#   Mtot = "50pct"
# )
#
# df_mutaphy_h0_n300_Mtot50pct_noise <- data.frame(
#   pvalue = sapply(res_all_n300_Mtot50pct_k20_H0_noise, function(res) {     res$MutaPhy$tree$min_corr_pvalue  }),
#   true_label = 0,
#   n = 300,
#   Mtot = "50pct"
# )
#
#
# df_mutaphy_noise <- rbind(
#   df_mutaphy_h1_n20_Mtot1pct_noise,
#   df_mutaphy_h1_n50_Mtot1pct_noise,
#   df_mutaphy_h1_n100_Mtot1pct_noise,
#   df_mutaphy_h1_n300_Mtot1pct_noise,
#   df_mutaphy_h1_n20_Mtot10pct_noise,
#   df_mutaphy_h1_n50_Mtot10pct_noise,
#   df_mutaphy_h1_n100_Mtot10pct_noise,
#   df_mutaphy_h1_n300_Mtot10pct_noise,
#   df_mutaphy_h1_n20_Mtot50pct_noise,
#   df_mutaphy_h1_n50_Mtot50pct_noise,
#   df_mutaphy_h1_n100_Mtot50pct_noise,
#   df_mutaphy_h1_n300_Mtot50pct_noise,
#   df_mutaphy_h0_n20_Mtot1pct_noise,
#   df_mutaphy_h0_n50_Mtot1pct_noise,
#   df_mutaphy_h0_n100_Mtot1pct_noise,
#   df_mutaphy_h0_n300_Mtot1pct_noise,
#   df_mutaphy_h0_n20_Mtot10pct_noise,
#   df_mutaphy_h0_n50_Mtot10pct_noise,
#   df_mutaphy_h0_n100_Mtot10pct_noise,
#   df_mutaphy_h0_n300_Mtot10pct_noise,
#   df_mutaphy_h0_n20_Mtot50pct_noise,
#   df_mutaphy_h0_n50_Mtot50pct_noise,
#   df_mutaphy_h0_n100_Mtot50pct_noise,
#   df_mutaphy_h0_n300_Mtot50pct_noise
# )
#
# roc_mutaphy_noise <- roc(response = df_mutaphy_noise$true_label,
#                          predictor = df_mutaphy_noise$pvalue )
#
# roc_df_mutaphy_noise <- data.frame(
#   specificity = roc_mutaphy_noise$specificities,
#   sensitivity = roc_mutaphy_noise$sensitivities,
#   Method = "MutaPhy"
# )
#
#
#
# ####
#
# #### H1
#
# ## Mtot1pct_noise
# df_crp_tree_h1_n20_Mtot1pct_noise <- data.frame(
#   pvalue = sapply(res_all_n20_Mtot1pct_k20_noise, function(res) {     tryCatch({       res[["CRPTree"]][["pval_tree"]]     }, error = function(e) NA)   }),
#   true_label = 1,
#   n = 20,
#   Mtot = "1pct"
# )
#
# df_crp_tree_h1_n50_Mtot1pct_noise <- data.frame(
#   pvalue = sapply(res_all_n50_Mtot1pct_k20_noise, function(res) {     tryCatch({       res[["CRPTree"]][["pval_tree"]]     }, error = function(e) NA)   }),
#   true_label = 1,
#   n = 50,
#   Mtot = "1pct"
# )
#
# df_crp_tree_h1_n100_Mtot1pct_noise <- data.frame(
#   pvalue = sapply(res_all_n100_Mtot1pct_k20_noise, function(res) {     tryCatch({       res[["CRPTree"]][["pval_tree"]]     }, error = function(e) NA)   }),
#   true_label = 1,
#   n = 100,
#   Mtot = "1pct"
# )
#
# df_crp_tree_h1_n300_Mtot1pct_noise <- data.frame(
#   pvalue = sapply(res_all_n300_Mtot1pct_k20_noise, function(res) {     tryCatch({       res[["CRPTree"]][["pval_tree"]]     }, error = function(e) NA)   }),
#   true_label = 1,
#   n = 300,
#   Mtot = "1pct"
# )
#
# ## Mtot10pct_noise
# df_crp_tree_h1_n20_Mtot10pct_noise <- data.frame(
#   pvalue = sapply(res_all_n20_Mtot10pct_k20_noise, function(res) {     tryCatch({       res[["CRPTree"]][["pval_tree"]]     }, error = function(e) NA)   }),
#   true_label = 1,
#   n = 20,
#   Mtot = "10pct"
# )
#
# df_crp_tree_h1_n50_Mtot10pct_noise <- data.frame(
#   pvalue = sapply(res_all_n50_Mtot10pct_k20_noise, function(res) {     tryCatch({       res[["CRPTree"]][["pval_tree"]]     }, error = function(e) NA)   }),
#   true_label = 1,
#   n = 50,
#   Mtot = "10pct"
# )
#
# df_crp_tree_h1_n100_Mtot10pct_noise <- data.frame(
#   pvalue = sapply(res_all_n100_Mtot10pct_k20_noise, function(res) {     tryCatch({       res[["CRPTree"]][["pval_tree"]]     }, error = function(e) NA)   }),
#   true_label = 1,
#   n = 100,
#   Mtot = "10pct"
# )
#
# df_crp_tree_h1_n300_Mtot10pct_noise <- data.frame(
#   pvalue = sapply(res_all_n300_Mtot10pct_k20_noise, function(res) {     tryCatch({       res[["CRPTree"]][["pval_tree"]]     }, error = function(e) NA)   }),
#   true_label = 1,
#   n = 300,
#   Mtot = "10pct"
# )
#
# ## Mtot50pct_noise
# df_crp_tree_h1_n20_Mtot50pct_noise <- data.frame(
#   pvalue = sapply(res_all_n20_Mtot50pct_k20_noise, function(res) {     tryCatch({       res[["CRPTree"]][["pval_tree"]]     }, error = function(e) NA)   }),
#   true_label = 1,
#   n = 20,
#   Mtot = "50pct"
# )
#
# df_crp_tree_h1_n50_Mtot50pct_noise <- data.frame(
#   pvalue = sapply(res_all_n50_Mtot50pct_k20_noise, function(res) {     tryCatch({       res[["CRPTree"]][["pval_tree"]]     }, error = function(e) NA)   }),
#   true_label = 1,
#   n = 50,
#   Mtot = "50pct"
# )
#
# df_crp_tree_h1_n100_Mtot50pct_noise <- data.frame(
#   pvalue = sapply(res_all_n100_Mtot50pct_k20_noise, function(res) {     tryCatch({       res[["CRPTree"]][["pval_tree"]]     }, error = function(e) NA)   }),
#   true_label = 1,
#   n = 100,
#   Mtot = "50pct"
# )
#
# df_crp_tree_h1_n300_Mtot50pct_noise <- data.frame(
#   pvalue = sapply(res_all_n300_Mtot50pct_k20_noise, function(res) {     tryCatch({       res[["CRPTree"]][["pval_tree"]]     }, error = function(e) NA)   }),
#   true_label = 1,
#   n = 300,
#   Mtot = "50pct"
# )
#
# #### H0
#
# ## Mtot1pct_noise
# df_crp_tree_h0_n20_Mtot1pct_noise <- data.frame(
#   pvalue = sapply(res_all_n20_Mtot1pct_k20_H0_noise, function(res) {     tryCatch({       res[["CRPTree"]][["pval_tree"]]     }, error = function(e) NA)   }),
#   true_label = 0,
#   n = 20,
#   Mtot = "1pct"
# )
#
# df_crp_tree_h0_n50_Mtot1pct_noise <- data.frame(
#   pvalue = sapply(res_all_n50_Mtot1pct_k20_H0_noise, function(res) {     tryCatch({       res[["CRPTree"]][["pval_tree"]]     }, error = function(e) NA)   }),
#   true_label = 0,
#   n = 50,
#   Mtot = "1pct"
# )
#
# df_crp_tree_h0_n100_Mtot1pct_noise <- data.frame(
#   pvalue = sapply(res_all_n100_Mtot1pct_k20_H0_noise, function(res) {     tryCatch({       res[["CRPTree"]][["pval_tree"]]     }, error = function(e) NA)   }),
#   true_label = 0,
#   n = 100,
#   Mtot = "1pct"
# )
#
# df_crp_tree_h0_n300_Mtot1pct_noise <- data.frame(
#   pvalue = sapply(res_all_n300_Mtot1pct_k20_H0_noise, function(res) {     tryCatch({       res[["CRPTree"]][["pval_tree"]]     }, error = function(e) NA)   }),
#   true_label = 0,
#   n = 300,
#   Mtot = "1pct"
# )
#
# ## Mtot10pct_noise
# df_crp_tree_h0_n20_Mtot10pct_noise <- data.frame(
#   pvalue = sapply(res_all_n20_Mtot10pct_k20_H0_noise, function(res) {     tryCatch({       res[["CRPTree"]][["pval_tree"]]     }, error = function(e) NA)   }),
#   true_label = 0,
#   n = 20,
#   Mtot = "10pct"
# )
#
# df_crp_tree_h0_n50_Mtot10pct_noise <- data.frame(
#   pvalue = sapply(res_all_n50_Mtot10pct_k20_H0_noise, function(res) {     tryCatch({       res[["CRPTree"]][["pval_tree"]]     }, error = function(e) NA)   }),
#   true_label = 0,
#   n = 50,
#   Mtot = "10pct"
# )
#
# df_crp_tree_h0_n100_Mtot10pct_noise <- data.frame(
#   pvalue = sapply(res_all_n100_Mtot10pct_k20_H0_noise, function(res) {     tryCatch({       res[["CRPTree"]][["pval_tree"]]     }, error = function(e) NA)   }),
#   true_label = 0,
#   n = 100,
#   Mtot = "10pct"
# )
#
# df_crp_tree_h0_n300_Mtot10pct_noise <- data.frame(
#   pvalue = sapply(res_all_n300_Mtot10pct_k20_H0_noise, function(res) {     tryCatch({       res[["CRPTree"]][["pval_tree"]]     }, error = function(e) NA)   }),
#   true_label = 0,
#   n = 300,
#   Mtot = "10pct"
# )
#
# ## Mtot50pct_noise
# df_crp_tree_h0_n20_Mtot50pct_noise <- data.frame(
#   pvalue = sapply(res_all_n20_Mtot50pct_k20_H0_noise, function(res) {     tryCatch({       res[["CRPTree"]][["pval_tree"]]     }, error = function(e) NA)   }),
#   true_label = 0,
#   n = 20,
#   Mtot = "50pct"
# )
#
# df_crp_tree_h0_n50_Mtot50pct_noise <- data.frame(
#   pvalue = sapply(res_all_n50_Mtot50pct_k20_H0_noise, function(res) {     tryCatch({       res[["CRPTree"]][["pval_tree"]]     }, error = function(e) NA)   }),
#   true_label = 0,
#   n = 50,
#   Mtot = "50pct"
# )
#
# df_crp_tree_h0_n100_Mtot50pct_noise <- data.frame(
#   pvalue = sapply(res_all_n100_Mtot50pct_k20_H0_noise, function(res) {     tryCatch({       res[["CRPTree"]][["pval_tree"]]     }, error = function(e) NA)   }),
#   true_label = 0,
#   n = 100,
#   Mtot = "50pct"
# )
#
# df_crp_tree_h0_n300_Mtot50pct_noise <- data.frame(
#   pvalue = sapply(res_all_n300_Mtot50pct_k20_H0_noise, function(res) {     tryCatch({       res[["CRPTree"]][["pval_tree"]]     }, error = function(e) NA)   }),
#   true_label = 0,
#   n = 300,
#   Mtot = "50pct"
# )
#
#
# df_crp_tree_noise <- rbind(
#   df_crp_tree_h1_n20_Mtot1pct_noise,
#   df_crp_tree_h1_n50_Mtot1pct_noise,
#   df_crp_tree_h1_n100_Mtot1pct_noise,
#   df_crp_tree_h1_n300_Mtot1pct_noise,
#   df_crp_tree_h1_n20_Mtot10pct_noise,
#   df_crp_tree_h1_n50_Mtot10pct_noise,
#   df_crp_tree_h1_n100_Mtot10pct_noise,
#   df_crp_tree_h1_n300_Mtot10pct_noise,
#   df_crp_tree_h1_n20_Mtot50pct_noise,
#   df_crp_tree_h1_n50_Mtot50pct_noise,
#   df_crp_tree_h1_n100_Mtot50pct_noise,
#   df_crp_tree_h1_n300_Mtot50pct_noise,
#   df_crp_tree_h0_n20_Mtot1pct_noise,
#   df_crp_tree_h0_n50_Mtot1pct_noise,
#   df_crp_tree_h0_n100_Mtot1pct_noise,
#   df_crp_tree_h0_n300_Mtot1pct_noise,
#   df_crp_tree_h0_n20_Mtot10pct_noise,
#   df_crp_tree_h0_n50_Mtot10pct_noise,
#   df_crp_tree_h0_n100_Mtot10pct_noise,
#   df_crp_tree_h0_n300_Mtot10pct_noise,
#   df_crp_tree_h0_n20_Mtot50pct_noise,
#   df_crp_tree_h0_n50_Mtot50pct_noise,
#   df_crp_tree_h0_n100_Mtot50pct_noise,
#   df_crp_tree_h0_n300_Mtot50pct_noise
# )
#
# roc_crp_tree_noise <- roc(response = df_crp_tree_noise$true_label,
#                           predictor = df_crp_tree_noise$pvalue)
#
# roc_df_crp_tree_noise <- data.frame(
#   specificity = roc_crp_tree_noise$specificities,
#   sensitivity = roc_crp_tree_noise$sensitivities,
#   Method = "CRP-Tree"
# )
#
#
# auc_ai_noise <- round(auc(roc_ai_noise),3)
# auc_mutaphy_noise <- round(auc(roc_mutaphy_noise),3)
# auc_ps_noise <- round(auc(roc_ps_noise),3)
# auc_mc_trait0_noise <- round(auc(roc_mc_trait0_noise),3)
# auc_mc_trait1_noise <- round(auc(roc_mc_trait1_noise),3)
# auc_crp_tree_noise <- round(auc(roc_crp_tree_noise),3)
#
#
# roc_list_noise <- list(
#   "AI" = roc_ai_noise,
#   "PS" = roc_ps_noise,
#   "MC trait0" = roc_mc_trait0_noise,
#   "MC trait1" = roc_mc_trait1_noise,
#   "CRP-Tree" = roc_crp_tree_noise,
#   "MutaPhy" = roc_mutaphy_noise
# )
#
# colors <- c("#009E60",
#             "#5E3C99",
#             "#ADD8E6",
#             "#6495ED",
#             "#FFAC1C",
#             "#C70039"
# )

# roc_plot_noise <- ggroc(roc_list_noise, size = 1.2) +
#   scale_color_manual(
#     name = "Method",
#     values = colors,
#     labels = c(
#       paste0("AI (AUC = ", auc_ai_noise, ")"),
#       paste0("PS (AUC = ", auc_ps_noise, ")"),
#       paste0("MC trait0 (AUC = ", auc_mc_trait0_noise, ")"),
#       paste0("MC trait1 (AUC = ", auc_mc_trait1_noise, ")"),
#       paste0("CRP-Tree (AUC = ", auc_crp_tree_noise, ")"),
#       paste0("MutaPhy (AUC = ", auc_mutaphy_noise, ")")
#     )
#   ) +
#   labs(
#     title = "ROC curves (noise)",
#     x = "Specificity",
#     y = "Sensitivity"
#   ) +
#   theme_minimal(base_size = 14) +
#   theme(
#     plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
#     axis.title.x = element_text(size = 13, face = "bold"),
#     axis.title.y = element_text(size = 13, face = "bold"),
#     axis.text = element_text(size = 11),
#     legend.position = c(0.65, 0.25),
#     legend.title = element_text(size = 11),
#     legend.text = element_text(size = 10),
#     panel.grid.major = element_line(color = "grey90"),
#     panel.grid.minor = element_blank()
#   ) +
#   geom_abline(slope = 1, intercept = 1, linetype = "dashed", color = "grey40") +
#   coord_equal()

print(roc_plot_noise)

roc_plot + roc_plot_noise + plot_layout(widths = c(1, 1))
#
#
# ######### H0H1, n, noise, methods -> mtot
#
# df_ai_no_noise$Method <- "AI"
# df_ps_no_noise$Method <- "PS"
# df_mc_trait0_no_noise$Method <- "MC trait0"
# df_mc_trait1_no_noise$Method <- "MC trait1"
# df_crp_tree_no_noise$Method <- "CRP-Tree"
# df_mutaphy_no_noise$Method <- "MutaPhy"
#
# df_ai_noise$Method <- "AI"
# df_ps_noise$Method <- "PS"
# df_mc_trait0_noise$Method <- "MC trait0"
# df_mc_trait1_noise$Method <- "MC trait1"
# df_crp_tree_noise$Method <- "CRP-Tree"
# df_mutaphy_noise$Method <- "MutaPhy"
#
# df_ai_no_noise$Noise <- "no noise"
# df_ps_no_noise$Noise <- "no noise"
# df_mc_trait0_no_noise$Noise <- "no noise"
# df_mc_trait1_no_noise$Noise <- "no noise"
# df_crp_tree_no_noise$Noise <- "no noise"
# df_mutaphy_no_noise$Noise <- "no noise"
#
# df_ai_noise$Noise <- "noise"
# df_ps_noise$Noise <- "noise"
# df_mc_trait0_noise$Noise <- "noise"
# df_mc_trait1_noise$Noise <- "noise"
# df_crp_tree_noise$Noise <- "noise"
# df_mutaphy_noise$Noise <- "noise"
#
# roc_crp_tree_noise <- roc(response = df_crp_tree_noise$true_label,
#                                            predictor = df_crp_tree_noise$pvalue)
#
#
# df_all <- rbind(
#   df_ai_no_noise,
#   df_ps_no_noise,
#   df_mc_trait0_no_noise,
#   df_mc_trait1_no_noise,
#   df_crp_tree_no_noise,
#   df_mutaphy_no_noise,
#   df_ai_noise,
#   df_ps_noise,
#   df_mc_trait0_noise,
#   df_mc_trait1_noise,
#   df_crp_tree_noise,
#   df_mutaphy_noise
# )
#
# auc_df <- df_all %>%
#   group_by(Method, Noise, Mtot, n) %>%
#   filter(length(unique(true_label)) == 2) %>%
#   summarise(
#     AUC = as.numeric(auc(response = true_label, predictor = pvalue)),
#     .groups = "drop"
#   )
#
#
# # line_types <- c("no noise" = "solid", "two-sided" = "dashed")
#
# colors <- c("#009E60",
#             "#5E3C99",
#             "#ADD8E6",
#             "#6495ED",
#             "#FFAC1C",
#             "#C70039"
# )


#
# named_colors <- c(
#   "AI" = "#5E3C99",
#   "PS" = "#009E60",
#   "MC trait0" = "#ADD8E6",
#   "MC trait1" = "#6495ED",
#   "MutaPhy" = "#C70039",
#   "CRP-Tree" = "#FFAC1C"
# )
#
# line_types <- c("no noise" = "solid", "noise" = "solid")

# for (m in unique(auc_df$Mtot)) {
#   p <- auc_df %>%
#     filter(Mtot == m) %>%
#     ggplot(aes(x = n, y = AUC, color = Method, linetype = Noise)) +
#     geom_line(size = 1) +
#     geom_point(size = 2) +
#     scale_color_manual(values = named_colors) +
#     scale_linetype_manual(values = line_types) +
#     labs(
#       title = paste("AUC depending to noise, n, method for", m),
#       x = "Sample size (n)",
#       y = "AUC"
#     ) +
#     ylim(0, 1) +
#     theme_minimal(base_size = 14) +
#     theme(legend.position = "bottom")
#
#   print(p)
# }


# for (m in unique(auc_df$Mtot)) {
#   p <- auc_df %>%
#     filter(Mtot == m) %>%
#     ggplot(aes(x = n, y = AUC, color = Method, linetype = Noise)) +
#     geom_line(size = 1) +
#     geom_point(size = 2) +
#     scale_linetype_manual(values = line_types) +
#     labs(
#       title = paste("AUC depending to noise, n, method for", m),
#       x = "Sample size (n)",
#       y = "AUC"
#     ) +
#     ylim(0, 1) +
#     theme_minimal(base_size = 14) +
#     theme(legend.position = "bottom")
#
#   print(p)
# }
#
# for (m in unique(auc_df$Mtot)) {
#   p <- auc_df %>%
#     filter(Mtot == m) %>%
#     ggplot(aes(x = n, y = AUC, color = Method)) +
#     geom_line(aes(linetype = Noise), size = 1) +
#     geom_point(size = 2) +
#     scale_color_manual(values = named_colors) +
#     scale_linetype_manual(values = c("solid", "solid")) +
#     labs(
#       title = paste("AUC by noise, n, method –", m),
#       x = "Sample size (n)",
#       y = "AUC"
#     ) +
#     ylim(0, 1) +
#     facet_wrap(~Noise) +
#     theme_minimal(base_size = 14) +
#     theme(legend.position = "bottom")
#
#   print(p)
# }
#
# auc_df$Mtot <- factor(auc_df$Mtot, levels = c("1pct", "10pct", "50pct"))
#
#
# auc_df <- auc_df %>%
#   mutate(Mtot = factor(Mtot, levels = c("1pct", "10pct", "50pct"))) %>%
#   mutate(Mtot_label = factor(Mtot,
#                              levels = c("1pct", "10pct", "50pct"),
#                              labels = c("1%", "10%", "50%")))
#
# p <- auc_df %>%
#   ggplot(aes(x = n, y = AUC, color = Method, group = Method)) +
#
#   geom_line(data = subset(auc_df, Method != "MutaPhy"),
#             aes(linetype = Noise), size = 1) +
#   geom_point(data = subset(auc_df, Method != "MutaPhy"),
#              size = 2) +
#
#   geom_line(data = subset(auc_df, Method == "MutaPhy"),
#             aes(linetype = Noise), size = 1.5) +
#   geom_point(data = subset(auc_df, Method == "MutaPhy"),
#              size = 3) +
#   scale_linetype_manual(values = c("solid", "solid")) +
#   scale_color_manual(values = named_colors) +
#   labs(
#     x = "Sample size (n)",
#     y = "AUC"
#   ) +
#   ylim(0, 1) +
#   facet_grid(Noise ~ Mtot_label) + # Mtot_label ~ Noise
#   theme_minimal(base_size = 14) +
#   theme(
#     legend.position = "bottom",
#     panel.border = element_rect(colour = "black", fill = NA)
#   )

print(p)


#### Subtree scale

get_internal_descendants <- function(tree, node) {
  descendants <- phangorn::Descendants(tree, node, "all")
  internal_descendants <- descendants[descendants > Ntip(tree)]
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

  all_data <- do.call(rbind, lapply(seq_along(list_tree), function(i) {
    tree <- list_tree[[i]]
    true_positives <- simu_subtrees[[i]]
    df_pvals <- tree_outputs[[i]][["mutaphy_h1"]][["subtrees"]][["permutation"]]

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

  all_data <- do.call(rbind, lapply(seq_along(list_tree), function(i) {
    tree <- list_tree[[i]]
    true_positives <- numeric(0)
    df_pvals <- tree_outputs[[i]][["mutaphy_h0"]][["subtrees"]][["permutation"]]

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

  all_data <- do.call(rbind, lapply(seq_along(list_tree), function(i) {
    tree <- list_tree[[i]]
    df_pvals <- tree_outputs[[i]][["mutaphy_h0"]][["subtrees"]][["permutation"]]
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
