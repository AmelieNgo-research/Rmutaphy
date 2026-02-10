#' Generate null distribution for Parsimony Score (PS) by permutation
#'
#' Computes a null distribution of the Parsimony Score (PS) by permuting the
#' trait labels on the tips \code{n_simu} times and recomputing PS each time.
#'
#' @param tree A phylogenetic tree of class \code{"phylo"} (typically rooted and binary).
#' @param trait A vector of tip states (length = \code{ape::Ntip(tree)}), ordered as \code{tree$tip.label}.
#' @param n_simu Number of permutations used to build the null distribution.
#'
#' @return A list with one element:
#' \describe{
#'   \item{ps_scores}{Numeric vector of length \code{n_simu} containing permuted PS values.}
#' }
#'
#' @details The permutation test breaks any phylogenetic association by randomly
#' shuffling trait labels across tips. See \href{https://academic.oup.com/sysbio/article-abstract/20/4/406/1673276}{Fitch 1971}.
#'
#' @seealso \code{\link{calculate_PS}}, \code{\link{ps_test}}
#' @references
#' Fitch, W. M. (1971). Toward defining the course of evolution: minimum change for a specific tree topology.
#' @export
random_PS_scores <- function(tree, trait, n_simu) {
  ps_scores <- numeric(n_simu)
  for (i in 1:n_simu) {
    permuted_trait <- sample(trait)
    ps_scores[i] <- calculate_PS(tree, permuted_trait)
  }
  list(ps_scores = ps_scores)
}

#' Compute permutation p-value for Parsimony Score (PS)
#'
#' Computes a one-sided permutation p-value for PS as:
#' \deqn{p = (1 + \sum I(PS_{null} \le PS_{obs})) / (1 + n_{simu})}
#' where smaller PS indicates stronger clustering (more phylogenetic signal).
#'
#' @param ps_obs Observed PS statistic (numeric scalar).
#' @param res Output of \code{\link{random_PS_scores}} (must contain \code{ps_scores}).
#' @param n_simu Number of permutations used (must match \code{length(res$ps_scores)}).
#'
#' @return A one-row \code{data.frame} with columns:
#' \describe{
#'   \item{statistic}{Observed PS value.}
#'   \item{pvalue}{Permutation p-value.}
#' }
#'
#' @details See \href{https://academic.oup.com/sysbio/article-abstract/20/4/406/1673276}{Fitch 1971}.
#' @references
#' Fitch, W. M. (1971). Toward defining the course of evolution: minimum change for a specific tree topology.
#' @export
calculate_pvalue_for_PS <- function(ps_obs, res, n_simu) {
  ps_null <- res$ps_scores
  pvalue <- (1 + sum(ps_null <= ps_obs)) / (1 + n_simu)
  data.frame(statistic = ps_obs, pvalue = pvalue)
}

#' Permutation test for Parsimony Score (PS)
#'
#' Computes the observed Parsimony Score (PS) and assesses significance via
#' a permutation test.
#'
#' @param tree A phylogenetic tree of class \code{"phylo"}.
#' @param trait A vector of tip states (length = \code{ape::Ntip(tree)}), ordered as \code{tree$tip.label}.
#' @param n_simu Number of permutations (default: 1000).
#'
#' @return A list with:
#' \describe{
#'   \item{ps_obs}{Observed PS value.}
#'   \item{pvalue}{Permutation p-value (numeric scalar).}
#'   \item{null_distribution}{Numeric vector of permuted PS values.}
#' }
#'
#' @details PS is computed using a Fitch parsimony-style recursion on a binary tree.
#' Lower PS indicates fewer changes and thus stronger clustering of identical states. See \href{https://academic.oup.com/sysbio/article-abstract/20/4/406/1673276}{Fitch 1971}.
#'
#' @seealso \code{\link{calculate_PS}}
#'
#' @examples
#' \dontrun{
#' set.seed(1)
#' tree <- ape::rcoal(30)
#' trait <- sample(c(rep("severe", 15), rep("non severe", 15)))
#'
#' res_ps <- ps_test(tree = tree, trait = trait, n_simu = 100)
#' res_ps$pvalue
#'}
#'
#' @references
#' Fitch, W. M. (1971). Toward defining the course of evolution: minimum change for a specific tree topology.
#' @export
ps_test <- function(tree, trait, n_simu = 1000) {
  cat("Parsimony Score (PS)...\n")
  ps_obs <- calculate_PS(tree, trait)
  res <- random_PS_scores(tree, trait, n_simu)
  pval <- calculate_pvalue_for_PS(ps_obs, res, n_simu)
  return(list(ps_obs = ps_obs, pvalue = pval$pvalue, null_distribution = res$ps_scores))
}

#' Compute Parsimony Score (PS) for a binary trait on a tree
#'
#' Computes the Parsimony Score (PS) using a Fitch parsimony procedure for a
#' (typically) binary tree: for each internal node, sets are intersected or unioned;
#' each union corresponds to one inferred change.
#'
#' @param tree A phylogenetic tree of class \code{"phylo"} (assumed bifurcating).
#' @param trait A vector of tip states (length = \code{ape::Ntip(tree)}), ordered as \code{tree$tip.label}.
#'
#' @return Numeric scalar, the PS value.
#'
#' @details This implementation assumes each internal node has exactly two children.
#' If your tree is not strictly bifurcating, results may be incorrect. See \href{https://academic.oup.com/sysbio/article-abstract/20/4/406/1673276}{Fitch 1971}.
#'
#' @references
#' Fitch, W. M. (1971). Toward defining the course of evolution: minimum change for a specific tree topology.
#'
#' @importFrom ape Ntip
#' @export
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

#' Generate null distribution for Association Index (AI) by permutation
#'
#' Computes a null distribution of the Association Index (AI) by permuting
#' the trait labels on the tips \code{n_simu} times and recomputing AI each time.
#'
#' @param tree A phylogenetic tree of class \code{"phylo"}.
#' @param trait A vector of tip states (length = \code{ape::Ntip(tree)}), ordered as \code{tree$tip.label}.
#' @param n_simu Number of permutations (default: 1000).
#'
#' @return A list with:
#' \describe{
#'   \item{ai_scores}{Numeric vector of length \code{n_simu} containing permuted AI values.}
#' }
#' @details See \href{https://doi.org/10.1128/JVI.75.23.11686-11699.2001}{Wang and al, 2001}.
#'
#' @references
#' Wang and al (2001). Identification of Shared Populations of Human Immunodeficiency Virus Type 1 Infecting Microglia and Tissue Macrophages outside the Central Nervous System.
#'
#' @seealso \code{\link{calculate_AI}}, \code{\link{ai_test}}
#' @export
random_AI_scores <- function(tree, trait, n_simu = 1000) {
  ai_scores <- numeric(n_simu)
  for (i in 1:n_simu) {
    permuted_trait <- sample(trait)
    ai_scores[i] <- calculate_AI(tree, permuted_trait)
  }
  list(ai_scores = ai_scores)
}

#' Compute permutation p-value for Association Index (AI)
#'
#' Computes a one-sided permutation p-value for AI as:
#' \deqn{p = (1 + \sum I(AI_{null} \le AI_{obs})) / (1 + n_{simu})}
#' where smaller AI indicates stronger clustering.
#'
#' @param ai_obs Observed AI statistic (numeric scalar).
#' @param res Output of \code{\link{random_AI_scores}} (must contain \code{ai_scores}).
#' @param n_simu Number of permutations used (must match \code{length(res$ai_scores)}).
#'
#' @return A one-row \code{data.frame} with columns \code{statistic} and \code{pvalue}.
#' @details See \href{https://doi.org/10.1128/JVI.75.23.11686-11699.2001}{Wang and al, 2001}.
#'
#' @references
#' Wang and al (2001). Identification of Shared Populations of Human Immunodeficiency Virus Type 1 Infecting Microglia and Tissue Macrophages outside the Central Nervous System.
#'
#' @export
calculate_pvalue_for_AI <- function(ai_obs, res, n_simu) {
  ai_null <- res$ai_scores
  pvalue <- (1 + sum(ai_null <= ai_obs)) / (1 + n_simu)
  data.frame(statistic = ai_obs, pvalue = pvalue)
}

#' Permutation test for Association Index (AI)
#'
#' Computes the observed Association Index (AI) and assesses significance using
#' a permutation test.
#'
#' @param tree A phylogenetic tree of class \code{"phylo"}.
#' @param trait A vector of tip states (length = \code{ape::Ntip(tree)}), ordered as \code{tree$tip.label}.
#' @param n_simu Number of permutations (default: 1000).
#'
#' @return A list with:
#' \describe{
#'   \item{ai_obs}{Observed AI value.}
#'   \item{pvalue}{Permutation p-value (numeric scalar).}
#'   \item{null_distribution}{Numeric vector of permuted AI values.}
#' }
#'
#' @details AI is computed by summing node-wise contributions that depend on
#' the majority state frequency among descendants and the number of descendant tips.
#' Lower AI indicates stronger clustering. See \href{https://doi.org/10.1128/JVI.75.23.11686-11699.2001}{Wang and al, 2001}.
#'
#' @examples
#' \dontrun{
#' set.seed(1)
#' tree <- ape::rcoal(30)
#' trait <- sample(c(rep("severe", 15), rep("non severe", 15)))
#' # trait must be ordered as tree$tip.label:
#' # here it's already length Ntip(tree) and will be interpreted in that order.
#'
#' res_ai <- ai_test(tree = tree, trait = trait, n_simu = 100)
#' res_ai$pvalue
#'}
#'
#' @references
#' Wang and al (2001). Identification of Shared Populations of Human Immunodeficiency Virus Type 1 Infecting Microglia and Tissue Macrophages outside the Central Nervous System.
#'
#' @note In \code{phylo.stats()}, \code{ai_test()} is called without passing \code{n_simu}
#' (so it uses its default 1000), even if \code{n_simu} is set in \code{phylo.stats()}.
#'
#' @seealso \code{\link{calculate_AI}}
#' @export
ai_test <- function(tree, trait, n_simu = 1000) {
  cat("Association Index (AI)...\n")
  ai_obs <- calculate_AI(tree, trait)
  res <- random_AI_scores(tree, trait, n_simu)
  pval <- calculate_pvalue_for_AI(ai_obs, res, n_simu)
  return(list(ai_obs = ai_obs, pvalue = pval$pvalue, null_distribution = res$ai_scores))
}

#' Compute Association Index (AI) on a tree
#'
#' Computes an Association Index (AI) by iterating over internal nodes and
#' summarizing trait clustering among descendant tips.
#'
#' @param tree A phylogenetic tree of class \code{"phylo"}.
#' @param trait A vector of tip states (length = \code{ape::Ntip(tree)}), ordered as \code{tree$tip.label}.
#'
#' @return Numeric scalar, the AI value.
#'
#' @details For each internal node, the function retrieves descendant tips
#' using \code{\link{get_tips_descendants}} and adds a node-wise contribution
#' based on the maximum trait frequency among descendants and the descendant clade size.
#' See \href{https://doi.org/10.1128/JVI.75.23.11686-11699.2001}{Wang and al, 2001}.
#'
#' @references
#' Wang and al (2001). Identification of Shared Populations of Human Immunodeficiency Virus Type 1 Infecting Microglia and Tissue Macrophages outside the Central Nervous System.
#'
#' @seealso \code{\link{get_tips_descendants}}, \code{\link{ai_test}}
#' @importFrom ape Ntip
#' @export
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


#' Generate null distributions for Monophyletic Clade size (MC) by permutation
#'
#' Computes null distributions for the Monophyletic Clade (MC) statistic for two
#' states (\code{trait0} and \code{trait1}) by permuting tip labels and
#' recomputing MC.
#'
#' @param tree A phylogenetic tree of class \code{"phylo"}.
#' @param trait A vector of tip states (length = \code{ape::Ntip(tree)}), ordered as \code{tree$tip.label}.
#' @param trait0 Character scalar: label for state "0" (or the reference state).
#' @param trait1 Character scalar: label for state "1" (or the interest state).
#' @param n_simu Number of permutations (default: 1000).
#'
#' @return A list with:
#' \describe{
#'   \item{mc_trait0}{Numeric vector of length \code{n_simu}: permuted MC for \code{trait0}.}
#'   \item{mc_trait1}{Numeric vector of length \code{n_simu}: permuted MC for \code{trait1}.}
#' }
#'
#' @details See \href{https://pubmed.ncbi.nlm.nih.gov/16103186/}{Salemi and al, 2005}
#'
#' @references
#' Salemi and al (2005). Phylodynamic analysis of human immunodeficiency virus type 1 in distinct brain compartments provides a model for the neuropathogenesis of AIDS.
#'
#' @seealso \code{\link{calculate_MC}}, \code{\link{mc_test}}
#' @export
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


#' Compute permutation p-values for Monophyletic Clade size (MC)
#'
#' Computes one-sided permutation p-values for MC (larger MC indicates stronger
#' clustering), separately for \code{trait0} and \code{trait1}:
#' \deqn{p = (1 + \sum I(MC_{null} \ge MC_{obs})) / (1 + n_{simu})}.
#'
#' @param mc_obs Named numeric vector of length 2 with observed MC values for \code{trait1} and \code{trait0}.
#' @param res Output of \code{\link{random_MC_scores}}.
#' @param n_simu Number of permutations used.
#'
#' @return A \code{data.frame} with columns \code{trait}, \code{MC_obs}, \code{pvalue}.
#' @details See \href{https://pubmed.ncbi.nlm.nih.gov/16103186/}{Salemi and al, 2005}
#'
#' @references
#' Salemi and al (2005). Phylodynamic analysis of human immunodeficiency virus type 1 in distinct brain compartments provides a model for the neuropathogenesis of AIDS.
#'
#' @export
calculate_pvalue_for_MC <- function(mc_obs, res, n_simu) {
  pval0 <- (1 + sum(res$mc_trait0 >= mc_obs[1])) / (1 + n_simu)
  pval1 <- (1 + sum(res$mc_trait1 >= mc_obs[2])) / (1 + n_simu)
  data.frame(
    trait = c(names(mc_obs)[1], names(mc_obs)[2]),
    MC_obs = c(mc_obs[1], mc_obs[2]),
    pvalue = c(pval0, pval1)
  )
}

#' Permutation test for Monophyletic Clade size (MC)
#'
#' Computes the observed MC statistic for two interest states and assesses
#' significance via permutation tests.
#'
#' @param tree A phylogenetic tree of class \code{"phylo"}.
#' @param trait A vector of tip states (length = \code{ape::Ntip(tree)}), ordered as \code{tree$tip.label}.
#' @param trait0 Character scalar: label for state "0".
#' @param trait1 Character scalar: label for state "1".
#' @param n_simu Number of permutations (default: 1000).
#'
#' @return A list with:
#' \describe{
#'   \item{mc_obs}{Named numeric vector: observed MC values for \code{trait1} and \code{trait0}.}
#'   \item{null_distributions}{List with permuted MC vectors for each state.}
#'   \item{pvalues}{\code{data.frame} of permutation p-values for each state.}
#' }
#'
#' @details See \href{https://pubmed.ncbi.nlm.nih.gov/16103186/}{Salemi and al, 2005}
#'
#' @examples
#' \dontrun{
#' set.seed(1)
#' tree <- ape::rcoal(30)
#' trait <- sample(c(rep("severe", 15), rep("non severe", 15)))
#'
#' res_mc <- mc_test(
#'   tree = tree, trait = trait,
#'   trait0 = "non severe", trait1 = "severe",
#'   n_simu = 100
#' )
#' res_mc$pvalues
#'}
#'
#' @references
#' Salemi and al (2005). Phylodynamic analysis of human immunodeficiency virus type 1 in distinct brain compartments provides a model for the neuropathogenesis of AIDS.
#'
#' @seealso \code{\link{calculate_MC}}
#' @export
mc_test <- function(tree, trait, trait0, trait1, n_simu = 1000) {
  cat("Monophyletic Clade (MC)...\n")
  cat(paste0("Trait 1: ", trait1, "\nTrait 0: ", trait0, "\n"))
  mc_obs <- calculate_MC(tree, trait, trait0, trait1)
  res <- random_MC_scores(tree, trait, trait0, trait1, n_simu)
  pvals <- calculate_pvalue_for_MC(mc_obs, res, n_simu)
  return(list(mc_obs = mc_obs, null_distributions = res, pvalues = pvals))
}

#' Compute Monophyletic Clade size (MC) for two states
#'
#' For each of the two states (\code{trait1} and \code{trait0}), computes the size
#' of the largest internal clade whose descendant tips all share that state
#' (i.e., a monophyletic clade for that state).
#'
#' @param tree A phylogenetic tree of class \code{"phylo"}.
#' @param trait A vector of tip states (length = \code{ape::Ntip(tree)}), ordered as \code{tree$tip.label}.
#' @param trait0 Character scalar: label for state "0".
#' @param trait1 Character scalar: label for state "1".
#'
#' @return Named numeric vector of length 2 with MC values for \code{trait1} and \code{trait0}.
#'
#' @details If a state is absent from \code{trait}, its MC is returned as 0.
#' See \href{https://pubmed.ncbi.nlm.nih.gov/16103186/}{Salemi and al, 2005}.
#'
#' @references
#' Salemi and al (2005). Phylodynamic analysis of human immunodeficiency virus type 1 in distinct brain compartments provides a model for the neuropathogenesis of AIDS.
#'
#' @seealso \code{\link{get_tips_descendants}}, \code{\link{mc_test}}
#' @export
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

#' Compute phylogenetic association statistics (AI, PS, MC)
#'
#' Convenience wrapper to compute three phylogenetic association statistics:
#' Association Index (AI), Parsimony Score (PS), and Monophyletic Clade size (MC),
#' including permutation p-values and null distributions.
#'
#' @param tree A phylogenetic tree of class \code{"phylo"}.
#' @param trait A vector of tip states (length = \code{ape::Ntip(tree)}), ordered as \code{tree$tip.label}.
#' @param trait0 Character scalar: label for state "0".
#' @param trait1 Character scalar: label for state "1".
#' @param n_simu Number of permutations used for PS and MC (default: 1000).
#' @param alpha Significance level (currently not used; kept for convenience).
#'
#' @return A nested list with components \code{AI}, \code{PS}, and \code{MC}.
#' Each contains the statistic(s), p-value(s), and corresponding null distribution(s).
#'
#' @details \code{AI} is computed via \code{\link{ai_test}} (which has its own default \code{n_simu = 1000}).
#' In the current implementation, \code{phylo.stats()} does not pass \code{n_simu} to \code{ai_test()}.
#'
#' @examples
#' \dontrun{
#' set.seed(1)
#' tree <- ape::rcoal(30)
#' trait <- sample(c(rep("severe", 15), rep("non severe", 15)))
#'
#' phylo_stat <- phylo.stats(
#'   tree = tree, trait = trait,
#'   trait0 = "non severe", trait1 = "severe",
#'   n_simu = 100
#' )
#' phylo_stat$AI$pvalue
#' phylo_stat$PS$pvalue
#' phylo_stat$MC$pvalues
#' }
#'
#' @references
#' PS: Fitch, W. M. (1971). Toward defining the course of evolution: minimum change for a specific tree topology.
#' AI: Wang and al (2001). Identification of Shared Populations of Human Immunodeficiency Virus Type 1 Infecting Microglia and Tissue Macrophages outside the Central Nervous System.
#' MC: Salemi and al (2005). Phylodynamic analysis of human immunodeficiency virus type 1 in distinct brain compartments provides a model for the neuropathogenesis of AIDS.
#'
#' @seealso \code{\link{ai_test}}, \code{\link{ps_test}}, \code{\link{mc_test}}
#' @export
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

#' Get descendant tip indices of a node
#'
#' Returns the indices of descendant tips (i.e., values in \code{1:ape::Ntip(tree)})
#' for a given internal (or any) node.
#'
#' @param tree A phylogenetic tree of class \code{"phylo"}.
#' @param node Integer node index in the \code{"phylo"} encoding.
#'
#' @return Integer vector of tip indices (subset of \code{1:ape::Ntip(tree)}).
#'
#' @importFrom ape Ntip
#' @importFrom phangorn Descendants
#' @export
get_tips_descendants <- function(tree, node) {
  descendants <- phangorn::Descendants(tree, node, "all")
  internal_descendants <- descendants[descendants <= ape::Ntip(tree)]
  return(internal_descendants)
}


