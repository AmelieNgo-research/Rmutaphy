# MutaPhy

This repository contains the R implementation of **MutaPhy**, a method for detecting shared mutations associated with a trait in a phylogenetic framework, developed during the PhD thesis of *Amélie Ngo*.

MutaPhy is designed to study genotype–phenotype associations in a phylogenetic framework. The method detects clades significantly enriched for a binary phenotype and identifies candidate mutations that may underlie this association using permutation-based statistical tests and ancestral sequence reconstruction.

## Simulations
Folder: `analysis/`
Functions: `analysis/simulation/evaluation/functions.R`
Results: `analysis/results`
Simulation frameworks implemented in this repository are used to evaluate the performance of MutaPhy under controlled evolutionary scenarios. Synthetic viral phylogenies are generated, along which mutations are propagated, and a binary phenotype (severe vs. non-severe), observed in infected hosts and associated with the corresponding viral sequences, is assigned to the tree tips according to a probabilistic model.

These simulated datasets are then analyzed using MutaPhy to detect viral clades enriched for the severe phenotype and to assess the method’s ability to recover the mutations underlying the observed associations.  
Performance is evaluated across a range of evolutionary scenarios, including different tree sizes, noise levels, and proportions of causal mutations.

## Usage (`mutaphy_test()`)

MutaPhy expects a phylogenetic tree (`phylo`) whose tip labels encode a binary phenotype. The main entry point is:

- `mutaphy_test(tree, trait1, trait0, n_simu, alpha)`
  
--  `trait1` corresponds to the phenotype of interest

-- `trait0` to the reference phenotype

-- Tip labels in the phylogenetic tree are expected to match either `trait1` or `trait0`

-- The parameter `n_simu` specifies the number of permutations used to generate the null distribution of association statistics

-- `alpha` defines the significance threshold (typically set to 0.05)

It returns subtree-level p-values (hypergeometric and permutation-based), a tree-level summary (minimum p-values), and additional objects used for p-value correction.

### Example

```r
source(here::here("R/functions.R"))
set.seed(1)
tree <- ape::rcoal(30)
id_leaf <- tree$tip.label
trait <- c("severe", "non severe", "non severe", "severe", "severe",
           "non severe", "non severe", "severe", "non severe", "non severe",
           "non severe", "severe", "non severe", "non severe", "non severe",
           "non severe", "non severe", "non severe", "non severe", "severe",
           "severe", "severe", "severe", "severe", "severe", "severe",
           "severe", "severe", "severe", "severe")
tree$tip.label <- trait

res <- mutaphy_test(
  tree   = tree,
  trait1 = "severe",
  trait0 = "non severe",
  n_simu = 1000,
  alpha  = 0.05
)
node <- res$positifs$permutation_nodes_corrected # 35
```

## Candidate sites (`get_candidate_sites()`)

After running `mutaphy_test()`, candidate mutations can be explored on the branch leading to a significant clade using `get_candidate_sites()`.

- `get_candidate_sites(nodes, tree, sequences, verbose)`

-- `nodes`: internal node ID corresponding to a significant subtree (typically taken from
  `res$positifs$permutation_nodes_corrected` or `res$positifs$permutation_nodes`)
  
-- `tree`: a `phylo` object whose tip labels match the sequence names used in `sequences`. (In practice, this means using the original sequence identifiers as `tree$tip.label`)

-- `sequences`: a named list of aligned sequences, where each element is a character vector of nucleotides (A/C/G/T) and the list names correspond exactly to the tree tip labels.

### Example

```r
get_descendant_tips <- function(tree, node) {
  descendants <- phangorn::Descendants(tree, node, "all")
  internal_descendants <- descendants[descendants <= ape::Ntip(tree)]
  return(internal_descendants)
}

# Building of sequences
L <- 10
ref_seq <- sample(c("A","C","G","T"), L, replace = TRUE)
sequences <- replicate(ape::Ntip(tree), ref_seq, simplify = FALSE)
names(sequences) <- tree$tip.label
desc_tips <- get_descendant_tips(tree, node)
mut_pos <- 4 # Introducing variation at site 4
for (i in desc_tips) {
  sequences[[i]][mut_pos] <- "G"
}

tree$tip.label <- paste0("seq_", id_leaf)

sites <- get_candidate_sites(
  nodes     = node,
  tree      = tree,
  sequences = sequences,
  verbose   = TRUE
)
sites$candidates_by_node # Node 35, detected mutation at site 4
```

Example scripts are provided in the `analysis/Dengue_data/` directory, including a dengue virus use case.

## Additional phylogenetic association statistics (AI / PS / MC)

In addition to `mutaphy_test()`, the package provides three classic phylogenetic association
statistics computed with permutation tests:

- `ai_test(tree, trait, n_simu)` : Association Index (AI: Wang and al (2001). Identification of Shared Populations of Human Immunodeficiency Virus Type 1 Infecting Microglia and Tissue Macrophages outside the Central Nervous System. `https://journals.asm.org/doi/pdf/10.1128/jvi.75.23.11686-11699.2001`)
- `ps_test(tree, trait, n_simu)` : Parsimony Score (PS: Fitch, W. M. (1971). Toward defining the course of evolution: minimum change for a specific tree topology. `https://academic.oup.com/sysbio/article-pdf/20/4/406/4697394/20-4-406.pdf`)
- `mc_test(tree, trait, trait0, trait1, n_simu)` : Monophyletic Clade size (MC: Salemi and al (2005). Phylodynamic analysis of human immunodeficiency virus type 1 in distinct brain compartments provides a model for the neuropathogenesis of AIDS. `https://pubmed.ncbi.nlm.nih.gov/16103186/`)

A convenience wrapper, `phylo.stats()`, computes the three statistics at once and returns
a nested list with AI/PS/MC results.

### Example

```r
source(here::here("R/functions_other_statistics.R"))

res_ai <- ai_test(tree = tree, trait = trait, n_simu = 1000)
res_ai$pvalue

res_ps <- ps_test(tree = tree, trait = trait, n_simu = 1000)
res_ps$pvalue

res_mc <- mc_test(tree = tree, trait = trait,
                  trait0 = "non severe", trait1 = "severe",
                  n_simu = 1000)
res_mc$pvalues

phylo_stat <- phylo.stats(tree = tree, trait = trait,
                          trait0 = "non severe", trait1 = "severe",
                          n_simu = 1000)
phylo_stat$AI$pvalue
phylo_stat$PS$pvalue
phylo_stat$MC$pvalues
```

# References

Amélie Ngo, Stéphane Guindon and Vincent Pedergnana (2026)

Parsimony Score: Walter M. Fitch, Toward Defining the Course of Evolution: Minimum Change for a Specific Tree Topology, Systematic Biology, Volume 20, Issue 4, December 1971, Pages 406–416, https://doi.org/10.1093/sysbio/20.4.406

Association Index: Wang TH, Donaldson YK, Brettle RP, Bell JE, Simmonds P. Identification of shared populations of human immunodeficiency virus type 1 infecting microglia and tissue macrophages outside the central nervous system. J Virol. 2001 Dec;75(23):11686-99. doi: 10.1128/JVI.75.23.11686-11699.2001. PMID: 11689650; PMCID: PMC114755.

Monophyletic Clade: Salemi M, Lamers SL, Yu S, de Oliveira T, Fitch WM, McGrath MS. Phylodynamic analysis of human immunodeficiency virus type 1 in distinct brain compartments provides a model for the neuropathogenesis of AIDS. J Virol. 2005 Sep;79(17):11343-52. doi: 10.1128/JVI.79.17.11343-11352.2005. PMID: 16103186; PMCID: PMC1193641.

