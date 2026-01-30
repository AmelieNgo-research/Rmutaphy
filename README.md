# MutaPhy

This repository contains the R implementation of **MutaPhy**, a method for detecting shared mutations associated with a trait in a phylogenetic framework, developed during the PhD thesis of *Amélie Ngo*.

MutaPhy is designed to study genotype–phenotype associations in a phylogenetic framework. The method detects clades significantly enriched for a binary phenotype and identifies candidate mutations that may underlie this association using permutation-based statistical tests and ancestral sequence reconstruction.

## Simulations

Simulation frameworks implemented in this repository are used to evaluate the performance of MutaPhy under controlled evolutionary scenarios. Synthetic viral phylogenies are generated, along which mutations are propagated, and a binary phenotype (severe vs. non-severe), observed in infected hosts and associated with the corresponding viral sequences, is assigned to the tree tips according to a probabilistic model.

These simulated datasets are then analyzed using MutaPhy to detect viral clades enriched for the severe phenotype and to assess the method’s ability to recover the mutations underlying the observed associations.  
Performance is evaluated across a range of evolutionary scenarios, including different tree sizes, noise levels, and proportions of causal mutations.

## Application to dengue virus

MutaPhy was applied to real genomic and clinical data from a dengue virus cohort, in which the phenotype corresponds to clinical severity observed in infected patients.  
This application illustrates the ability of the method to detect localized phylogenetic clustering of severe cases and to explore candidate viral mutations potentially associated with disease severity.

## Usage (`mutaphy_test()`)

MutaPhy expects a phylogenetic tree (`phylo`) whose tip labels encode a binary phenotype. The main entry point is:

- `mutaphy_test(tree, trait1, trait0, n_simu, alpha)`
  
--  `trait1` corresponds to the phenotype of interest

-- `trait0` to the reference phenotype

-- Tip labels in the phylogenetic tree are expected to match either `trait1` or `trait0`

-- The parameter `n_simu` specifies the number of permutations used to generate the null distribution of association statistics

-- `alpha` defines the significance threshold (typically set to 0.05)

It returns subtree-level p-values (hypergeometric and permutation-based), a tree-level summary (minimum p-values), and additional objects used for p-value correction.

## Candidate sites (`get_site_candidates()`)

After running `mutaphy_test()`, candidate mutations can be explored on the branch leading to a significant clade using `get_site_candidates()`.

- `get_site_candidates(nodes, tree, sequences, verbose)`

-- `nodes`: internal node ID corresponding to a significant subtree (typically taken from
  `res$positifs$permutation_nodes_corrected` or `res$positifs$permutation_nodes`)
  
-- `tree`: a `phylo` object whose tip labels match the sequence names used in `sequences`. (In practice, this means using the original sequence identifiers as `tree$tip.label`)

-- `sequences`: a named list of aligned sequences, where each element is a character vector of nucleotides (A/C/G/T) and the list names correspond exactly to the tree tip labels.


Example scripts are provided in the `Dengue_data/` directory, including a dengue virus use case.

