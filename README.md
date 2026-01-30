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
