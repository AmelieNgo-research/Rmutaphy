# Loading of functions
source(here::here("R/MutaPhy/functions.R"))
source(here::here("R/simulation_evaluation/functions.R"))

# Tree loading
tree_300 <- read.tree(here::here("Dengue_data/dengue_300.phy_phyml_tree.txt"))
tree_300$tip.label[1:2]

# Pre-processing
name_leaf <- tree_300$tip.label
color <- ifelse(grepl("_pl_", name_leaf), "red", "blue")
plot.phylo(tree_300, tip.color = color, edge.width = 1,cex = 0.4)

name_leaf_new<- ifelse(grepl("_pl_", name_leaf), "pl",
                       ifelse(grepl("_npl_", name_leaf), "npl", name_leaf))
tree_300$tip.label <- name_leaf_new

# Test
dengue_mutaphy <- mutaphy_test(tree = tree_300,
                               trait1 = "pl",
                               trait0 = "npl",
                               n_simu = 1000,
                               alpha = 0.05)

# Descendants of the positive node
tree_300 <- read.tree(here::here("Dengue_data/dengue_300.phy_phyml_tree.txt"))
descendant_indices <- phangorn::Descendants(tree_300, 360, type = "tips")[[1]]

descendant_names <- tree_300$tip.label[descendant_indices]
descendant_names

# Viral sequences
library(seqinr)
sequences <- read.fasta(file = "Dengue_data/300_filtered_sequences.fasta", seqtype = "DNA")


sequences <- lapply(sequences, function(seq) {
  nucs <- toupper(as.character(seq))
  nucs[! nucs %in% c("A","C","G","T")] <- NA
  nucs
})

# Candidates mutations?
dengue_site <- get_site_candidates(nodes = 360,
                    tree = tree_300,
                    sequences = sequences,
                    verbose = TRUE)



### draft

library(ape)


is_variable <- function(pos, seqs) {
  vals <- unique(na.omit(sapply(seqs, `[`, pos)))
  length(vals) > 1
}

variable_sites <- which(sapply(1:length(sequences[[1]]), is_variable, seqs = sequences))


lik_node360 <- lapply(variable_sites, function(pos) {
  site <- sapply(sequences, `[`, pos)
  names(site) <- names(sequences)


  common <- intersect(names(site), tree_300$tip.label)
  site   <- site[common]
  tree_sub <- keep.tip(tree_300, common)


  fit <- tryCatch({
    ace(site, tree_sub, type = "discrete", model = "ER")
  }, error = function(e) NULL)

  if (is.null(fit)) return(NULL)


  if ("360" %in% rownames(fit$lik.anc)) {
    return(fit$lik.anc["360", ])
  } else {
    return(NULL)
  }
})


lik_node360_df <- do.call(rbind, lik_node360)
rownames(lik_node360_df) <- variable_sites[!sapply(lik_node360, is.null)]


head(round(lik_node360_df, 3))

unique(unlist(sequences))

is_variable <- function(pos, seqs) {
  vals <- unique(na.omit(sapply(seqs, `[`, pos)))
  length(vals) > 1
}


sequences <- lapply(sequences, function(seq) {
  toupper(as.character(seq))
})
seq_mat <- do.call(rbind, sequences)
dim(seq_mat)

View(seq_mat[, 1:40])

variable_sites <- which(sapply(1:length(sequences[[1]]), is_variable, seqs = sequences))
site <- sapply(sequences, `[`, 2311)
names(site) <- names(sequences)
res <- ace(site, tree_300, type = "discrete", model = "ER")
tree_300 <- root(tree_300, outgroup = tree_300$tip.label[1], resolve.root = TRUE)
