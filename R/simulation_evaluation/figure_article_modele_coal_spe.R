#### Coalescent vs random tree
set.seed(42)
n <- 10

tree_coal <- rcoal(n)
tree_spec <- rtree(n)

par(mfrow = c(1, 2))
plot(tree_coal, main = "Arbre coalescent (rcoal)")
plot(tree_spec, main = "Arbre à topologie aléatoire (rtree)")
