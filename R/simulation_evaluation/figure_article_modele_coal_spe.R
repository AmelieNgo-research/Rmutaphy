#### Modèle de coalescense vs modèle de spéciation
set.seed(42)
n <- 10

tree_coal <- rcoal(n)
tree_spec <- rtree(n)

par(mfrow = c(1, 2))
plot(tree_coal, main = "Modèle de coalescence (rcoal)")
plot(tree_spec, main = "Modèle de spéciation (rtree)")
