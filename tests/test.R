
library(hyperdraw)

# Check graph validation

# Edges must be directed

badgnel.1 <- new("graphNEL",
                nodes=c("A", "R"),
                edgeL=list(
                  A=list(edges="R"),
                  R=list(edges="A")))
stopifnot(inherits(try(graphBPH(badgnel.1, "")), "try-error"))

# All edges must be between normal node and edge node
badgnel.2 <- new("graphNEL",
                 nodes=c("A", "B"),
                 edgeL=list(
                   A=list(edges="B"),
                   B=list(edges="A")),
                 edgemode="directed")
stopifnot(inherits(try(graphBPH(badgnel.2, "")), "try-error"))

# If it's a Hypergraph, all Hyperedges must be DirectedHyperedges

require(hypergraph)

badhg <- Hypergraph(c("A", "B"), list(Hyperedge(c("A", "B"))))
stopifnot(inherits(try(graphBPH(badhg)), "try-error"))

# Examples in man pages test simple examples that should work

