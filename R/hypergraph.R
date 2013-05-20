
# Converting a 'hypergraph' package "Hypergraph" into a "graphBPH"

setMethod("graphBPH",
          signature(graph="Hypergraph", edgeNodePattern="missing"),
          function(graph, edgeNodePattern, ...) {
              hEdges <- hyperedges(graph)
              nodes <- nodes(graph)
              # Only valid for Hypergraph with all Hyperedges
              # DirectedHyperedges
              if (!all(sapply(hEdges, is, "DirectedHyperedge")))
                  stop("All hyperedges must be directed hyperedges")
              # Convert Hypergraph to graphNEL
              hEdgeNames <- hyperedgeLabels(graph)
              if (is.null(hEdgeNames)) 
                  hEdgeNames <- as.character(1:length(hEdges))
              if (any(hEdgeNames %in% nodes)) 
                  stop("hyperedge names must be distinct from node names")
              gnelNodes <- c(nodes, hEdgeNames)
              edgeList <- vector("list", length(gnelNodes))
              names(edgeList) <- gnelNodes
              for (i in 1:length(hEdges)) {
                  # Edges from this edge-node to other nodes
                  hename <- hEdgeNames[i]
                  newEdges <- hypergraph::tail(hEdges[[i]])
                  if (is.null(edgeList[[hename]])) {
                      edgeList[[hename]] <-
                          list(edges=newEdges)
                  } else {
                      edgeList[[hename]] <-
                          list(edges=c(edgeList[[hename]]$edges, newEdges))
                  }
                  # Edges from other nodes to this edge-node
                  headNodes <- hypergraph::head(hEdges[[i]])
                  headList <- lapply(headNodes, list, edges=hEdgeNames[i])
                  for (j in 1:length(headList)) {
                      hename <- headNodes[j]
                      if (is.null(edgeList[[hename]])) {
                          edgeList[[hename]] <- headList[[j]]
                      } else {
                          edgeList[[hename]] <-
                              list(edges=c(edgeList[[hename]]$edges,
                                     headList[[j]]$edges))
                      }
                  }
              }
              gnel <- new("graphNEL", nodes=gnelNodes,
                          edgeL=edgeList, edgemode="directed")
              # Standard initialization
              new("graphBPH", gnel,
                  edgeNodePattern=paste(paste("^", hEdgeNames, "$", sep=""),
                    collapse="|", sep=""))
          })
