
###################
# Initialisation

# MUST supply a graph and an edgeNodePattern when create a graphBPH
setMethod("initialize", "graphBPH",
          function(.Object,
                   graph,
                   edgeNodePattern,
                   ...) {
              
              # Calculate the sets of normal nodes and edge nodes
              nodes <- nodes(graph)
              edgeNodeIndex <- grep(edgeNodePattern, nodes)
              normalNodes <- nodes[-edgeNodeIndex]
              edgeNodes <- nodes[edgeNodeIndex]

              # Calculate which nodes enter or leave each edgeNode
              edges <- edges(graph)
              edgeNodeOutgoing <- edges[edgeNodes]
              edgeNodeIncoming <- inEdges(edgeNodes, graph)

              # Fill the slots of the object
              .Object@graph <- graph
              .Object@edgeNodePattern <- edgeNodePattern
              .Object@nodes <- normalNodes
              .Object@edgeNodes <- edgeNodes
              .Object@edgeNodeIO <- list(incoming=edgeNodeIncoming,
                                         outgoing=edgeNodeOutgoing)
              
              validGraphBPH(.Object)
          })

# Convenience
setGeneric("graphBPH",
           function(graph, edgeNodePattern, ...) {
               standardGeneric("graphBPH")
           })

setMethod("graphBPH",
          signature(graph="graphNEL", edgeNodePattern="character"),
          function(graph, edgeNodePattern, ...) {
              new("graphBPH", graph, edgeNodePattern)
          })

###################
# Validation

validEdgeDest <- function(dest, validDest) {
    all(dest %in% validDest)
}

# Check for a valid graphBPH object
validGraphBPH <- function(obj) {
    # graphNEL must be "directed"
    # if (edgemode(obj@graph) != "directed")
    #     stop("Must be a directed graph")
    
    # Check that ALL edges are between
    # an "edge node" and a "normal node"
    edges <- edges(obj@graph)
    
    if (!all(sapply(edges[obj@nodes], validEdgeDest, obj@edgeNodes)) ||
        !all(sapply(edges[obj@edgeNodes], validEdgeDest, obj@nodes)))
        stop("All edges must be between a normal node and an edge node")
    
    obj
}


###################
# Plot method

setMethod("plot",
          signature(x="graphBPH", y="ANY"),
          function(x, y, layoutType="dot", ..., newpage=TRUE) {
              # Convert the graphNEL to an Ragraph
              # and lay it out
              # (this uses Rgraphviz)
              # NOTE:  '...' args are passed to graphLayout()
              # (which passes them to agopen())
              ragBPH <- graphLayout(x, layoutType=layoutType, ...)
              drawRagraphBPH(ragBPH, newpage)
          })
              
          
###################
# Print method ?
