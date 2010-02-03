
# Turn graphLayout() into a generic

setGeneric("graphLayout",
           function(graph, layoutType, ...) {
               standardGeneric("graphLayout")
           })

setMethod("graphLayout",
          signature(graph="Ragraph"),
          Rgraphviz::graphLayout)

# Method for graphBPH to create an RagraphBPH

layoutGraphBPH <- function(graph, layoutType="dot", ...) {
    rag <- agopen(graph@graph, name="rag",
                  layoutType=layoutType, ...)
    new("RagraphBPH", 
        graph=rag,
        allNodes=nodes(graph@graph),
        nodes=graph@nodes,
        edgeNodes=graph@edgeNodes,
                  edgeNodeIO=graph@edgeNodeIO)
}

setMethod("graphLayout",
          signature(graph="graphBPH", layoutType="missing"),
          function(graph, layoutType, ...) {
              layoutGraphBPH(graph, ...)
          })

setMethod("graphLayout",
          signature(graph="graphBPH", layoutType="character"),
          function(graph, layoutType, ...) {
              layoutGraphBPH(graph, layoutType, ...)
          })

drawRagraphBPH <- function(x, newpage) {
    if (newpage) {
        grid.newpage()
    }
    nodes <- x@allNodes
    rag <- x@graph
              # (x, y) locations of all the nodes
              # The order is the same as the nodes in
              # the original graphNEL
    xy <- getNodeXY(rag)
    names(xy$x) <- nodes
    names(xy$y) <- nodes
              # For each edgeNode, what's the average angle at which
              # edges pass through?
    edgeNodeDir <- mapply(edgeNodeTangent,
                          x@edgeNodes,
                          x@edgeNodeIO$incoming,
                          x@edgeNodeIO$outgoing,
                          MoreArgs=list(xy=xy))
    bb <- boundBox(rag)
    pushViewport(viewport(xscale=c(getX(botLeft(bb)), getX(upRight(bb))),
                          yscale=c(getY(botLeft(bb)), getY(upRight(bb)))))
    lapply(x@nodes, drawNode, xy, rag)
              lapply(x@edgeNodes, drawEdgeNode, xy, x@edgeNodeIO)
    lapply(edgeNames(rag), drawHyperEdge, xy, x@edgeNodes, edgeNodeDir, rag)
    lapply(x@edgeNodes, drawLabel, xy, x@edgeNodeIO, edgeNodeDir, rag)
              # Redraw main nodes (to cover bleeding edges)
    lapply(x@nodes, drawNode, xy, rag)
    popViewport()
}

setMethod("plot",
          signature(x="RagraphBPH"),
          function(x, y, newpage=TRUE, ...) {
              drawRagraphBPH(x, newpage)
          })

setMethod("graphDataDefaults<-",
          signature(self="RagraphBPH", attr="character", value="ANY"),
          function(self, attr, value) {
              graphDataDefaults(self@graph, attr) <- value
              self
          } )

setMethod("graphData<-",
          signature(self="RagraphBPH", attr="character", value="ANY"),
          function(self, attr, value) {
              graphData(self@graph, attr) <- value
              self
          })

setMethod("edgeDataDefaults<-",
          signature(self="RagraphBPH", attr="character", value="ANY"),
          function(self, attr, value) {
              edgeDataDefaults(self@graph, attr) <- value
              self
          } )

setMethod("edgeData<-",
          signature(self="RagraphBPH", from="character", to="character",
                    attr="character", value="ANY"),
          function(self, from, to, attr, value) {
              edgeData(self@graph, from, to, attr) <- value
              self
          })

setMethod("nodeDataDefaults<-",
          signature(self="RagraphBPH", attr="character", value="ANY"),
          function(self, attr, value) {
              nodeDataDefaults(self@graph, attr) <- value
              self
          } )

setMethod("nodeData<-",
          signature(self="RagraphBPH", n="character",
                    attr="character", value="ANY"),
          function(self, n, attr, value) {
              nodeData(self@graph, n, attr) <- value
              self
          })


