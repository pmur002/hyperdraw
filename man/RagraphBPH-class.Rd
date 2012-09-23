\name{RagraphBPH-class}
\Rdversion{1.1}
\docType{class}
\alias{RagraphBPH}
\alias{RagraphBPH-class}
\alias{plot,RagraphBPH,ANY-method}
\alias{edgeDataDefaults<-,RagraphBPH,character,ANY-method}
\alias{edgeData<-,RagraphBPH,character,character,character,ANY-method}
\alias{nodeDataDefaults<-,RagraphBPH,character,ANY-method}
\alias{nodeData<-,RagraphBPH,character,character,ANY-method}

\title{Class "RagraphBPH"}
\description{The purpose of this class is to represent a
  laid out version of a \code{graphBPH} object.
  The laying out is performed by the \pkg{Rgraphviz} package.
  This is an intermediate step in the process of drawing
  a \code{graphBPH} object. 
}
\section{Objects from the Class}{
  Objects of this class should be created via the
  \code{graphLayout()} function.
}
\section{Slots}{
  \describe{
    \item{\code{graph}:}{Object of class \code{Ragraph}.
      The laid out graph.}
    \item{\code{allNodes}:}{Object of class \code{character}.
      The names of all nodes in the graph.}
    \item{\code{nodes}:}{Object of class \code{character}.
      Records normal nodes in the graph.}
    \item{\code{edgeNodes}:}{Object of class \code{character}.
      Records edge-nodes in the graph. }
    \item{\code{edgeNodeIO}:}{Object of class \code{list}.
      Records which edges enter and exit each edge-node. }
  }
}
\section{Methods}{
  \describe{
    \item{plot}{\code{signature(x = "RagraphBPH", y = "ANY")}:
      draw a representation of the hypergraph where edges between
      normal nodes in the graph pass through an intermediate
      edge-node in a nice smooth curve. }
    \item{edgeDataDefaults<-}{\code{signature(self = "RagraphBPH",
        attr = "character", value = "ANY")}:
      set the default drawing attributes for all edges.}
    \item{edgeData<-}{\code{signature(self = "RagraphBPH",
        from = "character", to = "character", attr = "character",
        value = "ANY")}:
      set a specific drawing attribute for one or more edges. }
    \item{nodeDataDefaults<-}{\code{signature(self = "RagraphBPH",
        attr = "character", value = "ANY")}:
      set the default drawing attributes for all nodes.}
    \item{nodeData<-}{\code{signature(self = "RagraphBPH",
        n = "character", attr = "character", value = "ANY")}:
      set a specific attribute for one or more nodes. }
  }
}
\author{ Paul Murrell }
\seealso{
  \code{\link{graphLayout}},
  \code{\linkS4class{graphBPH}},
  and
  \code{\linkS4class{Ragraph}}
}
\examples{
nodes <- c(LETTERS[1:5], paste("R", 1:3, sep=""))
testgnel <- new("graphNEL",
                nodes=nodes,
                edgeL=list(
                  A=list(edges=c("R1", "R2")),
                  B=list(edges="R2"),
                  C=list(),
                  D=list(edges="R3"),
                  E=list(),
                  R1=list(edges="B"),
                  R2=list(edges=c("C", "D")),
                  R3=list(edges="E")),
                edgemode="directed")
testbph <- graphBPH(testgnel, "^R")
testrabph <- graphLayout(testbph)
edgeDataDefaults(testrabph, "lwd") <- 1
edgeData(testrabph, c("A", "R1"), c("R1", "B"), "lwd") <- c("3", 5)
edgeDataDefaults(testrabph, "color") <- "black"
edgeData(testrabph, c("A", "R1"), c("R1", "B"), "color") <- "red"
nodeDataDefaults(testrabph, "margin") <- 'unit(2, "mm")'
nodeDataDefaults(testrabph, "shape") <- "circle"
plot(testrabph)
graphDataDefaults(testrabph, "arrowLoc") <- "middle"
graphData(testrabph, "arrowLoc") <- "end"
plot(testrabph)
graphData(testrabph, "arrowLoc") <- "none"
plot(testrabph)
}
\keyword{classes}
