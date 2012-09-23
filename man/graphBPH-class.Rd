\name{graphBPH-class}
\Rdversion{1.1}
\docType{class}
\alias{graphBPH-class}
\alias{plot,graphBPH,ANY-method}
\alias{graphLayout,graphBPH,missing-method}
\alias{graphLayout,graphBPH,character-method}

\title{Class "graphBPH" }
\description{A bipartite representation of a hypergraph.
  The purpose of this class is to support visualization
  of the hypergraph;  it is not intended for analysis or manipulation
  of the hypergraph. }
\section{Objects from the Class}{
  Objects can be created by calls of the form \code{new("graphBPH",
    graph, edgeNodePattern, ...)}.
  There is also a convenience function \code{graphBPH()}.
  
  A \code{graphBPH} object consists of a \code{graphNEL}
  object, which must obey some strict rules:
  \itemize{
    \item nodes in the graph are divided into two sets:
      normal nodes and edge-nodes,
    \item all edges in the graph
      must connect a normal node to an edge node,
    \item the graph must be a directed graph.
  }
  The \code{edgeNodePattern} is a regular expression that is
  used to define the set of edge-nodes.
}
\section{Slots}{
  \describe{
    \item{\code{graph}:}{Object of class \code{graphNEL}.  This graph
    must obey the constraints described above. }
  \item{\code{edgeNodePattern}:}{Object of class \code{character}.
    The regular expression used to define edge-nodes. }
  \item{\code{nodes}:}{Object of class \code{character}.
    Records which nodes in the graph are normal nodes. }
  \item{\code{edgeNodes}:}{Object of class \code{character}.
    Records which nodes in the graph are edge-nodes. }
    \item{\code{edgeNodeIO}:}{Object of class \code{list}.
      Records information about which edges enter and exit
      each edge-node.}
  }
}
\section{Methods}{
  \describe{
    \item{plot}{\code{signature(x = "graphBPH", y = "ANY")}:
      draw a representation of the hypergraph where edges between
      normal nodes in the graph pass through an intermediate
      edge-node in a nice smooth curve. }
    \item{graphLayout}{\code{signature(graph = "graphBPH",
          layoutType = "missing")}:
        convert the \code{graphBPH} object to a
        \code{RagraphBPH} object (using a default layout method).
      }
      \item{graphLayout}{\code{signature(graph = "graphBPH",
          layoutType = "character")}:
        convert the \code{graphBPH} object to a
        \code{RagraphBPH} object (using the specified layout method).
      }
    }
}
\references{
Gansner, E.R. and  and North, S.C. (1999)
 An open graph visualization system and its applications to software
  engineering,
\emph{Software - Practice and Experience}, 30:1203--1233.

Gentleman, R. and Whalen, E. and Huber, W. and Falcon, S. 
\pkg{graph}: A package to handle graph data structures.

Gentry, J. and Long, L. and Gentleman, R. and Falcon, S. and
Hahne, F. and Sarkar, D. and Hansen, K. 
\pkg{Rgraphviz}: Provides plotting capabilities for R graph objects.
}
\author{ Paul Murrell }
\seealso{
	\code{\link{agopen}},
	\code{\link{graphLayout}} and
	\code{\linkS4class{graphNEL}}
	\code{\linkS4class{RagraphBPH}}
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
plot(testbph)

# A Hypergraph equivalent
dh1 <- DirectedHyperedge("A", "B", "R1")
dh2 <- DirectedHyperedge(c("A", "B"), c("C", "D"), "R2")
dh3 <- DirectedHyperedge("D", "E", "R3")
hg <- Hypergraph(LETTERS[1:5], list(dh1, dh2, dh3))
plot(graphBPH(hg))                 
}
\keyword{classes}
