\name{graphBPH}
\alias{graphBPH}
\alias{graphBPH,graphNEL,character-method}
\alias{graphBPH,Hypergraph,missing-method}
\title{Constructor for graphBPH objects}
\description{
  A convenience constructor for \code{\link{graphBPH-class}}
  objects. This is a generic function.
}
\usage{
graphBPH(graph, edgeNodePattern, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{graph}{Some form of graph that is to be converted into a
    \code{graphBPH} object.}
  \item{edgeNodePattern}{A regular expression used to distinguish
    between normal nodes and edge nodes.}
  \item{...}{ Potential arguments to other methods. }
}
\section{Methods}{
  \describe{
    \item{graphBPH}{
      \code{signature(graph = "graphNEL", edgeNodePattern = "character")}:
      create a \code{graphBPH} object from a (directed) \code{graphNEL}
      object. }
    \item{graphBPH}{
      \code{signature(graph = "Hypergraph", edgeNodePattern = "missing")}:
      create a \code{graphBPH} object from a \code{Hypergraph}
      object (where all \code{Hyperedges} are
      \code{DirectedHyperedges}). }
    }
}
\value{
  An object of class \code{\link{graphBPH-class}}
}
\references{
  Falcon, S. and Gentleman, R.
  \pkg{hypergraph}: A package providing hypergraph data structures.

  Gentleman, R. and Whalen, E. and Huber, W. and Falcon, S. 
\pkg{graph}: A package to handle graph data structures.
}
\author{Paul Murrell}
\seealso{
  \code{\link{graphBPH-class}}
}
\keyword{classes}
