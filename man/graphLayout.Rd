\name{graphLayout}
\alias{graphLayout}
\title{
  Layout a graph.
}
\description{
  This function is designed to layout a graph using
  the \pkg{Rgraphviz} package.
  The \pkg{hyperdraw} package makes this a generic function with a method for
  \code{graphBPH} objects.
  The function of the same name in the \pkg{Rgraphviz}
  package is used as a method for \code{Ragraph} objects.
}
\usage{
graphLayout(graph, layoutType, ...)
}
\arguments{
  \item{graph}{
    An   \code{graphBPH} object, which is to be laid out.
  }
  \item{layoutType}{
    The layout method (e.g., \code{dot} or \code{neato}).
  }
  \item{\dots}{
    These arguments will be passed to the \code{agopen()} function.
  }
}
\value{
  An \code{RagraphBPH} object.
}
\references{
Gansner, E.R. and  and North, S.C. (1999)
 An open graph visualization system and its applications to software
  engineering,
\emph{Software - Practice and Experience}, 30:1203--1233.

Gentry, J. and Long, L. and Gentleman, R. and Falcon, S. and
Hahne, F. and Sarkar, D. and Hansen, K. 
\pkg{Rgraphviz}: Provides plotting capabilities for R graph objects.
}
\author{
Paul Murrell
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{agopen}} and
  \code{\link{GraphvizLayouts}}
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
testbph <- new("graphBPH", testgnel, "^R")
testrabph <- graphLayout(testbph)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ dplot }
