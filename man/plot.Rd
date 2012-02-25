\name{plot}
\alias{plot}

\title{
  plot
}
\description{

  NOTE: This man page is for the \code{plot} S4 generic function defined
  in this package.  See \code{?graphics::\link[graphics]{plot}} for the
  default S3 methods.
}
\usage{
plot(x, y, ...)
}
\arguments{
  \item{x, y, ...}{See \code{?graphics::\link[graphics]{plot}}}
}
\value{
  See \code{?graphics::\link[graphics]{plot}}
}
\seealso{

  \code{\link[graphics]{plot}} in the \pkg{graphics} package for the
  default \code{plot} methods.

}
\examples{
plot
showMethods("plot")
selectMethod("plot", "ANY")  # the default method
}

\keyword{ methods }
