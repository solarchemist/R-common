% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/numeric.R
\name{trapz}
\alias{trapz}
\title{Calculate area under a curve}
\usage{
trapz(x, y)
}
\arguments{
\item{x}{vector (of length n)}

\item{y}{vector (of length n)}
}
\value{
vector (of length n - 1)
}
\description{
Numerically calculate area under an arbitrary curve (defined by x, y coord pairs)
using trapezodial integration. See Wikipedia for more info on trapz integration.
}
