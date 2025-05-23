% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/unif.ks.test.r
\name{unif.ks.test}
\alias{unif.ks.test}
\title{unif.ks.test}
\usage{
unif.ks.test(ranks, uniform.max = NA)
}
\arguments{
\item{ranks}{vector of ranks of a tag in different collections, \eqn{1 .. N})}

\item{uniform.max}{the maximal rank in the uniform, default is NA which means
that we take the max(ranks) as the maximal value}
}
\value{
p-value for the KS test comparing the ranks distribution with uniform
}
\description{
returns p-value for uniformity of the ranks (they are in \eqn{1 .. N}) vector
#' See \link{friends.test} documentation for details.
}
\examples{
example(tag.int.ranks)
ks.p.vals<-apply(TF.ranks,1,"unif.ks.test")
}
