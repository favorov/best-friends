% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tag.int.ranks.r
\name{tag.int.ranks}
\alias{tag.int.ranks}
\title{tag.int.ranks}
\usage{
tag.int.ranks(
  attention = NULL,
  distance_like = FALSE,
  neglect_diagonal = FALSE
)
}
\arguments{
\item{attention}{original attention matrix}

\item{distance_like}{if \code{TRUE}, ranks are ascending (the smaller is the better). Default is \code{FALSE}.}

\item{neglect_diagonal}{if square matrix, and the diagonal does not make sense, we \code{NA} it and our ranks are in \eqn{[1 .. |T|-1]}. Default is \code{FALSE}.}
}
\value{
a \eqn{|T| x |C|} matrix of integer ranks in \eqn{[1 .. |T|]} (or, \eqn{[1 .. |T|-1]} if neglect_diagonal).
}
\description{
Given the (\code{attention}) \eqn{|T| \times |C|} matrix, convert it to matrix of descending ranks of tags(rows) in collections (columns).
See \link{friends.test} documentation for details.
}
\examples{
genes.no<-100
TFs.no<-50
gene.names<-
    paste0("gene_",as.character(seq( from = 1, to = genes.no )))
TF.names<-
    paste0("TF_",as.character(seq( from = 1, to = TFs.no )))   
regulation<-matrix(rep(1,TFs.no*genes.no),
    ncol=TFs.no,byrow=FALSE
)
#we fill it with ones, now let's fill the regulation of gene 42 
#by the first 1/2 TFs  with 2
regulation[42,seq(1,TFs.no/2)]=2
rownames(regulation)<-gene.names
colnames(regulation)<-TF.names

TF.ranks<-tag.int.ranks(regulation)
}
