#'
#' unif.ks.test
#' 
#' returns p-value for uniformity of the ranks (they are in \eqn{1 .. N}) vector
#' #' See [best.friends] documentation for details.
#' 
#' @param ranks vector of ranks of a tag in different collections, \eqn{1 .. N})
#' @papram maxrank \eqn{N}
#' @return p-value for the KS test comparing the ranks distribution with uniform
#'
#' @examples
#' example(tag.int.ranks)
#' ks.p.vals<-apply(TF.ranks,2,tag.int.ranks,genes)
#' @export
unif.ks.test<-function(ranks,maxrank){
  res<-ks.test(ranks,"punif",min = 1,max=maxrank)
  res$p.value
}
