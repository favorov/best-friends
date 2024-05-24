#'
#' unif.ks.test
#' 
#' returns p-value for uniformity of the ranks (they are in \eqn{1 .. N}) vector
#' #' See [best.friends] documentation for details.
#' 
#' @param ranks vector of ranks of a tag in different collections, \eqn{1 .. N})
#' @return p-value for the KS test comparing the ranks distribution with uniform
#' @importFrom stats ks.test
#' @examples
#' example(tag.int.ranks)
#' ks.p.vals<-apply(TF.ranks,1,"unif.ks.test")
#' @export
unif.ks.test<-function(ranks){
  jranks<-jitter(ranks)
  res<-ks.test(jranks,"punif",min = min(jranks),max=max(jranks))
  res$p.value
}
