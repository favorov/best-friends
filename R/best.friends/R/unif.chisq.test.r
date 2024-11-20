#'
#' unif.chisq.test
#' 
#' returns p-value for uniformity of the ranks (they are in \eqn{1 .. N}) vector
#' See [best.friends] documentation for details.
#' See https://mc-stan.org/docs/2_26/stan-users-guide/testing-uniformity.html 
#' for the formulas
#' 
#' @param ranks vector of ranks of a tag in different collections, \eqn{1 .. N})
#' @return p-value for the KS test comparing the ranks distribution with uniform
#' @importFrom stats chisq.test
#' @examples
#' example(tag.int.ranks)
#' chisq.p.vals<-apply(TF.ranks,1,"unif.ks.test")
#' @export
unif.chisq.test<-function(ranks){
  jranks<-jitter(ranks,amount=0.1E-6)
  res<-ks.test(jranks,"punif",min = min(jranks),max=max(jranks))
  res$p.value
}
