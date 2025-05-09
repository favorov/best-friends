#'
#' unif.ks.test
#' 
#' returns p-value for uniformity of the ranks (they are in \eqn{1 .. N}) vector
#' #' See [best.friends] documentation for details.
#' 
#' @param ranks vector of ranks of a tag in different collections, \eqn{1 .. N})
#' @param uniform.max the maximal rank in the uniform, default is NA which means
#' that we take the max(ranks) as the maximal value
#' @return p-value for the KS test comparing the ranks distribution with uniform
#' @importFrom stats ks.test
#' @examples
#' example(tag.int.ranks)
#' ks.p.vals<-apply(TF.ranks,1,"unif.ks.test")
#' @export
unif.ks.test<-function(ranks,uniform.max){
  jranks<-jitter(ranks,amount=0.1E-6)
  jrmin <- min(jranks)
  if(is.na(uniform.max)){
    jrmax <- max(jranks)
  } else {
    jrmax<-uniform.max
  }
  jranks_mapped <- (jranks-jrmin)/(jrmax-jrmin)
  res<-ks.test(jranks_mapped,"punif")
  res$p.value
}
