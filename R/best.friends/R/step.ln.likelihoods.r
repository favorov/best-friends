#' 
#' step.ln.likelihoods
#' 
#' fit possible bi-uniform step models for a set of ranks
#' See [best.friends] documentation for details.
#'
#' @param ranks vector of ranks of a tag in different collections
#' @param tags.no number of tags, i.e. maximal rank 
#' @returns list of two values: ln.likelihoods contains the ln of the likelihood of the ranks for each split (step) position into (this or less) and (greater than this) for \eqn{1 .. tags.no-1} the last (\eqn{tag.np)} element is for is for uniform, non-step case; $k1.by.l1 contains k_1 (number of values on the left of the step) for each split
#' @examples
#' example(tag.int.ranks)
#' steps<-step.ln.likelihoods(TF.ranks[42,],genes.no)
#' @export
step.ln.likelihoods<-function(ranks,tags.no){
  ln.likelihoods<-rep(0,tags.no)
  k1.by.l1<-rep(0,tags.no)
  k<-length(ranks)
  k1<-0
  for (l1 in 1:(tags.no-1)){
    print(paste(l1,k1,k))
    print(ranks[k1])
    while (ranks[k1+1]<=l1)
    {
      k1<-k1+1      
    }#l1 has hit next rank value
    k1.by.l1[l1]<-k1
    p1<-k1/k
    if(p1>0){
      ln.likelihoods[l1]<-
        ln.likelihoods[l1]+k1*log(p1/l1)
    }
    if(p1<1){
      ln.likelihoods[l1]<-
        ln.likelihoods[l1]+(k-k1*log((1-p1)/(tags.no-l1)))
    }
  }
  ln.likelihoods[tags.no]<-k*log(1/k)
  k1.by.l1[tags.no]<-k
  list(ln.likelihoods=ln.likelihoods,k1.by.l1=k1.by.l1)
}
