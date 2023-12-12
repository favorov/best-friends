#' fit possible bi-uniform step models for a set of ranks
#' #' See [best.friends] documentation for details.
#'
#' @param ranks vector of ranks of a tag in different collections
#' @param tags.no number of tags, i.e. maximal rank 
#' @returns list of two values: ln.likelihoods contains the ln of the likelihood of the ranks for each split (step) position into (this or less) and (greater than this) for \eqn{1 .. tags.no-1} the last (\eqn{tag.np)} element is for is for uniform, non-step case; $k_1 contains k_1 (number of values on the left of the step) for each split
#' @example
#' example(tag.int.ranks)
#' steps<-step.ln.likelihoods(TF.ranks[42,],genes.no)
#' @export
step.ln.likelihoods<-function(ranks,tags.no){
  lnlikls<-rep(0,tags.no)
  k1.by.l1<-rep(0,tags.no)
  k1<-0
  for (l1 in 1:tags.no){
    if(ranks[k1+1]>=l1)
    {
      k1<=k1+1      
    }#l1 has hit next rank value
    
  }
  lnlikls
}
