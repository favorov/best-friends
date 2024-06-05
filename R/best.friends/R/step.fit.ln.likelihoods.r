#' 
#' step.fit.ln.likelihoods
#' 
#' fit possible bi-uniform step models for a set of ranks of the same tag in different collections. 
#' The input ranks are integers in \eqn {1..tags.no}. 
#' The function ranks the collections (columns) by 
#' the rank of the tag (the \code{ranks} parameter)

#' See [best.friends] documentation for details.
#'
#' @param ranks vector of ranks of a tag in different collections
#' @param tags.no number of tags, i.e. maximal rank 
#' @return a list of three values: \cr
#' \code{collectons.order} is the order of ranks in, collection-by-collection\cr
#' \code{ln.likelihoods} the ln of the likelihood of the model соrresponding to each split rank value in \eqn{1 .. tags.no}
#' \code{k1.by.l1} contains \eqn{k_1} (number of values on the left of the step) for each splitting value of the ranks in \eqn {1..tags.no};\cr
#' Each of the split rank values \eqn{1 .. tags.no-1} split all the ranks into two steps: "this or less" and "greater than this" 
#' and the ln-likelihood is for the fit of the two-step model.
#' The last (\eqn{tag.no)} ln-likelihood is for a uniform, non-step model;\cr
#' @examples
#' example(tag.int.ranks)
#' steps<-step.ln.likelihoods(TF.ranks[42,],genes.no)
#' @export
step.ln.likelihoods<-function(ranks,tags.no){
  col.order<-order(ranks)
  ranks<-ranks[col.order]
  ln.likelihoods<-rep(0,tags.no)
  k1.by.l1<-rep(0,tags.no)
  k<-length(ranks)
  k1<-0
  #l1==tags_no is "no step"
  for (l1 in seq_len(tags.no-1)){ #1:(tags.no-1)
    while (k1<k && ranks[k1+1]<=l1)
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
        ln.likelihoods[l1]+(k-k1)*log((1-p1)/(tags.no-l1))
    }
  }
  ln.likelihoods[tags.no]<-k*log(1/k)
  k1.by.l1[tags.no]<-k
  
  best.step.rank<-which.max(ln.likelihoods[seq_len(tags.no-1)]) 
  #1:tags.no-1
  
  #maybe it is an alien plug, maybe not,
  #still we need it now
  if (k1.by.l1[best.step.rank]==k && 
      best.step.rank>1 &&
      k1.by.l1[best.step.rank-1]>0) 
  {
    best.step.rank<-best.step.rank-1
  } 
  
  population.on.left<-k1.by.l1[best.step.rank]
  #alternative plug
  #if(population.on.left==tags.no){
  #  population.on.left<-population.on.left-1
  #}
  
  col.on.left<-col.order[seq_len(population.on.left)] 
  #1:population.on.left
  col.on.right<-col.order[seq(population.on.left+1,k)] 
  #(population.on.left+1):k
  
  
  list(ln.likelihoods=ln.likelihoods,
       k1.by.l1=k1.by.l1,
       col.order=col.order,
       best.step.rank=best.step.rank,
       col.on.left=col.on.left,
       col.on.right=col.on.right,
       population.on.left=population.on.left)
}
