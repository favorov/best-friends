#' 
#' best.step.fit
#' 
#' finds the ML-best two-step model
#' 
#' See [best.friends] documentation for details.
#'
#' @inheritParams step.fit.ln.likelihoods
#' @return a list of four values: \cr
#' \code{step.models} is return from [step.fit.ln.likelihoods] call the function start with
#' \code{best.step.rank} is the rank value that makes the best step\cr
#' \code{col.on.left} is the vector of the collections on the left of the best step 
#' (including the step value)\cr
#' \code{col.on.right} is vector of those on the right
#' \code{population.on.left} is how many ranks are on left of split; they are friends! 
#' @examples
#' example(tag.int.ranks)
#' steps<-step.ln.likelihoods(TF.ranks[42,],genes.no)
#' @export
best.step.fit<-function(ranks,tags.no){
  step.models <- step.fit.ln.likelihoods(ranks,tags.no)
 
  possible.best.steps<-seq_len(tags.no-1)
  k1.by.l1<-step.models$k1.by.l1[possible.best.steps]
  possible.best.steps<-possible.best.steps[k1.by.l1>0 & k1.by.l1<length(ranks)]
  #we assess only the steps that have nonzero left and right sets
  
  best.step.rank<-which.max(step.models$ln.likelihoods[possible.best.steps]) 
  
  population.on.left<-k1.by.l1[best.step.rank]

  col.on.left<-step.models$result$
    collectons.order[seq_len(population.on.left)] 
  #1:population.on.left
  col.on.right<-step.models$result$
    collectons.order[seq(population.on.left+1,length(ranks))] 
  #(population.on.left+1):k
  
  
  list(step.models=step.models,
       best.step.rank=best.step.rank,
       col.on.left=col.on.left,
       col.on.right=col.on.right,
       population.on.left=population.on.left)
}
