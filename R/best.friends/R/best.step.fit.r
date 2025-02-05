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
#' \code{best.step.rank} is the rank value that makes the best step;
#' it is not obligatory one on the \code{ranks} value.\cr
#' \code{collections.on.left} is the vector of the collections on the left of the best step 
#' (including the step value). They are friends of the tag.\cr
#' \code{collections.on.right} is vector of those on the right \cr
#' \code{population.on.left} is how many ranks are on left of split; they are friends! \cr
#' @examples
#' example(tag.int.ranks)
#' steps<-step.fit.ln.likelihoods(TF.ranks[42,],genes.no)
#' @export
best.step.fit<-function(ranks,tags.no){
  step.models <- step.fit.ln.likelihoods(ranks,tags.no)
 
  possible.best.steps<-seq_len(tags.no-1)
  k1.by.l1<-step.models$k1.by.l1[possible.best.steps]
  possible.best.steps<-possible.best.steps[k1.by.l1>0 & k1.by.l1<length(ranks)]
  #we assess only the steps that have nonzero left and right sets
  
  #best.step.index<-which.max(step.models$ln.likelihoods[possible.best.steps])
  #almost; we want the last value, so:
  possible.likelihoods<-step.models$ln.likelihoods[possible.best.steps]
  ml<-max(possible.likelihoods)
  best.step.index<-max(which(possible.likelihoods==ml))

  best.step.rank<-possible.best.steps[best.step.index]
  
  population.on.left<-k1.by.l1[best.step.rank]

  collections.on.left<-
    step.models$collectons.order[seq_len(population.on.left)] 
  #1:population.on.left
  collections.on.right<-
    step.models$collectons.order[seq(population.on.left+1,length(ranks))] 
  #(population.on.left+1):k
  
  
  list(step.models=step.models,
       best.step.rank=best.step.rank,
       collections.on.left=collections.on.left,
       collections.on.right=collections.on.right,
       population.on.left=population.on.left
       )
}

