#'
#' tag.unif.ks.test
#' 
#' returns p-value for unifirmity of the ranks (they are in \eqn{1 .. N}) vector
#' #' See [best.friends] documentation for details.
#' 
#' @param ranks vector of ranks of a tag in different collections
#' @return p-value for the KS test comparing the ranks distribution with uniform
#'
#' @examples
#' example(matrix.to.ranks)
#' apply
#' @export
tag.unif.ks.test<-function(
    attention=NULL,
    tag.to.test=NULL,
    ranks.of.tags=NULL,
    distance_like=FALSE,
    neglect_diagonal=FALSE){
  if (! is.null(ranks.of.tags)) {
    if(!is.null(attention)){
      warning("ranks.of.tags is given, the attention matrix is omited")
    }
    dims<-dim(ranks.of.tags)
  } else {
    dims<-dim(attention)
    if(min(dims)<2){
      stop("best.friends.test requires both dimetions of the attention matrix to be more than 1")
    }
    if (neglect_diagonal){ 
      if(dims[1]==dims[2]) {
        diag(attention)<-NA
      } 
      else {
        warning("neglect_diagonal can work only for square attention matrix")
        neglect_diagonal<-FALSE
      }
    }

    order<-ifelse(distance_like,1,-1)
    #if attention is distance_like, we will order in ascending
    #if nor, descending. 
    #E.g., the least ranks are the 
    #most close attentions
    # if distance_like holds, the least is the best (first)
    #and order==1 (ascending) 
    ranks.of.tags<-apply(attention,2, 
                     function(x){
                       data.table::frankv(x,ties.method='average',
                                          na.last=TRUE,order=order)
                     }
    )
    #we applied ranking column-by-column (collection-by-cloud)
    rownames(ranks.of.tags)<-rownames(attention)
    colnames(ranks.of.tags)<-colnames(attention)
  }
  #we reapply NA to the diagonal -- it will be used not to see at in the C++ u statistics calculation
  #it also signals C++ that there are |C|-1 values rather that |C|
  #there a no other source on NA's in ranks.of.tags
  if (neglect_diagonal){diag(ranks.of.tags)<-NA}
  #here, rownames(ranks.of.tags) are the tag names, either by attention parsing of by parameter
  #here, colnames(ranks.of.tags) are the collection names, either by attention parsing of by parameter
  #we write something not to have them empty whatever

  #we start to count somethig here
  #the denominator is the range of ranks, so it is 1 larger than the number of possible friends
  ranks.of.tags<-(ranks.of.tags-.5)/(dims[1]-as.integer(neglect_diagonal))
  #there are |C|-1 maximal (default) number friends for general case
  #default number of friends; if we neglect diagonal, it decreases by 1
  default.friends.number <- dims[2]-1-as.integer(neglect_diagonal)
  if(friends.number<=0 || friends.number > default.friends.number){
    friends.number <- default.friends.number
  } 
  #the result "unlistres" contains collection names in even positions and p-values in odd
  unlistres<-
    unlist(t(apply(ranks.of.tags,1,rank_diff_and_p_for_the_best_n,max_num_friends=friends.number)))

  res$collections<-matrix(
    colnames(ranks.of.tags)[
      unlistres[seq(1,length(unlistres),2)]
    ],
    ncol = friends.number+1, nrow=dims[1], byrow = TRUE
  )
  #we show one more collection than the friends.number is to know what we separate from
  res$pvals<-matrix(
    unlistres[seq(2,length(unlistres),2)],
    ncol = friends.number+1, nrow=dims[1],byrow = TRUE
  )[,1:friends.number]
  rownames(res$collections)<-rownames(ranks.of.tags)
  rownames(res$pvals)<-rownames(ranks.of.tags)
  res
}
