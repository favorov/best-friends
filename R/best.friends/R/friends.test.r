#'
#' friends.test
#' 
#' Takes the attention matrix, and for each tag considers more than one collections as potential friends for each tag, and p-values are generated for each pair of the tag and the possible  (number of friend clouds). If p-value is low and the null is rejected, the tag reliably separates the clouds: \eqn{m} most friendly to \eqn{t_i} clouds are real friends of the tag, others are not. The tag is the marker for all its friends.
#'
#' @inheritParams best.friends.test
#' @param friends.number number of entities we consider for each tags; the default -1 means all;
#' if friends.number is 1, the call does essentially the same as the best.friends.test call
#' @return a list with 2 matrices, first is \eqn{|T| x |C|} dimensions (as the \code{attention}), the second is \eqn{|T| x (|C|-1)}. 
#' if \code{friends.number} is some \eqn{F:1<=F<|C|-1}, the dimensions are \eqn{|T| x (F+1)} and \eqn{|T| x F}
#' \code{collections} for each tag, the the collections are ranked by the importance of the tag, best friend first; 
#' \code{pvals} contains p-values for the corresponding split of the \code{collections} row to friends and others.
#' @examples
#' genes<-10
#' regulation<-matrix(
#'     c(0.2, 0.2, 0.2, 0.2, 0.25, rep(0.2,genes-5),
#'         rep(1, genes),
#'         rep(1, genes),
#'         rep(1, genes),
#'         rep(1, genes),
#'         rep(1, genes),
#'         rep(1, genes),
#'         rep(1, genes),
#'         rep(1, genes),
#'         rep(1, genes)
#'      ),
#'     ncol=10,byrow=FALSE
#' )
#' gene.names<-LETTERS[seq( from = 1, to = genes )]
#' TF.names<-c('TF1','TF2','TF3','TF4','TF5','TF6','TF7','TF8','TF9','TF10')
#' rownames(regulation)<-gene.names
#' colnames(regulation)<-TF.names
#' friends<-friends.test(regulation)
#' @export
friends.test<-function(attention=NULL,ranks.of.tags=NULL,distance_like=FALSE,friends.number=-1,neglect_diagonal=FALSE){
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
  if (length(rownames(ranks.of.tags))==0) {rownames(ranks.of.tags)<-as.character(seq(dims[1]))} 
  if (length(colnames(ranks.of.tags))==0) {colnames(ranks.of.tags)<-as.character(seq(dims[2]))} 
  res<-list()
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
