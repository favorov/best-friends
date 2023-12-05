#'
#' tag.unif.ks.test
#' 
#' Given the attention matrix, for each tag test whther its ranks in different collections are distributed uniformely.
#' 
#' If p-value is low and the null is rejected, we will find whether the tag separated the clouds by [tag.split.collections]: 
#' 
#' #' See [best.friends] documentation for details.
#'
#' @return a list with 2 matrices, first is \eqn{|T| x |C|} dimensions (as the \code{attention}), the second is \eqn{|T| \times (|C|-1)}. 
#' if \code{friends.number} is some \eqn{F:1<=F<|C|-1}, the dimensions are \eqn{|T| \times (F+1)} and \eqn{|T| \times F}
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
#' tag.unif.ks<-friends.test(regulation)
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
  