#A. Suvorikova, V. Mukhina, V. Ramensky, A. Mironov, A. Favorov (c) 2014-2023
#'
#' best.friends.test
#'
#' @param attention is the tags*collections matrix of the relations between tags and the clouds
#' @param ranks.of.tags the value of [tag.ranks] call. \code{best.friends.test(attenion)} and \code{best.friends.test(ranks.of.tags=tag.ranks(attenion)} return the same, as well as \code{friends.test(attenion)} and \code{friends.test(tag.ranks=tag.ranks(attenion)} The goal is to calculate tag.ranks only once if multiple calls of friends tests happen for the same attention matrix.
#' @param distance_like the default is \code{FALSE} and it shows that the relation values are not like distance, 
#' i.e. the better relation is shown by the lagrer value; if the relation is, on the contrary, distance-like, 
#' and 0 is the best, the value is \code{TRUE}.
#' @param neglect_diagonal in the case of square attention matrix, the diagonal sometimes is either uninformative or it carries some specific values. In each of these cases, 
#' the diagonal elements are excluded from the ranking and from the statistics by setting this parameter TRUE. The default is FALSE. 
#' @return \code{data.frame} with 5 columns: tag index, 
#' the index of the collection that is a putative best friend of the tag, 
#' uncorrected p-value for the pair, 
#' tag, 
#' collection. 
#' The small (after multiple hypothesis correction we are to do) p-value 
#' indicates that the collection is really the best friend of the tag.
#' @examples
#' genes<-10
#' regulation<-matrix(
#'   c(0.2, 0.2, 0.2, 0.2, 0.25, rep(0.2,genes-5),
#'     rep(1, genes),
#'     rep(1, genes),
#'     rep(1, genes),
#'     rep(1, genes),
#'     rep(1, genes),
#'     rep(1, genes),
#'     rep(1, genes),
#'     rep(1, genes),
#'     rep(1, genes)
#'   ),
#'   ncol=10,byrow=FALSE
#' )
#' gene.names<-LETTERS[seq( from = 1, to = genes )]
#' TF.names<-c('TF1','TF2','TF3','TF4','TF5','TF6','TF7','TF8','TF9','TF10')
#' rownames(regulation)<-gene.names
#' colnames(regulation)<-TF.names
#' bestfriends<-best.friends.test(regulation)
#' @export
best.friends.test<-function(attention=NULL,ranks.of.tags=NULL,distance_like=FALSE,neglect_diagonal=FALSE){
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
    #if attention is distance_like, we will order in ascending
    #if nor, descending. 
    #E.g., the least ranks are the 
    #most close attentions
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
    # if distance_like holds, the least is the best (first)
    #and order==1 (ascending) 
    ranks.of.tags<-apply(attention,2,
                         function(x){
                           data.table::frankv(x,ties.method='average',
                                              na.last=TRUE,order=order)
                         }
    )
    rownames(ranks.of.tags)<-rownames(attention)
    colnames(ranks.of.tags)<-colnames(attention)
  }
  #we applied ranking column-by-column (collection-by-cloud)
  ranks.of.tags<<-(ranks.of.tags-.5)/(dims[1]-as.integer(neglect_diagonal))
  #and mapped the ranks into [0..#tags] (or [0..#tags] is neglect_diagonal)
  if (neglect_diagonal){diag(ranks.of.tags)<-NA}
  #we reapply NA to the diagonal -- it will be used not to see at in the C++ u statistics calculation
  #it also signals C++ that there are |C|-1 values rather that |C|
  #there a no other source on NA's in tag.ranks
  res<-t(apply(ranks.of.tags,1,rank_diff_and_p_for_the_best))
  #here, rownames(ranks.of.tags) are the tag names, either by attention parsing of by parameter
  #here, colnames(ranks.of.tags) are the collection names, either by attention parsing of by parameter
  #we write something not to have them empty whatever
  rn<-rownames(ranks.of.tags); if (length(rn)==0) {rn<-as.character(seq(dims[1]))} 
  cn<-colnames(ranks.of.tags); if (length(cn)==0) {cn<-as.character(seq(dims[2]))} 
  data.frame(
    tag.index=seq(dims[1]),
    collection.index=as.integer(res[,1]),
    p.value=res[,2],
    tag=rn,
    collection=cn[as.integer(res[,1])]
  )
}
