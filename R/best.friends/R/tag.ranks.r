#A. Suvorikova, V. Mukhina, V. Ramensky, A. Mironov, A. Favorov (c) 2014-2023
#'
#' tag.ranks
#'
#' @param attention is the \eqn{|T| \times |C|} matrix of the relations between tags and the clouds.
#' @param distance_like the default is \code{FALSE} and it shows that the relation values are 
#' not like distance, i.e. the better relation is shown by the larger value; 
#' if the relation is, on the contrary, distance-like, and 0 is the best, the value is \code{TRUE}.
#' @param neglect_diagonal in the case of square attention matrix, the diagonal sometimes is either 
#' uninformative or it carries some specific values. In each of these cases, 
#' the diagonal elements are excluded from the ranking and from the statistics 
#' by setting this parameter TRUE. The default is FALSE. 
#' @return The matrix of ranks of attention to tags from collections. The size is the same as that of the attention matrix.
#' collection. 
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
#' ranks<-tag.ranks(regulation)
#' bestfriends<-best.friends.test(regulation)
#' @export
tag.ranks<-function(attention,distance_like=FALSE,neglect_diagonal=FALSE){
    dims<-dim(attention)
    if(min(dims)<2){
        stop("tag.ranks requires both dimensions of the attention matrix to be more than 1")
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
    tag.ranks<-apply(attention,2,
                         function(x){
                             data.table::frankv(x,ties.method='average',
                                                na.last=TRUE,order=order)
                         }
    )
    #we applied ranking column-by-column (collection-by-cloud)
    rownames(tag.ranks)<-rownames(attention)
    colnames(tag.ranks)<-colnames(attention)
    if (neglect_diagonal){diag(tag.ranks)<-NA}
    tag.ranks
}
