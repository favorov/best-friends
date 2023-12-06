#'
#' matrix.to.ranks
#' 
#' Given the (\code{attention}) \eqn{|T| \times |C|} matrix, convert it to matrix of descending ranks of tags(rows) in collections (columns). 
#' #' See [best.friends] documentation for details.
#' @param attention original attention matrix
#' @param distance_like if \code{TRUE}, ranks are ascending (the smaller is the better). Default is \code{FALSE}.
#' @param neglect_diagonal if square matrix, and the diagonal does not make sense, we \code{NA} it and our ranks are in \eqn{[1 .. |T|-1]}. Default is \code{FALSE}.
#' @return a \eqn{|T| x |C|} matrix of integer ranks in \eqn{[1 .. |T|]} (or, \eqn{[1 .. |T|-1]} if neglect_diagonal).
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
#' TF.ranks<-matrix.to.ranks(regulation)
#' @export
matrix.to.rankst<-function(
    attention=NULL,
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
  