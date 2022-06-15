#' best.friends: A package that describe wheteher a cloud is a best friend (or one of the best friends) for a tag. 
#'
#' We have a set of clouds and a set of tags and the attemtion that cloud pays to each tag is represented by a real value. THe attention actually can be any type of relation measure, e.g. fuzzy membership.
#' The absence of a relation is supposed to be represented by the smallest value, naturally, it is 0 and all the attentionvalues are are positive (not required). 
#' The attention values is a TxC matrix.
#' 
#' We want to identify pairs of a tag and a cloud that specifically prefers the tag.
#' We say that the cloud is friendly to the tag. The simplest: suppose that only one cloud pays attention to our tag. 
#' The tag is a marker for the cloud (if we see the tag, the cloud is around) and the cloud is friendly to the tag (all others do not like him).
#' We compare the degrees of friendship by ranking the ranks of attention -- first ranking is done inside the cloud - related column if the attention matrix -- and the second is inside the tag's row. 
#' We say that the cloud is a friend (or best friend) of a tag if it more friendly to the tag more than other clouds do and the difference is unlikely to be at random.
#'
#' @section best.friends functions:
#' [best.friends.test] takes the tags x clouds attention matrix, and for each tag provides the cloud that is the potential best friend of the tag (actually, the most friendly cloud) and the corresponding p-value. 
#' The p-value is the null hypothesis that for each cloud, the attention levels for all tags are i.i.d. values (the distrubution can be differennt for different tags). The statistics we use is the difference of friendness between the most friendly cloud and the next. If p-value is low and the null is rejected, the cloud is the best friend of the tag and the tag is a marker for the cloud.
#'
#'
#' [friends.test] does the same, but it considers n (possibly, all) the clouds as potential friends for each tag, and p-values are generated for each tag+cloud pair. The p-values tests the null hyposthesis that claims that the differnce of the cloud's rank of attention to the tag and in the next-by-attention-rank-for-the-tag is by random. If p-value is low and the null is rejected, the tag reliably separates the clouds, all freindly (e.g. this and more friendly) clouds are tag's friends, others are not. The tag is maker for all its friends.
#'
#' @docType package
#' @name best.friends
#' @importFrom utils packageDescription
#' @importFrom data.table frankv
#' @useDynLib best.friends, .registration = TRUE
#' @importFrom Rcpp evalCpp
# these two are Rcpp - specific ivocations
#' @examples
#' genes<-10
#' regulation=matrix(
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
#' bfriends<-best.friends.test(regulation)
#' friends<-friends.test(regulation)
NULL
