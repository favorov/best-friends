#best.friends package
#A. Suvorikova, V. Mukhina, V. Ramensky, A. Mironov, A. Favorov (c) 2014-2023
#'
#' best.friends: A package that describe whether a collection is a best friend (or one of the friends) 
#' for a tag or bot.
#'
#' We have a set T of tags a set C of collections. Clouds pay attention to tags.
#' The attention that each cloud pays to each tag is represented by a real value. 
#' The attention  can represent any type of relation measure, e.g. fuzzy membership. 
#' The attention values is a \eqn{|T| \times |C|} matrix \eqn{A}.
#'
#' For each tag, we want to identify the collection(s) that specifically prefer(s) the tag. 
#' We say that such a cloud is a friend (or the best friend if it is the only) for the tag. 
#' The simplest example: imagine that only one cloud pays attention to our tag.
#'
#' See our arxiv paper for details.
#'
#' @section best.friends functions:
#' [best.friends.test] given the attention matrix for each tag, identifies the most friendly collection 
#' and assesses the corresponding p-value.
#'
#' [friends.test] Given the attention matrix for each tag and each possible slit of the collections into 
#' set of the friends for the tag and the remainder set, assess the p-values for the split.
#'
#' [tag.ranks] is a common part of both of them, is makes the ranking of tags inside collections. It can be called separately is the calling program supposes to call the friend methods more than once.
#' 
#' @docType package
#' @name best.friends
#' @importFrom utils packageDescription
#' @importFrom data.table frankv
#' @useDynLib best.friends, .registration = TRUE
#' @importFrom Rcpp evalCpp
# these two are Rcpp - specific i invocations
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
