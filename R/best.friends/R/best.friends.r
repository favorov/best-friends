#best.friends package
#A. Suvorikova, V. Mukhina, V. Ramensky, A. Mironov, A. Favorov (c) 2014-2023
#'
#' best.friends: A package that describe whether a collection is a best friend (or one of the best friends) for a tag.
#'
#' We have a set C of collections (e.g. imagine a set of word/term/tag clouds, https://en.wikipedia.org/wiki/Tag_cloud)) and a set T of tags. Each tag can be related to each cloud, and the strength of the relation varies from one (tag,cloud) pair to another. We refer to the relation strength as the attention that a cloud pays to a tag. The attention that each cloud pays to each tag is represented by a real value. The attention actually can be any type of relation measure, e.g. fuzzy membership. The absence of the attention is supposed to be represented by the smallest value, naturally, it is 0 and all the attention values are are positive (not required). The attention values is is a \eqn{|T|x|C|} matrix \eqn{A}.
#'
#' The tag-collection-attention metaphor allows to represent a lot of applications in bioinformatics and statistics. The examples are gene patterns (cloud) and genes (tags) loads (attention) in the patterns; fuzzy sets (clouds, their elements (tags) and the inclusion degree (attention); weighted graph vertices (tags) and each vertex neighbourhood (cloud), here the attention is the weight of the edge. 
#'
#' Now, the question. For each tag, we want to identify the collection(s) that specifically prefer(s) the tag. We say that such a cloud is a friend (or the best friend if it is the only) for the tag. The simplest example: imagine that only one cloud pays attention to our tag.
#'
#' To identify the friends(s), first, for each collection, we rank all the tags by the attention the cloud pays to the tag. The ranking the decreasing, the first is the best. In other words, We create the rank matrix  of the same \eqn{|T|x|C|} size, and each element is the corresponding attention's rank inside inside the column of the attention matrix. We normalise the values to be in \eqn{[0,1]} by dividing by \eqn{|T|} and we refer to the normalised ranks as \eqn{r} matrix. Now, for each tag, we define the degree of friendliness of a cloud for this tag, by ranking the tag's row in \eqn{r_{i,j}=r(t_i,c_j)} matrix. The most friendly cloud \eqn{c_{(1)}(t_i)} is the cloud with the minimal value of \eqn{R_{ij}}, the next is \eqn{c_{(2)}(t_i)}, etc, etc.
#'
#' If a collection is best friend, it is to be the most friendly cloud for the tag, but it is not enough. In any ranking, there is a first element, and we want to estimate the probability to observe what we observe by random. The null-hypothesis we use to picture a random setup is that in any column of  all the elements are i.i.d., or, in other word, the attentions that a cloud pays to all the tags are independently sampled from the same distribution. The distributions can differ from cloud to cloud.
#'
#' The statistics we use to test whether the most friendly collection for the tag  is really the best friend is the difference \eqn{t} between the values \eqn{r(t_i,c_{(2)}(t_i))} and \eqn{r(t_i,c_{(1)}(t_i))}, in other words, between the next-after-the-best and the best values \eqn{r} for the tag \eqn{t_i}. We estimate the probability (p-value) to observe this difference as \eqn{<=t} given the null-hypothesis proposition. If p-value is small enough, we reject the null, and claim that the friendliness of the cloud \eqn{c_{(1)}(t_i)} is unlikely to observe by random and so we refer to it as the best friend of \eqn{t_i}. In this case, \eqn{t_i} is a marker of its best friend cloud \eqn{c_{(1)}(t_i)}.
#'
#' For a similar test that splits all the collections into \eqn{k} friends of the tag and the remaining \eqn{|C|-k} clouds uses the difference  \eqn{r(t_i,c_{(m+1)}(t_i))} and \eqn{r(t_i,c_{(m)}(t_i))}. If we obtain the p-value that is small enough, we claim that the clouds \eqn{c_{(1)}(t_i)}..\eqn{c_{(m)}(t_i)} are friends of \eqn{t_i} and \eqn{t_i} is their marker.
#'
#' See our arxiv paper for details.
#'
#' @section best.friends functions:
#' [best.friends.test] takes the attention matrix, and for each tag it finds the most friendly collection and the corresponding p-value. 
#'
#' [friends.test] does the same, but it considers more than one collections as potential friends for each tag, and p-values are generated for each pair of the tag and the possible  (number of friend clouds). If p-value is low and the null is rejected, the tag reliably separates the clouds: \eqn{m} most friendly to \eqn{t_i} clouds are real friends of the tag, others are not. The tag is the marker for all its friends.
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
