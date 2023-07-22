#A. Suvorikova, V. Mukhina, V. Ramensky, A. Mironov, A. Favorov (c) 2014-2023
#'
#' tag.ranks
#'
#'
#' We have a set C of collections (e.g. imagine a set of word/term/tag clouds, https://en.wikipedia.org/wiki/Tag_cloud)) and a set T of tags. Each tag can be related to each cloud, and the strength of the relation varies from one (tag,cloud) pair to another. We refer to the relation strength as the attention that a cloud pays to a tag. The attention that each cloud pays to each tag is represented by a real value. The attention actually can be any type of relation measure, e.g. fuzzy membership. The absence of the attention is supposed to be represented by the smallest value, naturally, it is 0 and all the attention values are are positive (not required). The attention values is is a \eqn{|T|x|C|} matrix \eqn{A}.
#'
#' The tag-collection-attention metaphor allows to represent a lot of applications in bioinformatics and statistics. The examples are gene patterns (cloud) and genes (tags) loads (attention) in the patterns; fuzzy sets (clouds, their elements (tags) and the inclusion degree (attention); weighted graph vertices (tags) and each vertex neighbourhood (cloud), here the attention is the weight of the edge. 
#'
#' Now, the question. For each tag, we want to identify the collection(s) that specifically prefer(s) the tag. We say that such a cloud is a friend (ot the best friend if it ia the only) for the tag. The simplest example: imagine that only one cloud pays attention to our tag. 
#'
#' To identify the friends(s), first, for each collection, we rank all the tags by the attention the cloud pays to the tag. The ranking the decreasing, the first is the best. In other words, We create the rank matrix \eqn{R} of the same \eqn{|T|x|C|} size, and each tag is the corresponding attention's rank inside inside the column of the attention matrix. We normalise the values to be in \eqn{[0,1]} by dividing by \eqn{|T|} and we refer to the normalised ranks as \eqn{r} matrix. Now, for each tag, we define the degree of friendliness of a cloud for this tag, by ranking the tag's row in \eqn{r_{i,j}=r(t_i,c_j)} matrix. The most friendly cloud \eqn{c_{(1)}(t_i)} is the cloud with the minimal value of \eqn{R_{ij}}, the next is \eqn{c_{(2)}(t_i)}, etc, etc.    
#'
#' If a collection is best friend, it is to be the most friendly cloud for the tag, but it is not enough. In any ranking, there is a first tag, and we want to estimate the probability to observe what we observe by random. The null-hypothesis we use to picture a random setup is that in any column of \eqn{A} all the tags are i.i.d., or, in other word, the attentions that a cloud pays to all the tags are indepentently sampled from the same distribution. The distributions can differ from cloud to cloud. 
#' The statistics we use to test whether the most friendly collection for the tag \eqn{t_i} is really the best friend is the difference \eqn{t} between the values \eqn{r(t_i,c_{(2)}(t_i))} and \eqn{r(t_i,c_{(1)}(t_i))}, in other words, between the next-after-the-best and the best values \eqn{r} for the tag \eqn{t_i}. We estimate the probablity (p-value) to observe this difference as \eqn{<=t} given the null-hypothesis proposition. If p-value is small enough, we reject the null, and claim that the friendliness of the cloud \eqn{c_{(1)}(t_i)} is unlikely to observe by random and so we refer to it as the best friend of \eqn{t_i}. In this case, \eqn{t_i} is a marker of its best friend cloud \eqn{c_{(1)}(t_i)}.
#'
#' This function makes common part of [best.friends.test] and [friends.test]: it calculates the ranks of tags inside collections.
#' 
#' @param attention is the tags*collections matrix of the relations between tags and the clouds
#' @param distance_like the default is \code{FALSE} and it shows that the relation values are not like distance, 
#' i.e. the better relation is shown by the lagrer value; if the relation is, on the contrary, distance-like, 
#' and 0 is the best, the value is \code{TRUE}.
#' @param neglect_diagonal in the case of square attention matrix, the diagonal sometimes is either uninformative or it carries some specific values. In each of these cases, 
#' the diagonal elements are excluded from the ranking and from the statistics by setting this parameter TRUE. The default is FALSE. 
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
    if (neglect_diagonal){diag(tag.ranks)<-NA}
    tag.ranks
}
