#best.friends package
#A. Favorov, A. Suvorikova, V. Mukhina, V. Ramensky, A. Mironov (c) 2014-2022
#'
#'
#' best.friends.test
#'
#'
#' We have a set C of clouds (e.g. imagine a set of word/term/tag clouds, https://en.wikipedia.org/wiki/Tag_cloud)) and a set T of tags. Each tag can be related to each cloud, and the strength of the relation varies from one (tag,cloud) pair to another. We refer to the relation strength as the attention that a cloud pays to a tag. The attention that each cloud pays to each tag is represented by a real value. The attention actually can be any type of relation measure, e.g. fuzzy membership. The absence of the attention is supposed to be represented by the smallest value, naturally, it is 0 and all the attention values are are positive (not required). The attention values is is a $|T|x|C|$ matrix $A$.
#'
#' The tag-cloud-attention metaphor allows to represent a lot of applications in bioinformatics and statistics. The examples are gene patterns (cloud) and genes (tags) loads (attention) in the patterns; fuzzy sets (clouds, their elements (tags) and the inclusion degree (attention); weighted graph vertices (tags) and each vertex neighbourhood (cloud), here the attention is the weight of the edge. 
#'
#' Now, the question. For each tag, we want to identify the cloud(s) that specifically prefer(s) the tag. We say that such a cloud is a friend (ot the best friend if it ia the only) for the tag. The simplest example: imagine that only one cloud pays attention to our tag. 
#'
#' To identify the friends(s), first, for each cloud, we rank all the tags by the attention the cloud pays to the tag. The ranking the decreasing, the first is the best. In other words, We create the rank matrix $R$ of the same $|T|x|C|$ size, and each element is the corresponding attention's rank inside inside the column of the attention matrix. We normalise the values to be in $[0,1]$ by dividing by $|T|$ and we refer to the normalised ranks as $r$ matrix. Now, for each tag, we define the degree of friendliness of a cloud for this tag, by ranking the tag's row in $r_{i,j}=r(t_i,c_j)$ matrix. The most friendly cloud $c_{(1)}(t_i)$ is the cloud with the minimal value of $R_{ij}$, the next is $c_{(2)}(t_i)$, etc, etc.    
#'
#' If a cloud is best friend, it is to be the most friendly cloud for the tag, but it is not enough. In any ranking, there is a first element, and we want to estimate the probability to observe what we observe by random. The null-hypothesis we use to picture a random setup is that in any column of $A$ all the elements are i.i.d., or, in other word, the attentions that a cloud pays to all the tags are indepentently sampled from the same distribution. The distributions can differ from cloud to cloud. 
#' The statistics we use to test whether the most friendly cloud for the tag $t_i$ is really the best friend is the difference $t$ between the values $r(t_i,c_{(2)}(t_i))$ and $r(t_i,c_{(1)}(t_i))$, in other words, between the next-after-the-best and the best values $r$ for the tag $t_i$. We estimate the probablity (p-value) to observe this difference as $<=t$ given the null-hypothesis proposition. If p-value is small enough, we reject the null, and claim that the friendliness of the cloud $c_{(1)}(t_i)$ is unlikely to observe by random and so we refer to it as the best friend of $t_i$. In this case, $t_i$ is a marker of its best friend cloud $c_{(1)}(t_i)$.
#'
#' @param attention is the tags*clouds matrix of the relations between tags and the clouds
#' @param distance_like the default is \code{FALSE} and it shows that the relation values are not like distance, 
#' i.e. the better relation is shown by the lagrer value; if the relation is, on the contrary, distance-like, 
#' and 0 is the best, the value is \code{TRUE}.
#' @param neglect_diagonal in the case of square attention matrix, the diagonal sometimes is either uninformative or it carries some specific values. In each of these cases, 
#' the diagonal elements are excluded from the ranking and from the statistics by setting this parameter TRUE. The default is FALSE. 
#' @return \code{data.frame} with 5 columns: tag index, 
#' the index of the cloud that is a putative best friend of the element, 
#' uncorrected p-value for the pair, 
#' tag name, 
#' friend name. 
#' The small (after multiple hypothesis correction we are to do) p-value 
#' indicates that the cloud is really the best friend of the tag.
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
#' bestfriends<-best.friends.test(regulation)
#' @export
best.friends.test<-function(attention,distance_like=FALSE,neglect_diagonal=FALSE){
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
  element.ranks<-apply(attention,2,
		function(x){
			data.table::frankv(x,ties.method='average',
			na.last=TRUE,order=order)
		}
  )
	element.ranks<-(element.ranks-1)/(dims[1])
	#we applied ranking column-by-column (entity-by-entity); A's were ranked in each row,
	res<-t(apply(element.ranks,1,rank_diff_and_p_for_the_best))
	rn<-rownames(attention); if (length(rn)==0) {as.character(seq(dims[1]))} 
	cn<-colnames(attention); if (length(cn)==0) {as.character(seq(dims[2]))} 
	data.frame(
		tag=seq(dims[1]),
		friend=as.integer(res[,1]),
	  p.value=res[,2],
	  tag.name=rn,
	  friend.name=cn[as.integer(res[,1])])
}

#'
#' friends.test
#'
#' We have a set C of clouds (e.g. imagine a set of word/term/tag clouds, https://en.wikipedia.org/wiki/Tag_cloud)) and a set T of tags. Each tag can be related to each cloud, and the strength of the attention varies from one (tag,cloud) pair to another. We refer to the attention strength as the attention that a cloud pays to a tag. The attention that each cloud pays to each tag is represented by a real value. The attention actually can be any type of attention measure, e.g. fuzzy membership. The absence of the attention is supposed to be represented by the smallest value, naturally, it is 0 and all the attention values are are positive (not required). The attention values is is a $|T|x|C|$ matrix $A$.
#'
#' The tag-cloud-attention metaphor allows to represent a lot of applications in bioinformatics and statistics. The examples are gene patterns (cloud) and genes (tags) loads (attention) in the patterns; fuzzy sets (clouds, their elements (tags) and the inclusion degree (attention); weighted graph vertices (tags) and each vertex neibourhood (cloud), here the attention is the weight of the edge. 
#'
#' Now, the question. For each tag, we want to identify the cloud(s) that specifically prefer(s) the tag. We say that such a cloud is a friend (or the best friend if it is the only) for the tag. The simplest example: imagine that only one cloud pays attention to our tag. 
#'
#' To identify the friends(s), first, for each cloud, we rank all the tags by the attention the cloud pays to the tag. The ranking the decreasing, the first is the best. In other words, We create the rank matrix $R$ of the same $|T|x|C|$ size, and each element is the corresponding attention's rank inside inside the column of the attention matrix. We normalise the values to be in $[0,1]$ by dividing by $|T|$ and we refer to the normalised ranks as $r$ matrix. Now, for each tag, we define the degree of friendliness of a cloud for this tag, by ranking the tag's row in $r_{i,j}=r(t_i,c_j)$ matrix. The most friendly cloud $c_{(1)}(t_i)$ is the cloud with the minimal value of $R_{ij}$, the next is $c_{(2)}(t_i)$, etc, etc.    
#'
#' If a cloud is best friend, it is to be the most friendly cloud for the tag, but it is not enough. In any ranking, there is a first element, and we want to estimate the probability to observe what we observe by random. The null-hypothesis we use to picture a random setup is that in any column of $A$ all the elements are i.i.d., or, in other word, the attentions that a cloud pays to all the tags are independently sampled from the same distribution. The distributions can differ from cloud to cloud. 
#' The statistics we use to test whether the most friendly cloud for the tag $t_i$ is really the best friend is the difference $t$ between the values $r(t_i,c_{(2)}(t_i))$ and $r(t_i,c_{(1)}(t_i))$, in other words, between the next-after-the-best and the best values $r$ for the tag $t_i$. We estimate the probability (p-value) to observe this difference as $<=t$ given the null-hypothesis proposition. If p-value is small enough, we reject the null, and claim that the friendliness of the cloud $c_{(1)}(t_i)$ is unlikely to observe by random and so we refer to it as the best friend of $t_i$. In this case, $t_i$ is a marker of its best friend cloud $c_{(1)}(t_i)$.
#'
#' For a similar test that splits all the clouds into $m$ friends of the tag and the remaining $|C|-k$ clouds uses the difference  $r(t_i,c_{(m+1)}(t_i))$ and $r(t_i,c_{(m)}(t_i))$. If we obtain the p-value that is small enough, we claim that the clouds $c_{(1)}(t_i)$..$c_{(m)}(t_i)$ are friends of $t_i$ and $t_i$ is their marker.
#'
#' We have what can to be friends (clouds) as columns and we have what they can be friends of (tags) as rows.
#` matrix where in each column and row there is a value of how strong the raw is related to this column.
#' @inheritParams best.friends.test
#' @param friends.number number of entities we consider for each tags; the default -1 means all;
#' if friends.number is 1, the call does essentially the same as the best.friends.test call
#' @return a list with 4 elements, each is a matrix with the same dimetions as the \code{attention}. 
#' \code{tag.ranks} are the ranks of attention-to-tags inside the clouds; 
#' \code{friends} is the ranked-by-friendship-to-the-tag list of friendly clouds, best friend first; 
#' \code{pvals} contains p-values for the corresponding split of the \code{friends} row to friends and others.
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
#' friends<-friends.test(regulation)
#' @export
friends.test<-function(attention,distance_like=FALSE,friends.number=-1){
  dims<-dim(attention)
	if(min(dims)<2){
		stop("best.friends.test requires both dimetions of the attention matrix to be more than 1")
	}
	if(-1==friends.number){friends.number=dims[2]}
	#default number of friends
  order<-ifelse(distance_like,1,-1)
  #if attention is distance_like, we will order in ascending
  #if nor, descending. 
  #E.g., the least ranks are the 
  #most close attentions
  # if distance_like holds, the least is the best (first)
  #and order==1 (ascending) 
  element.ranks<-apply(attention,2, 
    function(x){
      data.table::frankv(x,ties.method='average',
      na.last=TRUE,order=order)
    }
  )
	rownames(element.ranks)<-rownames(attention)
  res<-list()
  res$element.ranks<-element.ranks
	element.ranks<-(element.ranks-1)/(dims[1])
	#element.ranks<-element.ranks/dims[1]
  #we applied ranking column-by-column (community-by-community); A's were ranked in each row,
  unlistres<-unlist(t(apply(element.ranks,1,rank_diff_and_p_for_the_best_n,n=friends.number)))
	res$friends<-matrix(
	  colnames(attention)[unlistres[seq(1,length(unlistres),2)]],ncol = friends.number, nrow=dims[1], byrow = TRUE
  )
	res$pvals<-matrix(unlistres[seq(2,length(unlistres),2)],ncol = friends.number, nrow=dims[1],byrow = TRUE)
	rownames(res$friends)<-rownames(attention)
	rownames(res$pvals)<-rownames(attention)
	res
}
