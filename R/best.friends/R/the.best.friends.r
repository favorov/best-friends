#best.friends library
#A. Favorov, V. Mukhina, V. Ramensky, A. Mironov (c) 2014-2021
#'
#'
#' best.friends.test
#'
#' We have what can to be friends (communities) as columns and we have what they can befriends of (features) as rows.
#` matrix where in each column and row there is a value of how strong the raw is related to this column.
#' @param relation is the elements*communities matrix of the relations between features and the entities
#' @param distance_like the default is \code{FALSE} and it shows that the relation values are not like distance, 
#' i.e. the better relation is shown by the lagrer value; if the relation is, on the contrary, distance-like, 
#' and 0 is the best, the value is \code{TRUE}.
#' @return \code{data.frame} with 5 columns: element index, 
#' the index of the community that is a putative best friend of the element, 
#' uncorrected p-value for the pair, 
#' element name, 
#' friend name. 
#' The small (after multiple hypothesis correction we are to do) p-value 
#' indicates that the community is really the best friend of the element.
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
best.friends.test<-function(relation,distance_like=FALSE){
  dims<-dim(relation)
	if(min(dims)<2){
		stop("best.friends.test requires both dimetions of the relation matrix to be more than 1")
	}
  #if relation is distance_like, we will order in ascending
  #if nor, descending. 
  #E.g., the least ranks are the 
  #most close relations 
  order<-ifelse(distance_like,1,-1)
  # if distance_like holds, the least is the best (first)
  #and order==1 (ascending) 
  element.ranks<-apply(relation,2,
		function(x){
			data.table::frankv(x,ties.method='average',
			na.last=TRUE,order=order)
		}
  )
	element.ranks<-(element.ranks-1)/(dims[1])
	#we applied ranking column-by-column (entity-by-entity); A's were ranked in each row,
	res<-t(apply(element.ranks,1,rank_diff_and_p_for_the_best))
	rn<-rownames(relation); if (length(rn)==0) {as.character(seq(dims[1]))} 
	cn<-colnames(relation); if (length(cn)==0) {as.character(seq(dims[2]))} 
	data.frame(
		element=seq(dims[1]),
		friend=as.integer(res[,1]),
	  p.value=res[,2],
	  element.name=rn,
	  friend.name=cn[as.integer(res[,1])])
}

#'
#' friends.test
#'
#' We have what can to be friends (communities) as columns and we have what they can befriends of (features) as rows.
#` matrix where in each column and row there is a value of how strong the raw is related to this column.
#' @inheritParams best.friends.test
#' @param friends.number number of entities we consider for each feature; the default -1 means all;
#' if friends.number is 1, the call does essentially the same as the best.friends.test call
#' @return a list with 4 elements, each is a matrix with the same dimetions as the \code{relation}. 
#' \code{element.ranks} are the ranks of elemants in the communities; 
#' \code{friends} is the ranked-by-friendship-to-the-element list of friendly communities, best friend first; 
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
friends.test<-function(relation,distance_like=FALSE,friends.number=-1){
  dims<-dim(relation)
	if(min(dims)<2){
		stop("best.friends.test requires both dimetions of the relation matrix to be more than 1")
	}
	if(-1==friends.number){friends.number=dims[2]}
	#default number of friends
  order<-ifelse(distance_like,1,-1)
  #if relation is distance_like, we will order in ascending
  #if nor, descending. 
  #E.g., the least ranks are the 
  #most close relations
  # if distance_like holds, the least is the best (first)
  #and order==1 (ascending) 
  element.ranks<-apply(relation,2, 
    function(x){
      data.table::frankv(x,ties.method='average',
      na.last=TRUE,order=order)
    }
  )
	rownames(element.ranks)<-rownames(relation)
  res<-list()
  res$element.ranks<-element.ranks
	element.ranks<-(element.ranks-1)/(dims[1])
	#element.ranks<-element.ranks/dims[1]
  #we applied ranking column-by-column (community-by-community); A's were ranked in each row,
  unlistres<-unlist(t(apply(element.ranks,1,rank_diff_and_p_for_the_best_n,n=friends.number)))
	res$friends<-matrix(
	  colnames(relation)[unlistres[seq(1,length(unlistres),2)]],ncol = friends.number, nrow=dims[1], byrow = TRUE
  )
	res$pvals<-matrix(unlistres[seq(2,length(unlistres),2)],ncol = friends.number, nrow=dims[1],byrow = TRUE)
	rownames(res$friends)<-rownames(relation)
	rownames(res$pvals)<-rownames(relation)
	res
}
