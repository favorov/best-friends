#best-friends-of library
#A. Favorov, V. Ramensky, A. Mironov (c) 2014-2021
#'
#'
#' best.friends.of.features
#'
#' We have someones to be a friend (entity) as columns and we have what is to be friends of (features) as rows.
#` matrix where in each column and row there is a value of how the raw specifically friendly to this column.
#' @param relation is the fetures*entities matrix of the relations between features and the entities
#' @param distance_like the default is \code{FALSE} and it shows that the relation values are not like distance, i.e. the better relation is shown by the lagrer value; is the relation is, on the contrary, distance-like, and 0 is the best, the value is \code{TRUE}.
#' @return \code{data.frame} with 5 columns: feature index, friend entity index, uncorrected p-value for the pair, feature name, friend name 
#' Best friend has the highest order, the worst has the lowest
#' @export
best.friends.of.features<-function(relation,distance_like=FALSE){
  dims<-dim(relation)
  #if relation is distance_like, we will order in ascending
  #if nor, descending. 
  #E.g., the least ranks are the 
  #most close relations 
  order<-ifelse(distance_like,1,-1)
  # if distance_like holds, the least is the best (first)
  #and order==1 (ascending) 
  feature.ranks<-apply(relation,2, 
          function(x){
            data.table::frankv(x,ties.method='average',
                               na.last=T,order=order)
          }
  )
  #if(!distance_like) {
  #  feature.ranks <- dims[1]+1 - feature.ranks
  #}
	feature.ranks<-feature.ranks/dims[1]
	#we applied ranking column-by-column (entity-by-entity); A's were ranked in each row,
	res<-t(apply(feature.ranks,1,rank_diff_and_p_for_the_best))
	rn<-rownames(relation); if (length(rn)==0) {as.character(1:dim(relation)[1])} 
	cn<-rownames(relation); if (length(cn)==0) {as.character(1:dim(relation)[2])} 
	data.frame(feature=1:dims[1],friend=as.integer(res[,1]),p.value=res[,2],feature.name=rownames(relation),friend.name=colnames(relation)[as.integer(res[,1])])
}

#' friends.of.features
#'
#' We have someones to be a friend (entity) as columns and we have what is to be friends of (features) as rows.
#` matrix where in each column and row there is a value of how the raw specifically friendly to this column.
#' @inheritParams best.friends.of.features
#' @param friends_num number of entities we consider for each feature; the default -1 means all; if friends_num is 1, 
#' @return \code{data.frame} with 4 columns: friend entity index, uncorrected p-value for the pair, feature name, friend name 
#' Best friend has the highest order, the worst has the lowest
#' @export
friends.of.features<-function(relation,distance_like=FALSE,friends_num=-1){
  dims<-dim(relation)
  #if relation is distance_like, we will order in ascending
  #if nor, descending. 
  #E.g., the least ranks are the 
  #most close relations 
  order<-ifelse(distance_like,1,-1)
  # if distance_like holds, the least is the best (first)
  #and order==1 (ascending) 
  feature.ranks<-apply(relation,2, 
                       function(x){
                         data.table::frankv(x,ties.method='average',
                                            na.last=T,order=order)
                       }
  )
  #if(!distance_like) {
  #  feature.ranks <- dims[1]+1 - feature.ranks
  #}
  feature.ranks<-feature.ranks/dims[1]
  #we applied ranking column-by-column (entity-by-entity); A's were ranked in each row,
  res<-t(apply(feature.ranks,1,rank_diff_and_p_for_the_best_n,n=friends_num))
  res
  #data.frame(friend=as.integer(res[,1]),p.value=res[,2],feature.name=rownames(relation),friend.name=colnames(relation)[as.integer(res[,1])])
}



