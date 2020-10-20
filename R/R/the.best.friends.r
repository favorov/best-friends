#best-friends-of- library
#A. Favorov, V. Ramensky, A. Mironov 2014-2020
#'
#'
#' best.friends.of
#'
#' We have someones to be a friend (entity) as columns and we have what is to be friends of (features) as rows.
#` matrix where in each column and row there is a value of how the raw specifically friendly to this column.
#' @param relation is the fetures*entities matrix of the relations between features ans entities
#' @return \code{data.frame} with 3 columns: feature index, friend entity index, uncorrected p-value for the pair
#' Best friend has the highest order, the worst has the lowest
#' @export
best.friends.of<-function(relation,distance_like=FALSE){
  dims<-dim(relation)
  #if relation is distance_like, we will order in ascending
  #if nor, descending. 
  #E.g., the least ranks are the 
  #most close relations 
  feature.ranks<-apply(relation,2, 
          function(x){
            data.table::frank(x,ties.method='average')
          }
  )
  if(!distance_like) {
    feature.ranks <- dims[1]+1 - feature.ranks
  }
	feature.ranks<-feature.ranks/dims[1]
	#we applied ranking column-by-column (entity-by-entity); A's were ranked in each row,
	res<-t(apply(feature.ranks,1,friend_and_p_value_cpp))
	data.frame(friend=as.integer(res[,1]),p.value=res[,2])
}




