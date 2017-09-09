#best-friends-of- library
#A. Favorov, V. Ramensky, A. Mironov 2014-2017

#'RankByBestFriendsOf
##
#'We have someones to be friended by as columns and we have the possible frieds as rows. 
#'The function outputs a matrix where in each column, the list of the numbers of the rows are ordered by how this row is specifically friendly to this column. Create gene number lists that are orders of genes in ranking by backwards-rank of the genenes correlations matrix.
#'\code{my.row.names[number list]} will give the sorted list of gene nemes
#'Best friend has the lowest order.
#'
#'@param frriendship is the similarity matrix to be processed; the someones to be frinded by are columns; frinds are rows  
#'@return \code{matrix} object if \code{frriendship} is a matrix-like object; \code{data.table} if it is \code{data.table}; error otherwise; each column (or row if \code{by.column}, see \code{by.column} parameter description) of the object is is a list of orders of genes as sorted by backwards rank by relation to the tester gene, tester gen is the column (if \code{by.column})
#'export
RankByBestFriendOf<-function(friendship){
	#if('data.table' %in% class(friendship))
	#	return(OrderByBackwardsRank.data.table(friendship))
	#it is not data.table, go on	
		
	#columns are A, and rows are B
	#we rank b's for each b
	reflexive.ranks<-apply(friendship,1, frank)
	#we applied ranking row-by-row; A's were ranked in each row,
	#and apply transposes, so now B are columns
	#and and rows are A and they are ranked (upper rank is highest) 
	
	#now, we select a's and rank the a's ranks for each b
	#so, row-by-row again
	rank.by.best.friends.rank<-apply(reflexive.ranks,1,frank)
	#apply return the result in columns, so now 'our' is columns
	colnames(rank.by.best.friends.rank)<-colnames(friendship) 
	rownames(rank.by.best.friends.rank)<-rownames(friendship) 
}

