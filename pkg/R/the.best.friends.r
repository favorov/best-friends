#best-friends-of- library
#A. Favorov, V. Ramensky, A. Mironov 2014-2017

#'OrderBestFriendsOf
##
#'We have someones to be friended by as columns and we have the possible frieds as rows. 
#'The function outputs a matrix where in each column, the list of the numbers of the rows are ordered by how this row is specifically friendly to this column. Create gene number lists that are orders of genes in ranking by backwards-rank of the genenes correlations matrix.
#'\code{my.row.names[number list]} will give the sorted list of gene nemes
#'Best friend has the lowest order.
#'
#'@param frriendship is the similarity matrix to be processed; the someones to be frinded by are columns; frinds are rows  
#'@return \code{matrix} object if \code{frriendship} is a matrix-like object; \code{data.table} if it is \code{data.table}; error otherwise; each column (or row if \code{by.column}, see \code{by.column} parameter description) of the object is is a list of orders of genes as sorted by backwards rank by relation to the tester gene, tester gen is the column (if \code{by.column})
#'export
OrderByBackwardsRank<-function(friendship){
	#if('data.table' %in% class(friendship))
	#	return(OrderByBackwardsRank.data.table(friendship))
	#it is not data.table, go on	
	
	#we rannk the someones by how the guy is of value for the someone
	backwards.rank<-apply(correlations,1, frank)
		
	#we apply ranking row-by-row columns are rnked:
	#our gene of interest correspond to row
	#each column correspods to 'tester gene' 
	#and the row of 'our' gene carries the ranks of 'our' 
	#genes by in the tester's correlation lists
	
	# negation is to have decreasing rank 

	#now, for each 'our' gene (row), we prepare 
	#the list of names of testers ordered by
	#rank of 'our' gene in the tester's list 
	order.by.backwards.rank<-apply(backwards.rank,1,order)
	#apply return the result in columns, so now 'our' is columns
	colnames(order.by.backwards.rank)<-colnames(correlations) 
	#apply names of genes

	if(by.column) return(order.by.backwards.rank)
	#we want to have our genes in rows, so we transpose
	t(order.by.backwards.rank)	
}

#'Rank by backwards rank

#'Creates the matrix of rankes of genes by the backwards ranks in correlation matrix
#'
#'@inheritParams OrderByBackwardsRank 
#'@return \code{matrix} object if \code{correlations} is a matrix-like object; \code{data.table} if it is \code{data.table}; error otherwise; each column (or row, see \code{by.column} parameter description) of the object represet ranks of genes by backwards-rank relation to the tester gene tester gene is column if \code{by.column}; the possible friend of tester gene is row; the lower is the rank, the better is the friend
RankByBackwardsRank<-function(correlations,by.column=TRUE,similarity.measure=TRUE){
	if('data.table' %in% class(correlations))
		return(.RankByBackwardsRank.data.table(correlations,by.column,similarity.measure))
	#it is not data.table, go on	
	#this function returns the rank of bckw-rank of our gene (row)
	#in the tester gene's list (column)
	sign <- ifelse(similarity.measure,-1,1)
	
	backwards.rank<-apply(sign*correlations,2, rank)
	
	#see comments in sort.by.backwards.rank()

	rank.backwards.rank<-apply(backwards.rank,1,rank)
	# now, we rank tester by rank of our in the tester's list
	# 'our' genes are columns, tester are rows (apply does this)
	rownames(rank.backwards.rank)<-rownames(correlations)
	colnames(rank.backwards.rank)<-colnames(correlations)
	#after t, our genes is row 
	if(by.column) return(rank.backwards.rank)
	#we want to have our genes in rows, so we transpose
	t(rank.backwards.rank)	
}




#'backwards rank
#'
#'The simplest function of the family, just rank data in columns and return
#' it in columns if by \code{by.column==TRUE}
#'
#'@inheritParams OrderByBackwardsRank 
#'@return \code{matrix} object if \code{relations} is a matrix-like object; 
#'@where columns are ranks in columns of relations matrix if \code{by.column} is \code{TRUE}
#'@or rows are ranks in rows of relations matrix if \code{by.column} is \code{FALSE}

BackwardsRank<-function(relations,by.column=TRUE,similarity.measure=TRUE){
	
	#if('data.table' %in% class(correlations))
	#	return(.BackwardsRank.data.table(correlations,by.column,similarity.measure))
	#it is not data.table, go on --- template	
	sign <- ifelse(similarity.measure,-1,1)
	
	backwards.rank<-apply(sign*correlations,2, rank)
	if(by.column) return(backwards.rank)
	#we want to have our genes in rows, so we transpose
	t(backwards.rank)	
}

