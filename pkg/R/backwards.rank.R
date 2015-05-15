#gene-best-friends library
#A. Favorov, V. Ramensky, A. Mironov 2014

#main functions

#'create gene name lists that are sorted by backwards-rank in the correlations matrix
#'best friends first
#'
#'
#'@param correlations is the correlation-like (similarity) matrix to be processed it is supposed to be ranked; it is desribed by \code{similarity.measure} parameter 
#'@param by.column - if \code{by.column==TRUE} (default) columns of returned object corresponds to tester (our) genes:  if \code{by.column==FALSE}, the rows corrdspond to tester genes 
#'@param similarity.measure; if \code{TRUE}, the \code{correlations} matrix is similarity (the larger it is, the closer are the genes); if it is \code{FALSE}, it is distance
#'@return \code{matrix} object if \code{correlations} is a matrix-like object; \code{data.table} if it is \code{data.table}; error otherwise; each column (or row, see \code{by.column} parameter description) of the object is is a list of genes sorted by backwards rank by relation to the tester gene
sort.by.backwards.rank<-function(correlations,by.column=TRUE,similarity.measure=TRUE){
	
	if('data.table' %in% class(correlations))
		return(.sort.data.table.by.backwards.rank(correlations,by.column,similarity.measure))
	#it is not data.table, go on	
	sign <- ifelse(similarity.measure,-1,1)
	
	backwards.rank<-apply(sign*correlations,2, rank)
	
	#we apply ranking by columns :
	#our gene of interest correspond to row
	#each column correspods to 'tester gene' 
	#and the row of 'our' gene carries the ranks of 'our' 
	#genes by in the tester's correlation lists
	
	# negation is to have decreasing rank 

	#now, for each 'our' gene (row), we prepare 
	#the list of names of testers ordered by
	#rank of 'our' gene in the tester's list 
	sort.by.backwards.rank<-apply(backwards.rank,1,function(set){
		colnames(correlations)[order(set)]}
	)
	#apply return the result in columns, so now 'our' is columns
	colnames(sort.by.backwards.rank)<-colnames(correlations) 
	#apply names of genes

	if(by.column) return(sort.by.backwards.rank)
	#we want to have our genes in rows, so we transpose
	t(sort.by.backwards.rank)	
}


#'create gene name lists that are sorted by backwards-rank in the correlations matrix
#'best friends first
#'
#'@inheritParams sort.by.backwards.rank 
#'@return \code{matrix} object if \code{correlations} is a matrix-like object; \code{data.table} if it is \code{data.table}; error otherwise; each column (or row, see \code{by.column} parameter description) of the object is is a list of rankes of backwards ranks of genes by relation to the tester gene (the gene the row is devoted to)
rank.by.backwards.rank<-function(correlations,by.column=TRUE,similarity.measure=TRUE){
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

#'create distance matrix based on the rank-reverse-rank
#'best friends first
#'
#'
#'@param similarity.measure; if \code{TRUE}, the measure in the matrix is similarity (the larger it is, the closer are the genes); if it is \code{FALSE}, it is distance
#'
#'@return \code{matrix} object if \code{correlations} is a matrix-like object; \code{data.table} if it is \code{data.table}; error otherwise; the return is new distance matrix 
distance.by.backwards.rank<-function(correlations,similarity.measure=TRUE)
{
	sign <- ifelse(similarity.measure,-1,1)
	backwards.rank<-apply(sign*correlations,2,rank)
	rank.backwards.rank<-apply(backwards.rank,1,rank)
	rownames(rank.backwards.rank)<-rownames(correlations)
	colnames(rank.backwards.rank)<-colnames(correlations)
	#it is the same code as for rank.by.backwards.rank
	#we do not call it to avoid t() twice, just to save time
	maxrank<-dim(correlations)[2]-1
	tra<-t(rank.backwards.rank)
	mitra<-min(tra,rank.backwards.rank)
	matra<-max(tre,rank.backwards.rank)
	ifelse(matra*2<maxrank,mitra,matra)/(2*maxrank-1)
}

