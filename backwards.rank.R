sort.by.backwards.rank<-function(correlations){
	#The function returns the list of tester genes 
	#that are sorted by of bckw-rank of our gene (row)
	backwards.rank<-apply(-correlations,2, rank)
	
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

	#we want to have our genes in rows, so we transpose

	t(sort.by.backwards.rank)	
}


rank.by.backwards.rank<-function(correlations){
	#this function returns the rank of bckw-rank of our gene (row)
	#in the tester gene's list (column)
	backwards.rank<-apply(-correlations,2, rank)
	
	#see comments in sort.by.backwards.rank()

	rank.backwards.rank<-apply(backwards.rank,1,rank)
	# now, we rank tester by rank of our in the tester's list
	# 'our' genes are columns, tester are rows (apply does this)
	rownames(rank.backwards.rank)<-rownames(correlations)
	colnames(rank.backwards.rank)<-colnames(correlations)
	#after t, our genes is row 
	t(rank.backwards.rank)	
}

distance.by.backwards.rank<-function(correlations){
	backwards.rank<-apply(-correlations,2, rank)
	rank.backwards.rank<-apply(backwards.rank,1,rank)
	rownames(rank.backwards.rank)<-rownames(correlations)
	colnames(rank.backwards.rank)<-colnames(correlations)
	#it is the same code as for rank.by.backwards.rank
	#we do not call it to avoid t() twice, just to save time
	(t(rank.backwards.rank)+rank.backwards.rank-2)/(2*dim(correlations)[1]-2)	
}

