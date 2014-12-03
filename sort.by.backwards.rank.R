#The function returns the the list of tester genes that are sorted by of rev-rank of our gene (row)
#of our gene (rows). 
#The code is the same with the Rmd file
sort.by.backwards.rank<-function(correlations){
	backwards.rank<-apply(-correlations,2, rank)

	# negation is to have decreasing rank
	
	#we apply it by columns because our gene of interest is in row
	#each column correspods to 'tester gene' and the rank 'our' genes by 
	#the correlation with tester.
	
	sort.by.backwards.rank<-apply(backwards.rank,1,function(set){
		colnames(correlations)[order(set)]}
	)
	#apply cbinds, we want to have our genes in rows, do we transpose

	colnames(sort.by.backwards.rank)<-colnames(correlations) 
	#it will be rownames
	
	t(sort.by.backwards.rank)	
}
