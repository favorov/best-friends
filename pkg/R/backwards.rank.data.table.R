.sort.data.table.by.backwards.rank<-function(correlation.table,by.column=TRUE,similarity.measure=TRUE){
	#correlations.table is now a data.table with correlations
	#all colnames are the names (indices) of genes
	#if the table is square, its structure is 
	#diagonally-summetric correlation-like
	#square martix
	#if number of columns is number of rows+1, 
	#the first variable is the name (index) of the gene
	#so, it is a column of names and a diagonally-summetric correlation-like
	#square martix
	
	#the function returns a data.table with the lists of 
	#genes sorted by rev-rev rank
	#for each gene 
	#if by.column=TRUE (default) the the data.table where each 
	#tester gene's list is a row, the first variable has the same name as  
	#list of tester genes 
	#that are sorted by of bckw-rank of our gene (row)

	#test that the data.table is what we wait for
	#'data.table' %in% class(rank.table) - data.table table check here, better to write generic

	if(!'data.table' %in% class(correlation.table))
			stop("First parameter of sort.table.by.backwards.rank is not data.table.")

	###here, we will place the check whether we have 1-st column or not

	dims<-dim(correlation.table) #it is data.table as far as we are here. 
	#dims has dimension 2

	first.column.with.names<-FALSE
		
	if (dims[1] != dims[2] && dims[1]+1 != dims[2]  )
		stop('The data.pable passed to sort.data.table.by.backwards.rank is neither square nor соlumn+square')

	if (dims[1]==dims[2]) {
		#square; first.column.with.names=FLASE as it was inited
		gene.names<-colnames(correlation.table)
		if (length(gene.names)!=length(unique(gene.names)))
			stop('The colnames of data.pable passed to sort.data.table.by.backwards.rank are no unique')
	}
	else
	{
		gene.names<-colnames(correlation.table)[-1] #all the column names but first
		if (length(gene.names)!=length(unique(gene.names)))
			stop('The colnames of data.pable passed to sort.data.table.by.backwards.rank are not unique')
		my.row.names<-correlation.table[,eval(as.name(colnames(correlation.table)[1]))]
		if (length(my.row.names)!=length(unique(my.row.names)))
			stop('The names in the first column of the data.pable passed to sort.data.table.by.backwards.rank are not unique')
		#gene.names is index we apply to gene.names to obtain my.row.names if the lists are equal sets;
		#otherwise, the procedure will not provide my.row.names
		#genes2rows<-order(gene.names)[rank(my.row.names)]
		if (!identical(my.row.names,gene.names)) # if they are, we are not to do all the mapping
		{
			genes2rows<-charmatch(my.row.names,gene.names)
			if (!identical(my.row.names,gene.names[genes2rows]))
					stop("Correlaion table passed to sort.by.backwards.rank is not organised as we suppose to see:\nthe names in first column do not map to column names")
			#now, we use genes2rows as an ordering index to obtain the same order of names in rows as it was in gene.names
			correlation.table[,...TO.ORDER:=genes2rows] #...TO.ORDER is a column to be ordered, then, we remove it.
			setorder(correlation.table,'...TO.ORDER')
			correlation.table[,...TO.ORDER:=NULL] #...TO.ORDER is not necessary an more
		}
	}
	
	#creating a data.table with columns
	#our gene of interest correspond to row
	#each column correspods to 'tester gene' 
	#and the row of 'our' gene carries the ranks of 'our' 
	#genes by in the tester's correlation lists
	
	#first, the first column of the new table, it is for the first name
	
	sign <- ifelse(similarity.measure,-1,1)

	backwards.rank.table<-data.table(rank(sign*correlation.table[,eval(as.name(gene.names[1]))]))
	#set first column name. Ugly, but eval(as.name()) works only for [ rather than for constructor
	setnames(backwards.rank.table,1,gene.names[1])

	for(gene in gene.names[-1]) #all n-1 remaining guys
		backwards.rank.table[,eval(as.name(gene)):=rank(sign*correlation.table[,eval(as.name(gene))])]
	
	# negation is to have decreasing rank 

	#now, for each 'our' gene (column), we prepare 
	#the list of names of testers ordered by
	#rank of 'our' gene in the tester's list 
	sorted.table<-data.table(gene.names[order(backwards.rank.table[1,])])
	#it means: first row, but no first (name) element
	setnames(sorted.table,1,gene.names[1])
	#see comment for previuos setname
	for (i in 2:length(gene.names))
		sorted.table[,eval(as.name(gene.names[i])):=gene.names[order(backwards.rank.table[i,])]]
	#browser()
	#apply return the result in columns, so now 'our' is columns
	#colnames(sort.by.backwards.rank)<-colnames(correlations) 
	#apply names of genes

	#we want to have our genes in rows, so we transpose

	#t(sort.by.backwards.rank)	
	if(by.column){return(sorted.table)}
	#if we are here, we need to transpose the thing
	sorted.table.t<-data.table(gene.names)
	setnames(sorted.table.t,1,'name')
	#browser()
	for (i in 1:length(gene.names))
		sorted.table.t[,eval(as.name(i)):=unlist(sorted.table[i,])]
	sorted.table.t
}


.rank.data.table.by.backwards.rank<-function(correlation.table,by.column=TRUE,similarity.measure=TRUE){
	#correlations.table is now a data.table with correlations
	#all colnames are the names (indices) of genes
	#if the table is square, its structure is 
	#diagonally-summetric correlation-like
	#square martix
	#if number of columns is number of rows+1, 
	#the first variable is the name (index) of the gene
	#so, it is a column of names and a diagonally-summetric correlation-like
	#square martix
	
	#the function returns a data.table with the lists of 
	#genes sorted by rev-rev rank
	#for each gene 
	#if by.column=TRUE (default) the the data.table where each 
	#tester gene's list is a row, the first variable has the same name as  
	#list of tester genes 
	#that are sorted by of bckw-rank of our gene (row)

	#test that the data.table is what we wait for
	#'data.table' %in% class(rank.table) - data.table table check here, better to write generic

	if(!'data.table' %in% class(correlation.table))
			stop("First parameter of sort.table.by.backwards.rank is not data.table.")

	###here, we will place the check whether we have 1-st column or not

	dims<-dim(correlation.table) #it is data.table as far as we are here. 
	#dims has dimension 2

	first.column.with.names<-FALSE
		
	if (dims[1] != dims[2] && dims[1]+1 != dims[2]  )
		stop('The data.pable passed to sort.data.table.by.backwards.rank is neither square nor соlumn+square')

	if (dims[1]==dims[2]) {
		#square; first.column.with.names=FLASE as it was inited
		gene.names<-colnames(correlation.table)
		if (length(gene.names)!=length(unique(gene.names)))
			stop('The colnames of data.pable passed to sort.data.table.by.backwards.rank are no unique')
	}
	else
	{
		gene.names<-colnames(correlation.table)[-1] #all the column names but first
		if (length(gene.names)!=length(unique(gene.names)))
			stop('The colnames of data.pable passed to sort.data.table.by.backwards.rank are not unique')
		my.row.names<-correlation.table[,eval(as.name(colnames(correlation.table)[1]))]
		if (length(my.row.names)!=length(unique(my.row.names)))
			stop('The names in the first column of the data.pable passed to sort.data.table.by.backwards.rank are not unique')
		#gene.names is index we apply to gene.names to obtain my.row.names if the lists are equal sets;
		#otherwise, the procedure will not provide my.row.names
		#genes2rows<-order(gene.names)[rank(my.row.names)]
		if (!identical(my.row.names,gene.names)) # if they are, we are not to do all the mapping
		{
			genes2rows<-charmatch(my.row.names,gene.names)
			if (!identical(my.row.names,gene.names[genes2rows]))
					stop("Correlaion table passed to sort.by.backwards.rank is not organised as we suppose to see:\nthe names in first column do not map to column names")
			#now, we use genes2rows as an ordering index to obtain the same order of names in rows as it was in gene.names
			correlation.table[,...TO.ORDER:=genes2rows] #...TO.ORDER is a column to be ordered, then, we remove it.
			setorder(correlation.table,'...TO.ORDER')
			correlation.table[,...TO.ORDER:=NULL] #...TO.ORDER is not necessary an more
		}
	}
	
	#creating a data.table with columns
	#our gene of interest correspond to row
	#each column correspods to 'tester gene' 
	#and the row of 'our' gene carries the ranks of 'our' 
	#genes by in the tester's correlation lists
	
	#first, the first column of the new table, it is for the first name
	
	sign <- ifelse(similarity.measure,-1,1)

	backwards.rank.table<-data.table(rank(sign*correlation.table[,eval(as.name(gene.names[1]))]))
	#set first column name. Ugly, but eval(as.name()) works only for [ rather than for constructor
	setnames(backwards.rank.table,1,gene.names[1])

	for(gene in gene.names[-1]) #all n-1 remaining guys
		backwards.rank.table[,eval(as.name(gene)):=rank(sign*correlation.table[,eval(as.name(gene))])]
	
	# negation is to have decreasing rank 

	#now, for each 'our' gene (column), we prepare 
	#the list of names of testers ordered by
	#rank of 'our' gene in the tester's list 
	sorted.table<-data.table(gene.names[order(backwards.rank.table[1,])])
	#it means: first row, but no first (name) element
	setnames(sorted.table,1,gene.names[1])
	#see comment for previuos setname
	for (i in 2:length(gene.names))
		sorted.table[,eval(as.name(gene.names[i])):=gene.names[order(backwards.rank.table[i,])]]
	#browser()
	#apply return the result in columns, so now 'our' is columns
	#colnames(sort.by.backwards.rank)<-colnames(correlations) 
	#apply names of genes

	#we want to have our genes in rows, so we transpose

	#t(sort.by.backwards.rank)	
	if(by.column){return(sorted.table)}
	#if we are here, we need to transpose the thing
	sorted.table.t<-data.table(gene.names)
	setnames(sorted.table.t,1,'name')
	#browser()
	for (i in 1:length(gene.names))
		sorted.table.t[,eval(as.name(i)):=unlist(sorted.table[i,])]
	sorted.table.t
}

.distance.data.table.by.backwards.rank<-function(correlations){
	backwards.rank<-apply(-correlations,2, rank)
	rank.backwards.rank<-apply(backwards.rank,1,rank)
	rownames(rank.backwards.rank)<-rownames(correlations)
	colnames(rank.backwards.rank)<-colnames(correlations)
	#it is the same code as for rank.by.backwards.rank
	#we do not call it to avoid t() twice, just to save time
	(t(rank.backwards.rank)+rank.backwards.rank-2)/(2*dim(correlations)[1]-2)	
}

