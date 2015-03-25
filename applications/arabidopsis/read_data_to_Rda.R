names<-read.table('Map_Gene_Names.txt',stringsAsFactors=FALSE)
pearsonny<-read.table('Map_Mean_Expr_corr2.txt')
colnames(pearsonny)<-names
rownames(pearsonny)<-names
save(file='corr_matrix.Rda',list=c('pearsonny'))