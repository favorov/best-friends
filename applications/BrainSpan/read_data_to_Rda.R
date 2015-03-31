log.file.name='corr.min1.prenatal.log'
corr.snake.name="corr.min1.prenatal.nr.tsv"
genenames<-genenames<-read.table(log.file.name,stringsAsFactors = FALSE,colClasses = c('integer','character','integer'))
#debug
#genenames<-genenames[1:1000,]
number_of_genes<-dim(genenames)[1]
correlations <- data.frame(t(rep(0.,number_of_genes)))
message('frame is created')
correlations[number_of_genes,number_of_genes]=1.
message('corner is set')
colnames(correlations)=genenames[,2]
rownames(correlations)=genenames[,2]
message('names are set')
for (ind in 1:number_of_genes-1) {
	correlations[ind,ind]=1.
}
message('diagonal is set')
nr.tsv <- file(corr.snake.name,"r")
while(length(line <- readLines(nr.tsv, 1)) > 0) {
	spli<-strsplit(line,split="\t")[[1]]
	coord1=which(genenames[,1]==spli[1])
	coord2=which(genenames[,1]==spli[2])
	correlations[coord1,coord2]<-correlations[coord2,coord1]<-spli[3]
}
close(nr.tsv)
save(file='prenatal.correlations.matrix.Rda',list=c('correlations'))
