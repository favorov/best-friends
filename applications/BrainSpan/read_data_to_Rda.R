library(data.table)
log.file.name='corr.min1.prenatal.log'
corr.snake.name="corr.min1.prenatal.nr.tsv"
gene.index<-fread(log.file.name)
setnames(gene.index,1:3,c('idx','gene.id','cnt'))
gene.idx<-gene.index[,idx]
#debug
correlations <- data.table(idx=gene.idx)
setkey(correlations,idx)
message('frame is created')
corr.data<-fread(corr.snake.name)
setnames(corr.data,1:3,c('idx1','idx2','corr'))
#curr.idx1 = -1
#band.start = 1
#band.end = 0 #inclusive
#maxindex<-dim(corr.data[1])
maxindex=1000000
for (i in 1:maxindex)
{
	ind1<-corr.data[i,idx1]
	ind2<-corr.data[i,idx2]
	c<-corr.data[i,corr]
	correlations[idx==ind1,eval(as.name(ind2)):=corr]
}
save(file='prenatal.correlations.matrix.Rda',list=c('correlations','gene.index'))
