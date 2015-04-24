source('create_template_identity_frame.R')
#	save(file='template.brainspan.corr.table.Rda',list=c('correlations','gene.index')
snake<-file(description = 'corr.min1.prenatal.nr.tsv',open='r')
message('Snake opened')
block.size<-1000000
gene.idx<-gene.index[,idx]
max.gene.idx<-max(gene.idx)
#we know that the idx's are 0-based
rev.gen.idx<-integer(max.gene.idx+1)
for(i in 1:length(gene.idx))
	rev.gen.idx[gene.idx[i]+1]<-i
#now to find index no by idx we say rev.gen.idx[idx+1]
no<-0
#no locals
lines<-data.frame()
id1<-id2<-''
c<-0.0
ind1<-ind2<-0
repeat
{
	lines<-scan(file=snake,nlines=block.size,what=list('','',''),quiet=TRUE)
	read.no<-length(lines[[1]])
	if (read.no==0) break
	no<-no+1
	message(paste0('Block no ',no))
	for (i in 1:read.no)
	{
		id1<-lines[[1]][i]
		id2<-lines[[2]][i]
		ind1<-rev.gen.idx[as.numeric(id1)+1]
		ind2<-rev.gen.idx[as.numeric(id2)+1]
		c<-as.numeric(lines[[3]][i])
		correlations[id1,id2]=c
		correlations[id2,id1]=c
	}
}
close(snake)
save(file='prenatal.brainspan.corr.Rda',list=c('correlations','gene.index'))

