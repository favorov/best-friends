library(data.table)
log.file.name='corr.min1.prenatal.log'
corr.snake.name="corr.min1.prenatal.nr.tsv"
gene.index<-fread(log.file.name)
setnames(gene.index,1:3,c('idx','gene.id','cnt'))
gene.idx<-gene.index[,idx]
#debug
correlations <- data.table(idx=gene.idx) 
message('frame is created')
save(file='prenatal.correlations.matrix.Rda',list=c('correlations','gene.index'))
