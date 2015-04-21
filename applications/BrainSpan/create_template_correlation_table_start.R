library(data.table)
template.brainspan.corr.table.loaded<-FALSE
if(file.exists('template.brainspan.corr.table.Rda'))
	if ('correlations' %in% load('template.brainspan.corr.table.Rda'))
		if ('data.table' %in% class(correlations))
			template.brainspan.corr.table.loaded<-TRUE

if (!template.brainspan.corr.table.loaded)
{
	log.file.name='corr.min1.prenatal.log'
	corr.snake.name="corr.min1.prenatal.nr.tsv"
	gene.index<-fread(log.file.name)
	setnames(gene.index,1:3,c('idx','gene.id','cnt'))
	gene.idx<-gene.index[,idx]
	genes.number<-length(gene.idx)
	message('index table is created')
	#debug
	correlations <- data.table(idx=gene.idx)
	setkey(correlations,idx)
	message(paste0('corr table is created, number og genes=',genes.number))
	report.every<-1000
	for (i in 1:genes.number)
	{
		if (i %% report.every == 0)
			cat(paste0(i,"\n"))
		id<-gene.idx[i]
		correlations[,eval(as.name(id)):=rep(0,genes.number)]
		correlations[idx==id,eval(as.name(id)):=1]
	}
	message('corr table is a diagonal 0/1 now')
	save(file='template.brainspan.corr.table.Rda',list=c('correlations','gene.index','genes.number'))
}
