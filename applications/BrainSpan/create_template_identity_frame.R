library(data.table)
template.brainspan.corr.frame.loaded<-FALSE
if(file.exists('template.brainspan.corr.Rda'))
	if ('correlations' %in% load('template.brainspan.corr.frame.Rda'))
		if ('data.table' %in% class(correlations))
			template.brainspan.corr.frame.loaded<-TRUE

if (!template.brainspan.corr.frame.loaded)
{
	log.file.name='corr.min1.prenatal.log'
	corr.snake.name="corr.min1.prenatal.nr.tsv"
	gene.index<-fread(log.file.name)
	setnames(gene.index,1:3,c('idx','gene.id','cnt'))
	gene.idx<-gene.index[,idx]
	genes.number<-length(gene.idx)
	message('index table is created')
	#debug
	correlations.table <- data.table(idx=gene.idx)
	setkey(correlations.table,idx)
	message(paste0('corr table is created, number og genes=',genes.number))
	report.every<-1000
	for (i in 1:genes.number)
	{
		if (i %% report.every == 0)
			cat(paste0(i,"\n"))
		id<-gene.idx[i]
		correlations.table[,eval(as.name(id)):=rep(0,genes.number)]
		correlations.table[idx==id,eval(as.name(id)):=1]
	}
	message('identiy table prepared...')
	correlation.with.names.column<-as.data.frame(correlations.table)
	rm(correlations.table)
	message('converted to frame...')
	correlations<-correlation.with.names.column[,-1] #remove the names
	message('finalised...')
	colnames(correlations)<-gene.idx
	rownames(correlations)<-gene.idx
	save(file='template.brainspan.corr.frame.Rda',list=c('correlations','gene.index'))
	message('saved..')
}
else message("identity frame loaded")
