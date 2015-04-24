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
	report.every<-1000
	message('creating...')
	correlations <- data.frame(rep(0.,genes.number))
	for (i in 2:genes.number)
	{
		correlations<-rbind(correlations,rep(0.,genes.number))
		if (i %% report.every == 0) message(i)
	}
	message(paste0('corr frame is created, number og genes=',genes.number))
	for (i in 1:genes.number)
		correlations[i,i]=1.
	message('identiy table prepared...')
	colnames(correlations)<-gene.idx
	rownames(correlations)<-gene.idx
	save(file='template.brainspan.corr.frame.Rda',list=c('correlations','gene.index'))
	message('saved..')
} else message("identity frame loaded")
