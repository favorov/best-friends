source('create_template_correlation_table_start.R')
#	save(file='template.brainspan.corr.table.Rda',list=c('correlations','gene.index')
snake<-file(description = 'corr.min1.prenatal.nr.tsv',open='r')
report.every<-1000000
i<-0
while(length(line<-scan(file=snake,nlines=1,what='character'))==3)
{
	i<i+1
	if (i %% report.every == 0)
		cat(paste0(i,"\n"))
	ind1<-scan[1]
	ind2<-scan[2]
	c<-as,numeric(scan[3])
	correlations[idx==ind1,eval(as.name(ind2)):=c]
	correlations[idx==ind2,eval(as.name(ind1)):=c]
}
save(file='prenatal.brainspan.corr.table.Rda',list=c('correlations','gene.index'))

