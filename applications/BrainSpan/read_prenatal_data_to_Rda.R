source('create_template_correlation_table_start.R')
#	save(file='template.brainspan.corr.table.Rda',list=c('correlations','gene.index')
snake<-file(description = 'corr.min1.prenatal.nr.tsv',open='r')
report.every<-1000000
i<-0
line<-character(0)
repeat
{
	line<-scan(file=snake,nlines=1,what='character',quiet=TRUE)
	if (length(line)!=3) break
	i<-i+1
	if (i %% report.every == 0)
		cat(paste0(i,"\n"))
	ind1<-line[1]
	ind2<-line[2]
	c<-as.numeric(line[3])
	correlations[idx==ind1,eval(as.name(ind2)):=c]
	correlations[idx==ind2,eval(as.name(ind1)):=c]
}
close(snake)
save(file='prenatal.brainspan.corr.table.Rda',list=c('correlations','gene.index'))

