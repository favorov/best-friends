if (!suppressWarnings(require('gene.best.friends')))
{
	if (!suppressWarnings(require('devtools')))
	{
		source("http://bioconductor.org/biocLite.R")
		biocLite("devtools")
		library("devtools")
	}
	install_github('favorov/gene-friends/pkg')
	library(gene.best.friends)
}
load('prenatal.22k.correlations.Rda')
order.22k.prenatal<-OrderByBackwardsRank(corr.22k.prenatal)
save(list='order.22k.prenatal',file='prenatal.22k.order.Rda')
