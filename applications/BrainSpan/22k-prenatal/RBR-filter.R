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
load('prenatal.22.expression.Rda')
order.22k.prenatal.filtered<-OrderByBackwardsRank(corr.22k.prenatal)
save(list='order.22k.prenatal.filtered',file='prenatal.22.correlations.filtered.Rda')
