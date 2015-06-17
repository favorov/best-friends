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

load('HaCaT.TFGene.TrtDiff.RData')
HaCaT.Corr.Spearman.Diff<-cor(t(HaCaT.TFGene.TrtDiff),method = 'spearman')

gene.names<-colnames(HaCaT.Corr.Spearman.Diff)

OBRs<-OrderByBackwardsRank(HaCaT.Corr.Spearman.Diff)

EGFR.friends<-gene.names[OBRs[,'EGFR']]
ERBB2.friends<-gene.names[OBRs[,'ERBB2']]
ERBB3.friends<-gene.names[OBRs[,'ERBB3']]

save(file='EGFRfriends.Rda',list=('EGFR.friends','ERBB2.friends','ERBB3.friends')
