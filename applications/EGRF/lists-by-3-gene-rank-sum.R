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
backwards.ranks<-apply(-1*HaCaT.Corr.Spearman.Diff,2, rank)
order.to.three<-sapply(gene.names,function(g) backwards.ranks[g,'EGFR']+backwards.ranks[g,'ERBB2']+backwards.ranks[g,'ERBB3'])
load('TRANSFAC_Genes_2014.Rda')
gene.names<-setdiff(gene.names,c('EGFR','ERBB2','ERBB3'))
TRASFAC.p.values<-sapply(TF2Gene,function(TF.list){
		in.list=intersect(TF.list,gene.names)
		out.list=setdiff(gene.names,in.list)
		if(length(in.list)==0 || length(out.list)==0) return(NA)
		wilcox.test(order.to.three[in.list],order.to.three[out.list])$p.value
	}
)
save(file='TRANSFAC_p_values.Rda',list=c('TRANSFAC.p.values'))
