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

backwards.ranks<-BackwardsRank(HaCaT.Corr.Spearman.Diff)
BR<-backwards.ranks[,'EGFR']

RBRs<-RankByBackwardsRank(HaCaT.Corr.Spearman.Diff)
RBR<-RBRs[,'EGFR']

load('TRANSFAC_Genes_2014.Rda')
gene.names<-setdiff(gene.names,c('EGFR'))

TRANSFAC.p.values.EGFR.BR<-sapply(TF2Gene,function(TF.list){
		in.list=intersect(TF.list,gene.names)
		out.list=setdiff(gene.names,in.list)
		if(length(in.list)==0 || length(out.list)==0) return(NA)
		wilcox.test(BR[in.list],BR[out.list])$p.value
	}
)

TRANSFAC.p.values.EGFR.RBR<-sapply(TF2Gene,function(TF.list){
		in.list=intersect(TF.list,gene.names)
		out.list=setdiff(gene.names,in.list)
		if(length(in.list)==0 || length(out.list)==0) return(NA)
		wilcox.test(RBR[in.list],RBR[out.list])$p.value
	}
)

save(file='TRANSFAC_p_values_3_genes.Rda',list=c('TRANSFAC.p.values.3genes.BR','TRANSFAC.p.values.3genes.RBR'))
