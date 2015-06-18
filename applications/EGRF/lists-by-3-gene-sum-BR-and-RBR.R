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
order.to.three<-backwards.ranks[,'EGFR']+backwards.ranks[,'ERBB2']+backwards.ranks[,'ERBB3']

RBRs<-RankByBackwardsRank(HaCaT.Corr.Spearman.Diff)
friend.to.three<-RBRs[,'EGFR']+RBRs[,'ERBB2']+RBRs[,'ERBB3']

load('TRANSFAC_Genes_2014.Rda')
gene.names<-setdiff(gene.names,c('EGFR','ERBB2','ERBB3'))
TRANSFAC.p.values.BR<-sapply(TF2Gene,function(TF.list){
		in.list=intersect(TF.list,gene.names)
		out.list=setdiff(gene.names,in.list)
		if(length(in.list)==0 || length(out.list)==0) return(NA)
		wilcox.test(order.to.three[in.list],order.to.three[out.list])$p.value
	}
)

TRANSFAC.p.values.RBR<-sapply(TF2Gene,function(TF.list){
		in.list=intersect(TF.list,gene.names)
		out.list=setdiff(gene.names,in.list)
		if(length(in.list)==0 || length(out.list)==0) return(NA)
		wilcox.test(friend.to.three[in.list],friend.to.three[out.list])$p.value
	}
)

save(file='TRANSFAC_p_values.Rda',list=c('TRANSFAC.p.values.BR','TRANSFAC.p.values.RBR'))
