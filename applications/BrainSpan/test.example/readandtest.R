if (!suppressWarnings(require('gene.best.friends')))
{
	if (!suppressWarnings(require('devtools')))
	{
		source("http://bioconductor.org/biocLite.R")
		biocLite("devtools")
		library("devtools")
	}
	install_github('favorov/gene-friends/pkg')
	library(devtools)
	library(gene.best.friends)
}
gene.expression<-fread('21-gene.expr.tsv',stringsAsFactors = FALSE,header = TRUE)
gene.names<-gene.expression[,eval(as.name('# key'))]
gene.expression[,eval(as.name('# key')):=NULL]
gene.expression<-as.data.frame(gene.expression)
message('No batch:')
corr<-cor(t(gene.expression))
colnames(corr)<-gene.names
rownames(corr)<-gene.names
q<-quantile(corr)
q25=q[['25%']]
q75=q[['75%']]
med=q[['50%']]
corrfiltered<-ifelse((corr<q75 & corr> q25),med,corr)
ord<-OrderByBackwardsRank(corr)
ordfilt<-OrderByBackwardsRank(corrfiltered)
message('Direct rank:')
print(gene.names[order(corr[,1],decreasing = TRUE)])
message('RBR:')
print(gene.names[ord[,1]])
message('RBR+filter:')
print(gene.names[ordfilt[,1]])
message('Adding batch (multiply each sample by (sample# % 5 + 1):')
gene.expression.mult<-1:237 %% 5 + 1
gene.expression.batchy.t<-apply(gene.expression,1,function(col)(col*gene.expression.mult))
corr<-cor(gene.expression.batchy.t)
colnames(corr)<-gene.names
rownames(corr)<-gene.names
q<-quantile(corr)
q25=q[['25%']]
q75=q[['75%']]
med=q[['50%']]
corrfiltered<-ifelse((corr<q75 & corr> q25),med,corr)
ord<-OrderByBackwardsRank(corr)
ordfilt<-OrderByBackwardsRank(corrfiltered)
message('Direct rank:')
print(gene.names[order(corr[,1],decreasing = TRUE)])
message('RBR:')
print(gene.names[ord[,1]])
message('RBR+filter:')
print(gene.names[ordfilt[,1]])
#print()
