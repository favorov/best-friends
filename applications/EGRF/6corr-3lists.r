library("gene.best.friends")
library(WGCNA)

load('HaCaT.TFGene.TrtMean.RData')
HaCaT.Corr.Pearson.Mean<-cor(t(HaCaT.TFGene.TrtMean),method = 'pearson')
HaCaT.Corr.Spearman.Mean<-cor(t(HaCaT.TFGene.TrtMean),method = 'spearman')
HaCaT.Corr.bicor.Mean<-bicor(t(HaCaT.TFGene.TrtMean))

load('HaCaT.TFGene.TrtDiff.RData')
HaCaT.Corr.Pearson.Diff<-cor(t(HaCaT.TFGene.TrtDiff),method = 'pearson')
HaCaT.Corr.Spearman.Diff<-cor(t(HaCaT.TFGene.TrtDiff),method = 'spearman')
HaCaT.Corr.bicor.Diff<-bicor(t(HaCaT.TFGene.TrtDiff))

gene.names<-colnames(HaCaT.Corr.bicor.Diff)

RBRs<-OrderByBackwardsRank(HaCaT.Corr.Pearson.Mean)
EGFR.Pearson.Mean.friends<-gene.names[RBRs[,'EGFR']]
ERBB2.Pearson.Mean.friends<-gene.names[RBRs[,'ERBB2']]
ERBB3.Pearson.Mean.friends<-gene.names[RBRs[,'ERBB3']]

RBRs<-OrderByBackwardsRank(HaCaT.Corr.Spearman.Mean)
EGFR.Spearman.Mean.friends<-gene.names[RBRs[,'EGFR']]
ERBB2.Spearman.Mean.friends<-gene.names[RBRs[,'ERBB2']]
ERBB3.Spearman.Mean.friends<-gene.names[RBRs[,'ERBB3']]

RBRs<-OrderByBackwardsRank(HaCaT.Corr.bicor.Mean)
EGFR.bicor.Mean.friends<-gene.names[RBRs[,'EGFR']]
ERBB2.bicor.Mean.friends<-gene.names[RBRs[,'ERBB2']]
ERBB3.bicor.Mean.friends<-gene.names[RBRs[,'ERBB3']]

RBRs<-OrderByBackwardsRank(HaCaT.Corr.Pearson.Diff)
EGFR.Pearson.Diff.friends<-gene.names[RBRs[,'EGFR']]
ERBB2.Pearson.Diff.friends<-gene.names[RBRs[,'ERBB2']]
ERBB3.Pearson.Diff.friends<-gene.names[RBRs[,'ERBB3']]

RBRs<-OrderByBackwardsRank(HaCaT.Corr.Spearman.Diff)
EGFR.Spearman.Diff.friends<-gene.names[RBRs[,'EGFR']]
ERBB2.Spearman.Diff.friends<-gene.names[RBRs[,'ERBB2']]
ERBB3.Spearman.Diff.friends<-gene.names[RBRs[,'ERBB3']]

RBRs<-OrderByBackwardsRank(HaCaT.Corr.bicor.Diff)
EGFR.bicor.Diff.friends<-gene.names[RBRs[,'EGFR']]
ERBB2.bicor.Diff.friends<-gene.names[RBRs[,'ERBB2']]
ERBB3.bicor.Diff.friends<-gene.names[RBRs[,'ERBB3']]

save(file='3genes-6ways-friends.Rda',list=ls()[grep('friends',ls())])
