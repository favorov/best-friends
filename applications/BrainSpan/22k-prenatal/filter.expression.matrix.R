library('data.table')
express<-fread('expression_matrix.csv')
express[,V1:=NULL]
samples<-fread('columns_metadata.csv',header=TRUE)
genes<-fread('rows_metadata.csv',header=TRUE)
my.gene.ids<-fread('lists/22k-genes.txt',header=FALSE)[,V1]
the.22.k.genes.selector<-genes[,ensembl_gene_id] %in% my.gene.ids
gene.symbols<-genes[,gene_symbol][the.22.k.genes.selector]
gene.ensemble.ids<-genes[,ensembl_gene_id][the.22.k.genes.selector]
prenatal.ages<-unique(samples[,age])[1:13]
prenatal.sample.selector<-samples[,age] %in% prenatal.ages
prenatal.sample.ids<-samples$column_num[prenatal.sample.selector]
prenatal.22k.genes.expression<-as.data.frame(express)[the.22.k.genes.selector,prenatal.sample.selector]
rownames(prenatal.22k.genes.expression)<-gene.ensemble.ids
colnames(prenatal.22k.genes.expression)<-prenatal.sample.ids
save(list=c('prenatal.sample.ids','gene.symbols','gene.ensemble.ids','prenatal.22k.genes.expression'),file='prenatal.22.expression.Rda')
corr.22k.prenatal<-cor(t(prenatal.22k.genes.expression))
colnames(corr.22k.prenatal)<-gene.ensemble.ids
rownames(corr.22k.prenatal)<-gene.ensemble.ids
save(list=c('corr.22k.prenatal'),file='prenatal.22k.correlations.Rda')
q<-quantile(corr.22k.prenatal)
q25=q[['25%']]
q75=q[['75%']]
med=q[['50%']]
corr.22k.prenatal.filtered<-ifelse((corr.22k.prenatal<q75 & corr.22k.prenatal> q25),med,corr.22k.prenatal)
save(list=c('corr.22k.prenatal.filtered'),file='prenatal.22k.correlations.filtered.Rda')
