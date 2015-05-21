sink('GWAS.RBR.filtered.pairs.txt')
for (gwas.gene in gwas.genes) {
  gwas.gene.no<-which(gwas.gene==gene.symbols)
  friends<-gene.symbols[order.22k.prenatal.filtered[,gwas.gene.no][1:list.5.length]]
  gwas.genes.by.me<-setdiff(gwas.genes,c(gwas.gene))
  my.gwas.friends<-gwas.genes.by.me[gwas.genes.by.me %in% friends]
  cat(paste(gwas.gene,my.gwas.friends,sep=' -> '),sep='\n')
}
sink()load(file = 'prenatal.22k.correlations.Rda')
load(file = 'prenatal.22k.expression.Rda')
load(file = 'prenatal.22k.order.Rda')
load(file = 'prenatal.22k.order.filtered.Rda')
library('data.table')
gwas.genes<-fread('lists/gwas-genes.txt',header = FALSE)[,V1]
gwas.genes<-gwas.genes[gwas.genes %in% gene.symbols]
list.5.length<-as.integer(length(gene.ensemble.ids)/20)

sink('GWAS.RBR.pairs.txt')
for (gwas.gene in gwas.genes) {
  gwas.gene.no<-which(gwas.gene==gene.symbols)
  friends<-gene.symbols[order.22k.prenatal[,gwas.gene.no][1:list.5.length]]
  gwas.genes.by.me<-setdiff(gwas.genes,c(gwas.gene))
  my.gwas.friends<-gwas.genes.by.me[gwas.genes.by.me %in% friends]
  cat(paste(gwas.gene,my.gwas.friends,sep=' -> '),sep='\n')
}
sink()

sink('GWAS.RBR.filtered.pairs.txt')
for (gwas.gene in gwas.genes) {
  gwas.gene.no<-which(gwas.gene==gene.symbols)
  friends<-gene.symbols[order.22k.prenatal.filtered[,gwas.gene.no][1:list.5.length]]
  gwas.genes.by.me<-setdiff(gwas.genes,c(gwas.gene))
  my.gwas.friends<-gwas.genes.by.me[gwas.genes.by.me %in% friends]
  cat(paste(gwas.gene,my.gwas.friends,sep=' -> '),sep='\n')
}
sink()

sink('GWAS.naive.pairs.txt')
for (gwas.gene in gwas.genes) {
  gwas.gene.no<-which(gwas.gene==gene.symbols)
  friends<-gene.symbols[order.22k.prenatal.filtered[,gwas.gene.no][1:list.5.length]]
  gwas.genes.by.me<-setdiff(gwas.genes,c(gwas.gene))
  my.gwas.friends<-gwas.genes.by.me[gwas.genes.by.me %in% friends]
  cat(paste(gwas.gene,my.gwas.friends,sep=' -> '),sep='\n')
}
sink()

