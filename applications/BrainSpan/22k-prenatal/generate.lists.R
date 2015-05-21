load(file = 'prenatal.22k.correlations.Rda')
load(file = 'prenatal.22k.expression.Rda')
load(file = 'prenatal.22k.order.Rda')
load(file = 'prenatal.22k.order.filtered.Rda')
our.gene.id<-grep('233723',gene.ensemble.ids)
list.1.length<-as.integer(length(gene.ensemble.ids)/100)
list.5.length<-as.integer(length(gene.ensemble.ids)/20)
friends.LINC01122.RBR.1<-gene.ensemble.ids[order.22k.prenatal[,our.gene.id][1:list.1.length]]
friends.LINC01122.RBR.5<-gene.ensemble.ids[order.22k.prenatal[,our.gene.id][1:list.5.length]]
friends.LINC01122.RBR.filtered.1<-gene.ensemble.ids[order.22k.prenatal.filtered[,our.gene.id][1:list.1.length]]
friends.LINC01122.RBR.filtered.5<-gene.ensemble.ids[order.22k.prenatal.filtered[,our.gene.id][1:list.5.length]]
friends.LINC01122.naive.1<-gene.ensemble.ids[order(corr.22k.prenatal[our.gene.id,],decreasing=TRUE)[1:list.1.length]]
friends.LINC01122.naive.5<-gene.ensemble.ids[order(corr.22k.prenatal[our.gene.id,],decreasing=TRUE)[1:list.5.length]]
listnames<-ls()[grep('friends.LINC01122',ls())]
lapply(listnames,function(name) {sink(paste0(name,'.txt'));cat(eval(as.name(name)),sep="\n");sink();NULL}
