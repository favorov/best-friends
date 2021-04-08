## ----source, echo=FALSE-------------------------------------------------------
if (!suppressWarnings(require('best.friends.of')))
{
	if (!suppressWarnings(require('devtools')))
	{
		source("http://bioconductor.org/biocLite.R")
		biocLite("devtools")
		library("devtools")
	}
	install_github('favorov/gene-friends/pkg/R')
	library(best.friends.of)
}
data.digits=2
p.val.digits=4

## -----------------------------------------------------------------------------
genes<-10
gene.names<-LETTERS[seq( from = 1, to = genes )]
regulation=matrix(
	c(0.2, 0.2, 0.2, 0.2, 0.25, rep(0.2,genes-5),
	  rep(1, genes),
		rep(1, genes),
		rep(1, genes),
		rep(1, genes),
		rep(1, genes),
		rep(1, genes),
		rep(1, genes),
		rep(1, genes),
		rep(1, genes)
    ),
	ncol=10,byrow=FALSE
)
TF.names<-c('TF1','TF2','TF3','TF4','TF5','TF6','TF7','TF8','TF9','TF10')
rownames(regulation)<-gene.names
colnames(regulation)<-TF.names

## -----------------------------------------------------------------------------
noquote(format(regulation,digits = data.digits))

## -----------------------------------------------------------------------------
friends<-best.friends.of.features(regulation)
noquote(format(friends,digits = p.val.digits))

## -----------------------------------------------------------------------------
regulation[,2:ncol(regulation)]<-rep(runif(nrow(regulation)*(ncol(regulation)-1),0.5,1.5))
noquote(format(regulation,digits = data.digits))

## -----------------------------------------------------------------------------
friends<-best.friends.of.features(regulation)
noquote(format(friends,digits = p.val.digits))


## -----------------------------------------------------------------------------
mmm<-matrix(ncol = 3,nrow=7)
rownames(mmm)<-c("Feature1","Feature2","Feature3","Feature4","Feature5","Feature6","Feature7")
colnames(mmm)<-c("Entity1","Entity2","Entity3")
mmm[1,]<-c(0.2,0.1,0.3)
mmm[2,]<-c(2,3,1)
mmm[3,]<-c(0.1, 6 ,0.05 )
mmm[4,]<-c(0.4, 1 ,3 )
mmm[5,]<-c(0.25,0.15 ,0.3 )
mmm[6,]<-c(2,0.9 ,0.4 )
mmm[7,]<-c(.7,0.1 ,11 )

## -----------------------------------------------------------------------------
connections<-matrix(nrow = 10,ncol=10,0)
names<-c('red','purple','blue','orange','green.1','green.2','green.3','green.4','green.5','green.6')
rownames(connections)<-names
colnames(connections)<-names
connections[,'red']<-5
connections['red',]<-5
connections['blue','orange']<-3
connections['orange','blue']<-3
connections['purple','blue']<-1
connections['blue','purple']<-1
diag(connections)=NA

