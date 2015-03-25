source('backwards.rank.R')
message('loading')
load(file='corr_matrix.Rda')
#it is Rda matrix the correlation matrix: 
#> load('corr_matrix.Rda')
#> dim(pearsonny)
#[1] 26847 26847
#> class(pearsonny)
#[1] "data.frame"
source('sort.by.backwards.rank.R')
message('calcucuclating gene lists')
genelists<-sort.by.backwards.rank(pearsonny)
genelists[1,]
save(file='genlists.Rda',list=c('genelists'))
