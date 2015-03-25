source('backwards.rank.R')
message('loading')
load(file='corr_matrix.Rda')
#it is Rda matrix the correlation matrix: 
#> load('corr_matrix.Rda')
#> dim(pearsonny)
#[1] 26847 26847
#> class(pearsonny)
#[1] "data.frame"
message('calcucuclating uniform-based simple distances')
distance.by.best.friends<-distance.by.backwards.rank(pearsonny)
message('saving')
save(file='distance.by.best.friends.Rda',list=c('distance.by.best.friends'))
