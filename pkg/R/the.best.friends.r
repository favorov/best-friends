#best-friends-of- library
#A. Favorov, V. Ramensky, A. Mironov 2014-2020

#'BestFriendsOf
##
#'We have someones to be a friend (entity) as columns and we have what is to be friends of (features) as rows.
#`matrix where in each column and row there is a value of how the raw specifically friendly to this column.
#'@param relation is the fetures*entities matrix of the relations between features ans entities
#'@return \code{data.frame} with 3 columns: feature index, friend entity index, uncorrected p-value for the pair
#'Best friend has the highest order, the worst has the lowest
#'@export
best.friend.of<-function(relation){
  dims<-dim(relation)
	feature.ranks<<-apply(relation,2, 
	      function(x){
	        1-data.table::frank(x,ties.method='average')/dims[1]
	      }
	    )
	#we applied ranking column-by-column (entity-by-entity); A's were ranked in each row,
	fp<-as.data.frame(t(apply(feature.ranks,1,friend_and_p_value)))
	colnames(fp)<-c("friend","pval")
	cbind(feature=c(1:dims[1]),fp)
}

friend_and_p_value<-function(x) { #x is anumeric vector
	#we are to find the difference of the best and the next; 
	#we know that all values are between 0 and 1
	#best and next are the smallest and the next
	bestv <- 1.1;
	nextv <- 1.1;
	n<-length(x);
	bestind<-n+1;
	if (n<2) return(c(1,0));
	for(i in c(1:n)) {
		if (x[i]<bestv) {
			nextv<-bestv;
			bestv<-x[i];
			bestind<-i;
			next;
		};
		#if we are here, x[i] >= best
		if (x[i]<nextv) {
			nextv=x[i];
		};
	}
	return (c(bestind,(1-nextv+bestv)^n,bestv,nextv,n));
}


if(0) {
cppFunction("
	double bestminusnext(NumericVector x) {
	//we are to find the difference of the best and the next; 
	//we know that all values are between 0 and 1
	//best and next are the smallest and the next
	int n = x.size(), i; 
	double best = 1.1, next=1.1;
	if (n<2) return 0;
	for(i = 0; i < n; i++) {
		if (x[i]<best) {
			next=best;
			best=x[i];
			continue;
		};
		//if we are here, x[i] >= best
		if (x[i]<next) {
			next=x[i];
		};
		return next-best;
	}")
}
