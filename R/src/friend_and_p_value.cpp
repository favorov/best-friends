#include <Rcpp.h>
#include <math.h>
#include <queue>

using namespace Rcpp;
//' Identifying the putative friend entity for the vector of ranks of the feature in different entities.
//'
//' @param x the list of ranks normalised to 0..1 -- in null, they are independently distributed
//' 
//' @return a vector of: index of the best, the best rank-the next rank and the p-value (it is the difference^n)
// [[Rcpp::export]]
NumericVector rank_diff_and_p_for_the_best(NumericVector x) {
	//we are to find the difference of the best and the next; 
	//we know that all values are between 0 and 1
	//best and next are the smallest and the next
	int len = x.size(), i;
	double bestind=1.;
	double best = 1.1, next=1.1;
	if (len==0) {
		return (NumericVector::create());
	}
	if (len==1) {
		return (NumericVector::create(1,1));
	}
	for(i = 0; i < len; i++) {
		if (x[i]<best) {
			next=best;
			best=x[i];
			bestind=i+1;
			//return it as 1- based
			continue;
		}
	//if we are here, x[i] >= best
		if (x[i]<next) {
			next=x[i];
		}
	};
	return (NumericVector::create(bestind,pow(1.-next+best,n)));
}


List rank_diff_and_p_for_the_best_n(NumericVector x,int n=-1) {
	//we are to find the difference of the best and the next; 
	//we know that all values are between 0 and 1
	//best and next are the smallest and the next
	int len = x.size(), i;
	List Res=List::create()
	if (n==-1) {n=len}
	if (n<size) {
		return (List::create());
	}
	if (len==1) {
		return (List::create(NumericVector::create(1,1)));
	}
	for(i = 0; i < n; i++) {
		if (x[i]<best) {
			next=best;
			best=x[i];
			bestind=i+1;
			//return it as 1- based
			continue;
		}
	//if we are here, x[i] >= best
		if (x[i]<next) {
			next=x[i];
		}
	};
	return (NumericVector::create(bestind,pow(1.-next+best,n)));
}

