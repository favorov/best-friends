#include <Rcpp.h>
#include <math.h>
#include <queue>

using namespace std;
using namespace Rcpp;

///' Estimating the p-value for the putative entity that is the best friend of a feature from a vestor of normalised ranks of the feature for different entities.
///' The putative friend is that with the the best (minimal) rank in the list.
///' The corresponding p-value is the difference of the next and the best to power of the size of the list of entities
///' next here means the next by rank rather than by index in x
///'
///' @param x the list of ranks of the feature in different entities normalised to 0..1 -- in null, they are independently distributed
///' 
///' @return a vector of: index of the best and the p-value (it is the the_next_value-the_best_value^n)
// three /// is not to have it run through document() as far it is not exported
// [[Rcpp::export]]
NumericVector rank_diff_and_p_for_the_best(NumericVector x) {
	//we are to find the difference of the best and the next; 
	//we know that all values are between 0 and 1
	//best and next are the smallest and the next
	//returns pair (R vector) of 1-based coordinate of the best and the p-value
	//@param x is where we look for the best and the next
	//
	int len = x.size(), i, ncollections=len;
	double bestind=1.;
	double best = 1.1, next=1.1;
	if (len==0) {
		return (NumericVector::create()); //empty vector -- empty list
	}
	for(i = 0; i < len; i++) {
		if (isnan(x[i])) {
			ncollections--; //NA comes from the neglected diagonal
			continue;
		}
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
	if (ncollections==1) {
		return (NumericVector::create(1,best)); //one member - we return 1-(1-val)**len = val for pval and 1 for coord
	}
	return (NumericVector::create(bestind,pow(1.-next+best,ncollections)));
}


typedef pair<double,int> rank_pair; //the first id the value for the compare to work by value rather than by index
typedef priority_queue<rank_pair, deque<rank_pair>,less<rank_pair> > pqt;
//less keeps the descending the order; the top() is the largest
//we want to keep the smallest n, so the queue has the maximal at the top() to compare and omit

///' Estimating the p-value for n entities that are potentially the best friend of a feature from a vector of normalised ranks of the feature for different entities.
///' The putative friends are that with the the n best (minimal) rank in the list.
///' The corresponding p-value for each of them is the difference of the next and the best to power of the size of the list of entities
///' next here means the next by rank rather than by index in x
///' @param x the list of ranks of the feature in different entities normalised to 0..1 -- in null, they are independently distributed
///' @param n the number of the best entities we look at; the default is to look at all (-1). In this case, we return a ranking of all entitites byt the feature's rank and the p-values to split to be the worst best friend 
///'
///' @return a vector of: index of the best and the p-value (it is the the_next_value-the_best_value^n)
// three /// is not to have it run through document() as far it is not exported
// [[Rcpp::export]]
List rank_diff_and_p_for_the_best_n(NumericVector x,int max_num_friends=-1) {
	//we are to find the p-values for difference between pairs of two sequential normalised ranks;
	//sequential means by value rather than by index in x 
	//normalised means the rank values are between 0 and 1
	//hay are provided by the x
	//we look only at n best ranks (values of the vector)
	//the best here means the least
	//if n=-1 (default) we look over all the values and return n difference p-values, including the 1-worst 
	//return list of n elements. Each element is a pair of 1-based coordinate in x and the corresponding p-value
	//p-value is (next_value - this_value)**len
	int len = x.size(), i, ncollections=len;
	List Res=List::create();
	if (max_num_friends<0) {max_num_friends=len-1;}
	//proceed default value of max_num_friends
	max_num_friends=min(max_num_friends,len-1);
	//proceed strange value of max_num_friends
	if (max_num_friends==0) {
		return (Res); //empty vector -- emplty list
	}
  //here: len==ncollections=size()
  //max_num_friends is in [1:len-1]

	pqt sorter;
	unsigned int sortercapacity = unsigned(max_num_friends+1);
	//we need one more slots than max_num_friends to know the next for the max_num_friends-th; 
	for(i = 0; i < len; i++) {
		if (isnan(x[i])) {
			ncollections--; //NA comes from the neglected diagonal
			continue;
		}
		double val=x[i];
		if ( sorter.size() < sortercapacity) 
			sorter.push(rank_pair(val,i+1)); //coord is 1-based
			//we just put it in the queue, it is not full yet
		else {
			if (sorter.top().first > val) { 
				//if our top (the larges we have in heap) is larger than the val, let's add the val and pop the top away
				// we want smallest
				sorter.pop();
				sorter.push(rank_pair(val, i+1)); //coord is 1-based
			}
		}
	};
	if (ncollections==1) {
	  Res.push_front(NumericVector::create(1,x[0]));  //one member - we return 1-(1-val)**len = val for pval and 1 for coord
	  return (Res);
	}
	//we have the next for max_num_friends-th on top, 
	//it is the largest, and we get it
    double next=sorter.top().first;
	sorter.pop();
	while (!sorter.empty()) {
	  rank_pair current=sorter.top();
	  Res.push_front(NumericVector::create(current.second,pow(1.-next+current.first,ncollections)));
	  next=current.first;
	  sorter.pop();
	}
	return (Res);
}

