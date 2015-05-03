/*
"corr.min1.prenatal.log": gene index to gene id mapping for all BrainSpan genes

$ head -5 corr.min1.prenatal.log
# idx   gene    non_zero_cnt
0       ENSG00000000003 237
1       ENSG00000000005 137
2       ENSG00000000419 237
3       ENSG00000000457 237

-- format: idx, gene_id, non_zero_cnt (non-zero expression value counts)
-- 52,376 lines, 1 lines per gene, non_zero_cnt == 0..237
-- 48,582 genes have non_zero_cnt>0, implicated in files below


"corr.min1.prenatal.nr.tsv": non-redundant sorted correlation matrix for 48,582 genes

$ cut -f2-4 corr.min1.prenatal.tsv | perl -ane 'print if $F[1]>$F[0];' 1>corr.min1.prenatal.nr.tsv &

$ head -5 corr.min1.prenatal.nr.tsv
0       2929    0.912
0       1941    0.905
0       6126    0.896
0       7765    0.896
0       11897   0.896

-- format: idx1, idx2, corr; 
-- non-redundant: only Cij values with i<j are given, 1,180,081,071 lines (=48,582*48,581/2)
-- sorted: for each idx1, sorted by descending correl (=col.3)
*/

#include <string>
#include <vector>
#include <iostream>
#include <iomanip>

using std::string;
using std::vector;
using std::ostream;
using std::cout;
using std::endl;


template <class T> class PairwiseMatrix : public vector < vector <T> >
{
private:
	PairwiseMatrix (const PairwiseMatrix &);
	//copy constructor is forbidden
	PairwiseMatrix & operator= (const PairwiseMatrix &);
	//copy operator is forbidden
public:
	PairwiseMatrix(){};
	vector<string> names;
	void setsize(unsigned long nsize)
	{
		this->clear();
		for (unsigned long i=0;i<nsize;i++)
			this->push_back(* new vector<T> (nsize));
	}
};

template <class T>
ostream &operator<<( ostream &output, const PairwiseMatrix<T> &PM )
{ 
	 output<<"Size:"<<PM[0].size();
	 return output;            
}

int main ()
{
	PairwiseMatrix<int> A;
	A.setsize(5);
	cout<<A;
}
