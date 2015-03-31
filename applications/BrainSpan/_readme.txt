3/18/2015 

"min1.prenatal": gene expression in 237 prenatal brain experiments, 
48,582 genes (out of total 52,376) with >0 expression in at least 1 experiment


Files: 

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

"idx_gene_annot.tsv": annotation for 48,582 genes, columns 1-3 are from "corr.min1.prenatal.log"

"idx_go_annot.tsv": GO terms for 48,582 genes, columns 1-3 are from "corr.min1.prenatal.log"




"tests": test results for 9 lincRNA

  "tests/ENSnnnnnnn.gene_annot.tsv": all 48,582 genes sorted by their _ascending reverse rank_ to query (ENSnnnnnnn)

-- line 1: query gene itself, correlation == 1.000, reverse and forward rank == 0
-- first columns: rev_rank, fwd_rank, corr, gene, non_zero_cnt, ...


