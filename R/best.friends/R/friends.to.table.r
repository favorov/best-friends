#A. Suvorikova, V. Mukhina, V. Ramensky, A. Mironov, A. Favorov (c) 2014-2023
#'
#' friends.to.table
#' 
#' Converts the list of matrices returned by friends.test to a table
#' @param friends the result of friends.test
#' @param p.val.threshold the p-value threshold, the default is 1 (no filtering)
#' @value a table, one line per tag times split between adjacent-by-the-tag-rank collections
#' tag is the name of the tag
#' 