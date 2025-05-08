#'
#' friends.test
#' 
#' We have two sets:T (rows) and C (columns) and 
#' A real matrix A(t,c) that describes the strength of association 
#' between each t and each c; t is an element of T and c is an element of C. 
#' For each t we want to identify whether it is significantly more 
#' relevant for some c's than for the remaining c's.
#' If it does, those c for which the t is relevant, 
#' are the t's friend. And, the t is the c's marker.
#' 
#' @param A original association matrix
#' @param threshold The adjusted p-value threshold for KS test for 
#' non-uniformity of ranks.
#' @param p.adjust.method Multiple testing correction method, 
#' see \link[stats]{p.adjust}.
#' @param max.friends.n The maximal number of friends for a marker, the default 
#' is \code{'all'}, that is an alias for #of columns in A. 
#' The string "all" means "all friends", i.e. we do not filter by this parameter 
#' value. A value $n$ means that we filter out a row if it has more 
#' than $n$ friendly columns. 1 means we look only for unuque (best) friends.

#' @param uniform.max The maximum of the uniform distribution of the ranks we 
#' fit the null model,it can be the maximal possible rank that is common for all 
#' rows and equals the number of rows \code{'c'} or the maximal observed rank 
#' for the row we test now, \code{'m'}.
#' @return A data.frame, rows are pairs of markers 
#' friends.
#' @importFrom stats p.adjust
#' @examples 
#' A <- matrix(c(10,6,7,8,9,
#'                 9,10,6,7,8,
#'                 8,9,10,6,7,
#'                 7,8,9,10,6,
#'                 6,7,8,9,10,
#'                 20,0,0,0,0), 
#'                 nrow=6, ncol=5, byrow=TRUE)
#' A
#' friends.test(A, threshold = .05)
#' friends.test(A, threshold = .0001)
#' 
#' @export
#' 
friends.test <- function(A=NULL, threshold = 0.05, 
                         p.adjust.method = "BH", max.friends.n = 'all') {
  #parameter checks
  if (is.na(max.friends.n) || max.friends.n == "all" ||
      max.friends.n == "al" || max.friends.n == "a" ||
      is.null(max.friends.n) || !as.logical(max.friends.n)){
    max.friends.n <- ncol(A)
  }
  if (max.friends.n < 1 || max.friends.n > ncol(A)) {
    stop("max.friends.n must be between 1 and the number of collections.")
  }
  if(threshold < 0 || threshold > 1) {
    stop("threshold must be between 0 and 1.")
  }
  #add names to A matrix rows if necessary
  if(is.null(dimnames(A)[[1]])) {
    rownames(A) <- seq(nrow(A))
  }
  #add names to A matrix cols if necessary
  if(is.null(dimnames(A)[[2]])) {
    colnames(A) <- seq(ncol(A))
  }
  all_ranks <- tag.int.ranks(A)
  
  adj_nunif_pval <- p.adjust(
      apply(all_ranks, 1, unif.ks.test),
      method = p.adjust.method)

  marker_ranks <- all_ranks[adj_nunif_pval<=threshold,,drop=FALSE]

  if(nrow(marker_ranks) == 0) {
    #message("No tags with non-uniform ranks found for given threshold.")
    return(data.frame(tag=character(), collection=character()))
  }


  #find friends that make tag ranks non-uniform
  tags.no <- dim(A)[1]

  all_friends <- apply(marker_ranks, 1,
                       function(x) best.step.fit(x, tags.no = tags.no))
  #we filter to match
  #max.friends.n parameter here,
  #best friends are cases where a tag is a marker in 
  #no more than max.friends.n collections
  #vapply is recommended by BioCheck as safer than sapply

  best_friends <- all_friends[vapply(all_friends, function(x) {
    x$population.on.left <= max.friends.n
  },logical(1))]
  
  if(!length(best_friends)){
    return(data.frame(tag=character(), collection=character()))
  } #if no tag passed best test, return empty frame rather than NULL

  res_pre <- lapply(seq_along(best_friends), function(x) {
    data.frame(
       marker=names(best_friends[x]),
       friend=colnames(marker_ranks)[best_friends[[x]]$collections.on.left],
       marker.rank=
     )})

  res <- do.call(rbind, res_pre)

  res
}
