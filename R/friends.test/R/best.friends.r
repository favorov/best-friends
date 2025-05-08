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
#' @param max.friends.no The maximal number of friends for a tag, the default is \code{'all'}, that is an alias for #of collections 
#' i.e. we look for the best friends only. The string "all" means "all friends", 
#' i.e. the maximal number of friends in the number of collections
#' The value $n$ means that we filter out a tag from the results is it has more 
#' than $n$ friendly collections and we do not tell it from the no-friends case.
#' @return A data.frame, rows are pairs of tags and collections that are markers and best friends 
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
                         p.adjust.method = "BH", max.friends.no = 'all') {
  #parameter checks
  if (is.na(max.friends.no) || max.friends.no == "all" ||
      max.friends.no == "al" || max.friends.no == "a" ||
      is.null(max.friends.no) || !as.logical(max.friends.no)){
    max.friends.no <- ncol(A)
  }
  if (max.friends.no < 1 || max.friends.no > ncol(A)) {
    stop("max.friends.no must be between 1 and the number of collections.")
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
  #find tags with non-uniform ranks
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
  #max.friends.no parameter here,
  #best friends are cases where a tag is a marker in 
  #no more than max.friends.no collections
  #vapply is recommended by BioCheck as safer than sapply

  best_friends <- all_friends[vapply(all_friends, function(x) {
    x$population.on.left <= max.friends.no
  },logical(1))]
  
  if(!length(best_friends)){
    return(data.frame(tag=character(), collection=character()))
  } #uf no tag passed best test, return empty frame rather than NULL

  res_pre <- lapply(seq_along(best_friends), function(x) {
    data.frame(
       tag=names(best_friends[x]),
       collection=colnames(marker_ranks)[best_friends[[x]]$collections.on.left]
     )})

  res <- do.call(rbind, res_pre)

  res
}
