#'
#' best.friends
#' 
#' Find Tags that are best friends to Collections
#' 
#' @param attention original attention matrix
#' @param threshold The adjusted p-value threshold for KS test for 
#' non-uniformity of ranks.
#' @param p.adjust.method Multiple testing correction method, see \link[stats]{p.adjust}.
#' @param best.no The maximal number of friends for a tag, the default is \code{1}, 
#' i.e. the best friend. The string "all" means "all friends". 
#' The value $n$ means that we filter out a tag from the results is it has more 
#' than $n$ friends.
#' @return A data.frame, rows are pairs of tags and collections that are markers and best friends 
#' friends.
#' @importFrom stats p.adjust
#' @examples 
#' attention <- matrix(c(10,6,7,8,9,
#'                 9,10,6,7,8,
#'                 8,9,10,6,7,
#'                 7,8,9,10,6,
#'                 6,7,8,9,10,
#'                 20,0,0,0,0), 
#'                 nrow=6, ncol=5, byrow=TRUE)
#' attention
#' best.friends(attention, threshold = .05)
#' best.friends(attention, threshold = .0001)
#' 
#' @export
#' 
best.friends <- function(attention=NULL, threshold = 0.05, 
                         p.adjust.method = "BH", best.no = 1) {
  #parameter checks
  if (is.na(best.no) || best.no == "all" ||
      best.no == "al" || best.no == "a" ||
      is.null(best.no) || !as.logical(best.no)){
    best.no <- nrow(attention)
  }
  if(best.no < 1 || best.no > nrow(attention)) {
    stop("best.no must be at between 1 and the number of tags.")
  }
  if(threshold < 0 || threshold > 1) {
    stop("threshold must be between 0 and 1.")
  }
  #add names to attention matrix rows if necessary
  if(is.null(dimnames(attention)[[1]])) {
    rownames(attention) <- seq(nrow(attention))
  }
  #add names to attention matrix cols if necessary
  if(is.null(dimnames(attention)[[2]])) {
    colnames(attention) <- seq(ncol(attention))
  }
  #find tags with non-uniform ranks
  all_ranks <- tag.int.ranks(attention)
  adj_nunif_pval <- p.adjust(
      apply(all_ranks, 1, unif.ks.test),
      method = p.adjust.method)

  marker_ranks <- all_ranks[adj_nunif_pval<=threshold,,drop=FALSE]

  if(nrow(marker_ranks) == 0) {
    #message("No tags with non-uniform ranks found for given threshold.")
    return(data.frame(tag=character(), collection=character()))
  }


  #find friends that make tag ranks non-uniform
  tags.no <- dim(attention)[1]

  all_friends <- apply(marker_ranks, 1,
                       function(x) best.step.fit(x, tags.no = tags.no))
  #we filter to match
  #best.no parameter here,
  #best friends are cases where a tag is a marker in 
  #no more than best.no collections
  #vapply is recommended by BioCheck as safer than sapply

  best_friends <- all_friends[vapply(all_friends, function(x) {
    x$population.on.left <= best.no
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
