#'
#' best.friends
#' 
#' Find Tags that are best friends to Collections
#' 
#' @param attention original attention matrix
#' @param threshold The adjusted p-value threshold for KS test for 
#' non-uniformity of ranks.
#' @param p.adjust.method Multiple testing correction method, see \link[stats]{p.adjust}.
#' @param best.no The maximum number of friends for a tag, the default is \code{1}, 
#' i.e. the best friend. The string "all" means "all friends".
#' @return A data those of tags and collections that are markers and best friends 
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
#' res <- best.friends(attention, threshold = 1)
#' @export
#' 
best.friends <- function(attention=NULL, threshold = 0.05, 
                         p.adjust.method = "BH", best.no = 1) {
  #parameter checks
  if (is.na(best.no) || best.no == "all" || 
      is.null(best.no) || !as.logical(best.no)){
    best.no > nrow(attention)
  }
  if(best.no < 1 || best.no > nrow(attention)) {
    stop("best.no must be at between 1 and the number of tags.")
  }
  if(threshold < 0 || threshold > 1) {
    stop("threshold must be between 0 and 1.")
  }
  if(is.null(dimnames(attention))) {
    dimnames(attention) <- list(1:nrow(attention), 1:ncol(attention))
  }
  #find tags with non-uniform ranks
  all_ranks <- tag.int.ranks(attention)
  adj_nunif_pval <- p.adjust(
      apply(all_ranks, 1, unif.ks.test),
      method = p.adjust.method)

  marker_ranks <- all_ranks[adj_nunif_pval<=threshold,,drop=FALSE]

  if(nrow(marker_ranks) == 0) {
    message("No tags with non-uniform ranks found for given threshold.")
    return(data.frame(tag=character(), collection=character()))
  }


  #find friends that make tag ranks non-uniform
  tag_count <- dim(attention)[1]

  all_friends <- apply(marker_ranks, 1,
                       function(x) best.step.fit(x, tags.no = tag_count))

  #best friends are cases where a tag is a marker in only best.no collections
  best_friends <- all_friends[sapply(all_friends, function(x) {
    x$population.on.left <= best.no
    })]

  res_pre <- lapply(seq_along(best_friends), function(x) {
    data.frame(
       tag=names(best_friends[x]),
       collection=colnames(marker_ranks)[best_friends[[x]]$collections.on.left]
     )})

  res <- do.call(rbind, res_pre)

  res
}
