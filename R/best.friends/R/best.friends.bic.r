#'
#' best.friends.bic
#' 
#' Find Tags that are best friends to Collections
#' 
#' @param attention original attention matrix
#' @param prior.to.have.friends The prior for a tag is important enough to have friendly collections.
#' @param best.no The maximal number of friends for a tag, the default is \code{1}, 
#' i.e. we look for the best friends only. The string "all" means "all friends", 
#' i.e. the maximal number of friends in the number of collections
#' The value $n$ means that we filter out a tag from the results is it has more 
#' than $n$ friendly collections and we do not tell it from the no-friends case.
#' @return A data.frame, rows are pairs of tags and collections that are markers and best friends 
#' friends.
#' @examples 
#' attention <- matrix(c(10,6,7,8,9,
#'                 9,10,6,7,8,
#'                 8,9,10,6,7,
#'                 7,8,9,10,6,
#'                 6,7,8,9,10,
#'                 20,0,0,0,0), 
#'                 nrow=6, ncol=5, byrow=TRUE)
#' attention
#' best.friends.bic(attention, prior.to.have.friends=0.5)
#' best.friends.bic(attention, prior.to.have.friends=0.001)
#' @export
#' 
best.friends.bic <- function(attention=NULL, prior.to.have.friends=-1, best.no = 1) {
  #parameter checks
  if (is.na(best.no) || best.no == "all" ||
      best.no == "al" || best.no == "a" ||
      is.null(best.no) || !as.logical(best.no)){
    best.no <- ncol(attention)
  }
  if (best.no < 1 || best.no > nrow(attention)) {
    stop("best.no must be at between 1 and the number of tags.")
  }
  if (prior.to.have.friends < 0 || prior.to.have.friends >1){
    stop("best.friends.bic requires the prior.to.have.friends value to be explicitely provided and to be a prior.")
  }
  #add names to attention matrix rows if necessary
  if(is.null(dimnames(attention)[[1]])) {
    rownames(attention) <- seq(nrow(attention))
  }
  #add names to attention matrix cols if necessary
  if(is.null(dimnames(attention)[[2]])) {
    colnames(attention) <- seq(ncol(attention))
  }
  all_ranks <- tag.int.ranks(attention)
  
  tags.no <- dim(attention)[1]

  all_friends <- apply(all_ranks, 1,
                       function(x) best.step.fit.bic(
                         x, tags.no = tags.no,
                         prior.to.have.friends=prior.to.have.friends)
                       )

  #here, we filer out the tags uniform model wins for
  #we also filter to match
  #best.no parameter here,
  #best friends are cases where a tag is a marker in 
  #no more than best.no collections

  #vapply is recommended by BioCheck as safer than sapply

  best_friends <- all_friends[vapply(all_friends, function(x) {
    x$population.on.left>0 && x$population.on.left <= best.no
  },logical(1))]
  
  if(!length(best_friends)){
    return(data.frame(tag=character(), collection=character()))
  } #tf no tag passed best test, return empty frame rather than NULL

  res_pre <- lapply(seq_along(best_friends), function(x) {
    data.frame(
       tag=names(best_friends[x]),
       collection=colnames(all_ranks)[best_friends[[x]]$collections.on.left]
     )})

  res <- do.call(rbind, res_pre)

  res
}
