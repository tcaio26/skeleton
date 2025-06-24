#' @title Extracts transitions from a skeleton and expands them to order k.
#'
#' @param skel A skeleton tree, such as the result of \link{sculptskeleton}.
#' @param A Alphabet of possible symbols.
#' @param sep Character separator for the skeleton contexts.
#'
#' @description
#' Extracts transitions from a skeleton object and expands contexts of order less than k to all possible pasts of order k. Internal use.
#'
#' @returns A list, where each element corresponds to the transition possibilities from past w (name) to each symbol u in A.
#'
#' @keywords internal
#'
#' @export
expand.transitions = function(skel, A, sep){
  contexts = Traverse(skeleton, filterFun = isLeaf)
  d = max(vapply(contexts, function(leaf) length(strsplit(leaf$context, sep)[[1]]), FUN.VALUE = integer(1)))
  transitions = lapply(contexts, function(leaf) leaf$transitions)
  names(transitions) = vapply(contexts, function(leaf) leaf$context, FUN.VALUE = character(1))
  pasts = apply(expand.grid(replicate(d, A, simplify = FALSE)), 1, paste0, collapse = sep)
  full_transitions = probabilities = replicate(length(A)^d, rep(0,length(A)), simplify = FALSE)
  for(w in 1:length(pasts)){
    full_transitions[[w]]=
      transitions[[getMaxContext(names(transitions),paste0(pasts[w],sep))]]
  }
  names(full_transitions)=pasts
  return(full_transitions)
}
