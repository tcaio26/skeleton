#' @title Wrapper for pruning
#'
#' @param parent Node to be evaluated.
#' @param Nmin Cutoff, see \link{skeleton} for details.
#'
#' @description Internal wrapper, should not be used.
#'
#' @keywords internal

should.you.cut = function(parent, Nmin){
  if(isLeaf(parent)) return(FALSE) #no children to prune
  if(all(sapply(parent$children, function(node) node$n < Nmin))) return(TRUE) #no significant leaves
  if(any(!sapply(parent$children, isLeaf))) return(FALSE) #shouldn't happen, but avoid cutting non-leaves
  valid_leaves = parent$children[sapply(parent$children, function(node) node$n >= Nmin)]
  if(all(
    sapply(valid_leaves, function(leaf) all(parent$transitions==leaf$transitions))
  )) return(TRUE) #no information added by leaves
  return(FALSE)
}
