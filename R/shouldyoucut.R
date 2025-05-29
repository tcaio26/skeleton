#' @title Wrapper for pruning
#'
#' @param nodo node
#' @param par name of the parameter to base cutting
#' @param Nmin cutoff, see \link{generate_skeleton} for details
#'
#' @description Internal wrapper, should not be used
#'
#' @keywords internal
#'
#' @noRd

shouldyoucut = function(nodo, par, Nmin){
  if(isLeaf(nodo)) return(FALSE)
  if(!all(sapply(nodo$children, isLeaf))) return(FALSE)
  n1 = (nodo$children[[1]][['n']]) >= Nmin
  n2 = (nodo$children[[2]][['n']]) >= Nmin
  d1 = nodo$children[[1]][[par]]
  d2 = nodo$children[[2]][[par]]
  decision(n1,n2,d1,d2)
}
