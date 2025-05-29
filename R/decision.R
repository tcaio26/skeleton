#' @title Decision
#'
#' @param n1, n2 Booleans. n1: (w$children[[1]]$n > Nmin)
#' @param d1, d2 Numerics. d1: w$children[[1]]$dom
#'
#' @description
#' Internal function to decide wether or not a node's children should be pruned.
#' A value of TRUE means the node's children will be pruned. Internal use only.
#'
#' @noRd

decision = function(n1, n2, d1, d2){
  c1 = n1*d1 #w0 n>Nmin e dom!=0
  c2 = n2*d2 #w1 n>Nmin e dom!=0

  if(c1+c2 + c1*c2== 0) return(TRUE) # No significant prohibited transitions.
  else if(c1 == c2) return(TRUE) # Same significant prohibited transition. This should not happen.
  else return(FALSE)
}
