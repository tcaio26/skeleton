#' @title Interval checking
#'
#' @return checks if *x* is in \eqn{(a,b),`[`a,b),(a,b`]`,`[a,b]`}, depending on the choices for *inc.L* and *inc.U*
#'
#' @param x x
#' @param a,b lower and upper bounds of interval
#' @param inc.L,inc.U Logical, should the interval be closed?
#'
#' @export

interval_check = function(x,a,b,inc.L=F,inc.U=F){
  L = ifelse(inc.L,x>=a,x>a)
  U = ifelse(inc.U,x<=b,x<b)
  return(L&U)
}
