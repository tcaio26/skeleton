#' @title Open and closed interval checking
#'
#' @returns checks if *x* is in the constructed interval.
#'
#' @param x x
#' @param a,b Lower and Upper bounds of interval
#' @param inc.L,inc.U Booleans, for having closed lower and upper bounds:
#'
#'    - **inc.L** = TRUE \eqn{\Rightarrow \;[a,}
#'    - **inc.U** = TRUE \eqn{\Rightarrow \;\;,b]}
#' @examples
#' interval_check(2,2,5)
#' interval_check(3,3,6,inc.L=T)
#'
#' @export

interval.check = function(x,a,b,inc.L=F,inc.U=F){
  L = ifelse(inc.L,x>=a,x>a)
  U = ifelse(inc.U,x<=b,x<b)
  return(L&U)
}
