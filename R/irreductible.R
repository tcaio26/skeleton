#' @title Irreductibility of a matrix
#'
#' @description
#' Tests the irreductibility of a Matrix.
#'
#' @param M A binary square matrix, such as the result of \link{skeleton}.
#'
#' @details
#' \eqn{M} is irreductible if
#' \deqn{\forall i,j \in [1,|A|^k], \exists n \in ℕ | M^n[i,j]>0}
#' In practice this can be tested with \eqn{n = 1,...,|A|^k-1}.\deqn{Q[i,j] = \mathcal{I}\{(\sum_{l=1}^{|A|^k}M^l)[i,j]>0\}}
#'
#' \eqn{M} is irreductible \eqn{\LeftRightArrow Q[i,j]>0\forall (i,j)\in [1,|A|^k]\times[1,|A|^k]}.
#'
#' If \eqn{M} has any null columns, it won't be irreductible.
#'
#' @returns Logical result for irreductibility.
#'
#' @export
irreductible = function(M){
  stopifnot(ncol(M)==nrow(M))
  if(any(apply(M, 2, function(J) sum(J)==0))) return(FALSE)
  Q = M
  M_prod = M
  k = ncol(M)

  for(i in 2:k){
    M_prod = M_prod%*%M
    Q = Q+M_prod
    if(all(Q>0)){
      print(paste("All transitions possible after", i, "steps or less"))
      return(TRUE)
    }
  }
  irreduc = all(Q>0)
  return(irreduc)
}
