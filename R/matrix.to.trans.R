#' @title Converting a matrix of transitions to a list
#'
#' @description
#' Does the opposite of \link{trans.to.matrix}.
#'
#'
#' @param M Matrix of transitions, with sequences of elements of *A* of length k (order of the skeleton) for its rownames and colnames, where \deqn{M[w,w^{-1}_{-k+1}u]=I\{P[u|w]>0\}}
#' @param A Alphabet of the VLMC.
#' @param sep Character separator used in the generation of the skeleton.
#'
#' @returns A list of named transition vectors.
#'
#' @export
matrix.to.trans = function(M, A, sep){
  a=length(A)
  d=length(strsplit(rownames(M)[1], sep)[[1]])
  probabilities = replicate(a^d, rep(0,a), simplify = FALSE)
  names(probabilities) = rownames(M)
  for(w in rownames(M)){
    possible_transitions = paste(
      paste(strsplit(w, sep)[[1]][2:d], collapse = sep), A, sep = sep
    )
    probabilities[[w]] = as.vector(unname(M[w,possible_transitions]))
  }
}
