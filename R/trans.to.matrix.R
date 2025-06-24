#' @title Converting a list of transitions to a matix
#'
#' @description
#' Given a sorted alphabet \eqn{A = \{a_1,a_2,...,a_{|A|}\}} and a list of named vectors \eqn{v^{(w)}},
#' where \eqn{w} is a context and
#' \deqn{v^{(w)}_i = I\{P[a_i|w]>0\}}
#' will return the matrix \eqn{M}, where
#' \deqn{M[w,w^{-1}_{-k+1}a_i]=v^{(w)}_i}
#'
#'
#' @param transitions List of size \eqn{|A|^k}, containing named binary vectors of size \eqn{|A|}.
#' @param A Alphabet of the VLMC.
#' @param sep Character separator used in the generation of the skeleton.
#'
#' @returns A transition matrix. See \link{skeleton} for more details.
#'
#' @export
trans.to.matrix = function(transitions, A, sep){
  M = matrix(0, nrow = length(transitions), ncol = length(transitions),
             dimnames = list(names(transitions), names(transitions)))

  d = length(strsplit(names(transitions)[1], sep)[[1]]) #espaço vazio no início
  for(w in names(transitions)){
    possible_transitions = paste(
      paste(strsplit(w, sep)[[1]][2:d], collapse = sep), A, sep = sep
    )
    M[w,possible_transitions] = transitions[[w]]
  }
  return(M)
}
