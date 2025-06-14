#' @title Converting integer string to a numeric vector.
#'
#' @return Vector of integers.
#'
#' @description
#' Efficient function made for the skeleton package, might be useful for other cases.
#'
#'
#' @examples
#' string_to_vec('1001')
#'
#'
#' @param string String comprised of positive integers, including 0. Other values won't work.
#'
#' @export

string_to_vec = function(string){
  as.numeric(charToRaw(string))-48L
}


# teste = vec_to_string(sample(c(0,1),1e5,T))
# teste2 = vec_to_string(sample(c(0,1),1e3,T))
# rbenchmark::benchmark(string_to_vec(teste), replications = 1e5) #46 sec
# rbenchmark::benchmark(string_to_vec(teste2), replications = 1e5) #0.7 sec
