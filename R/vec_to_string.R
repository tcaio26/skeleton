#' @title Converting integer numeric vector to binary string.
#'
#' @return String of numeric characters.
#'
#' @description
#' Efficient function made for the skeleton package, might be useful for other cases.
#'
#' @examples
#' vec_to_string(c(0,1,1,0))
#'
#'
#' @param vector Vector of positive integers, including 0. Other values won't work.
#'
#' @export

vec_to_string = function(vector){
  rawToChar(as.raw(vector+48L))
}

# teste = sample(c(0,1),1e5,T)
# rbenchmark::benchmark(vec_to_string(teste), replications = 1e5) #10.2 sec
