#' @title Finding positions after a pattern in a string.
#'
#' @return Vector of integer positions. If the pattern ends on the last character of the string, it won't be counted.
#'
#' @examples
#' string = 'banana'
#' match_str(string, 'b')
#' match_str(string, 'an')
#' match_str(string, 'ana')
#' match_str(string, 'nana')
#' match_str(string, '')
#'
#'
#' @param str String
#' @param p Pattern. An empty pattern returns 1:nchar(str).
#'
#' @export

match_str = function(str, p){
  if(p == '') return(1:nchar(str))
  pos = stringi::stri_locate_all_fixed(str, p, overlap = T)[[1]][, "end"] +1 #retorna as posições logo após o contexto
  pos = pos[pos<=nchar(str)] #evita ultrapassar o index maximo
  if(all(is.na(pos))) pos = 'no matches'
  return(unname(pos))
}

