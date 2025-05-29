#' @title Finding positions after a pattern in a string.
#'
#' @return Integer positions: if `str[3:8]` = p, returns 9.
#'
#' @param str string
#' @param p pattern
#'
#' @export

match_str = function(str, p){
  if(p == '') return(1:nchar(str))
  pos = stringi::stri_locate_all_fixed(str, p, overlap = T)[[1]][, "end"] +1 #retorna as posições logo após o contexto
  pos = pos[pos<=nchar(str)] #evita ultrapassar o index maximo
  if(all(is.na(pos))) pos = 'no matches'
  return(pos)
}

