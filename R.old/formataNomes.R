#' Ajusta maiusculas e minusculas nos nomes 
#' 
#' @param nomes vetor de nomes com grafia do instrumento
#' @return vetor de nomes com grafia ajustada
#' @author Ivan L M Ricarte
#' @export
formataNomes <- function(nomes) {
#    Encoding(nomes) <- 'UTF-8'
    nomes <- gsub("\\.", " ", nomes)
    nomes <- gsub("  ", " ", nomes)
    cnomes <- sapply(nomes, function(strn)
    { s <- strsplit(strn, "\\s")[[1]]
      paste0(toupper(substring(s, 1,1)), 
             tolower(substring(s, 2)),
             collapse=" ")}, USE.NAMES=FALSE)
    for (s in c(" De ", " Da ", " Dos ", " Das ", " E "))
        cnomes <- gsub(s, tolower(s), cnomes)
    cnomes
}