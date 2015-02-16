#' Capitalize words in proper names, lowercase connectives 
#' 
#' @param nomes names, as inserted by users
#' @return formatted names
#' @author Ivan L M Ricarte
#' @export
formatName <- function(nomes) {
    # usual mistypings
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