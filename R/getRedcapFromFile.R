#' Get data from file exported by REDCap
#' 
#' @param filename name of csv file with REDCap data
#' @return data frame com todos os dados importados
#' @author Ivan L M Ricarte
#' @export
getRedcapFromFile <- function(filename) {
    if (nchar(filename) == 0)
        stop("file name required")
    
    read.csv(filename, encoding="UTF-8-BOM")
}