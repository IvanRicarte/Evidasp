#' Importa dados dos resumos de evidencias
#' 
#' Carrega os dados do instrumento Resumos do REDCap, no formato R.
#' Script R e arquivo de dados (.csv) devem estar no diretorio de trabalho
#' @param dir diretorio de trabalho
#' @return data frame com todos os dados importados do instrumento Resumos
#' @author Ivan L M Ricarte
#' @export
getResumos <- function(dir = '.') {
    setwd(dir)
    datafile <- list.files(pattern="EvidSPResumos.*.r")
    if (length(datafile)==0)
        stop('Arquivo ausente em ', dir)
    source(datafile[1], encoding='UTF-8-BOM')
    data
}