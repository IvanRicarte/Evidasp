#' Importa dados da Plataforma REDCap
#' 
#' @param url  URI for REDCap API interface
#' @param myToken token obtido do REDCap, para exportar dados de cada projeto
#' @return data frame com todos os dados importados
#' @author Ivan L M Ricarte
#' @export
getRedcap <- function(url="http://redcap.fmrp.usp.br/api/", mytoken) {
    # token mandatorio
    if (length(myToken) == 0)
        stop("REDCap token is required")
    
    # obter dados exportados diretamente de REDCap
    require(RCurl)
    
    read.csv(textConnection(
        postForm(url, token = myToken, content="record", 
                 format="csv", type="flat")),
        encoding="UTF-8-BOM")
}