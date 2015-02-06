#' Carrega os dados de participantes exportados do REDCap, no formato R.
#' 
#' REDCap exporta dois arquivos para serem utilizados em R, um script R (.r) e um 
#' arquivo de dados (.csv), que devem estar no diretorio de trabalho
#' @param token string do token para autorizar exportacao de dados na plataforma REDCap
#' @return data.frame com dados importados
#' @author Ivan L M Ricarte
#' @export
getParticipantes <- function(myToken) {
    # token mandatorio
    if (length(myToken) == 0)
        stop("REDCap token is required")
    
    # obter dados exportados diretamente de REDCap
    require(RCurl)
    url <- "http://redcap.fmrp.usp.br/api/"
    out <- postForm(url, token = myToken, content="record", format="csv", type="flat")
    read.csv(textConnection(out),encoding="UTF-8-BOM")
}