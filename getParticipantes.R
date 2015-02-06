#' Carrega os dados de participantes exportados do REDCap, no formato R.
#' 
#' REDCap exporta dois arquivos para serem utilizados em R, um script R (.r) e um 
#' arquivo de dados (.csv), que devem estar no diretorio de trabalho
#' @param dir diretorio de trabalho
#' @param limpo valor logico, importacao deve ou nao remover do conjunto de dados as observacoes incompletas
#' @return data.frame com dados importados
#' @author Ivan L M Ricarte
#' @export
getParticipantes <- function(dir='.', limpo=FALSE) {
    setwd(dir)
    datafile <- list.files(pattern="EvidSPParticipantes.*.r")
    if (length(datafile) == 0)
        stop("Arquivos com dados de participantes nao encontrados em ", dir)
    source(datafile[1], encoding='UTF-8-BOM')
    if (limpo == TRUE)
        data <- subset(data,redcap_event_name=="inscricao_arm_1" & 
                tcle_concord==1 & 
                evidsp_tcle_demogrfico_inicial_timestamp != '[not completed]')
    data
}