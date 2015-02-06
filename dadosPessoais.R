#' Extrai dados pessoais dos participantes a partir dos dados exportados
#' pela Plataforma REDCap para o projeto Participantes 
#' 
#' @param dadosPart data frame com dados exportados pelo REDCap
#' @return data frame com id, idade e sexo de cada participante
#' @export
dadosPessoais <- function(dadosPart) {
    df <- subset(dadosPart, 
                 redcap_event_name=="inscricao_arm_1" & tcle_concord==1,
                 select = c(record_id, datanasc, sexo))
    names(df) <- c("pid","datanasc","sexo")
    df
}