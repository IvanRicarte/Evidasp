#' Extrai dados pessoais dos participantes a partir dos dados exportados
#' pela Plataforma REDCap para o projeto Participantes 
#' 
#' @param dadosPart data frame com dados exportados pelo REDCap
#' @return data frame com id, idade e sexo de cada participante
#' @export
dadosPessoais <- function(dadosPart) {
    require(lubridate)
    df <- subset(dadosPart, 
                 redcap_event_name=="inscricao_arm_1" & tcle_concord==1,
                 select = c(record_id, datanasc, sexo))
    dataBase <- today()
    idadeInterval <- new_interval(ymd(pid[,1]), dataInicio)
    idadePeriodo <- as.period(idadeInterval, unit="years")
    idadeTokens <- strsplit(as.character(idadePeriodo), "y")
    idade <- sapply(idadeTokens, 
                    function(el) { 
                        if (length(el) > 1) 
                            as.numeric(el[[1]]) 
                        else 
                            NA
                    }
    )
    names[,2] <- idade
    names(df) <- c("pid","idade","sexo")
    df
}