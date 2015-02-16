#' Demografico: obtem dados dos participantes sobre anos de idade
#' 
#' @param inscritos data frame com todos os dados dos participantes inscritos
#' @param dataBase dia utilizado como base para calculo da idade
#' @param grupos limiares para definir como anos de idade serao agrupados
#' @return data frame com quantidade de respostas por opcao
#' @author Ivan L M Ricarte
#' @export
inscritosPorIdade <- function(inscritos, 
                              dataBase = "2015-01-12",
                              grupos = c(0,25,30,35,40,45,50,55,60,65,100))
{                                        
    require(lubridate)
    require(broman)
    # Extrair as datas de nascimento
    pid <- data.frame(dataNasc = inscritos$datanasc)
    # Calcular idades, em anos
    dataInicio <- ymd(dataBase)
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
    # Descarta eventuais NA's
    idadesna <- idade[! is.na(idade)]
    # Classifica cada idade de acordo com os grupos indicados
    Nivel <- cut(idadesna, breaks=grupos)
    iddt <- as.data.frame(table(Nivel))
    # Inclui porcentagens
    idadepct <- 100 * iddt$Freq / sum(iddt$Freq)
    idadepctlab <- myround(idadepct,1)
    cbind(iddt,Porcent = idadepct, PorcentLab = idadepctlab)
}