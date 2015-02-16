#' Demografico: obtem dados dos participantes sobre anos de experiencia
#' 
#' @param inscritos data frame com todos os dados dos participantes inscritos
#' @param grupos limiares para definir como anos de experiencia serao agrupados
#' @return data frame com quantidade de respostas por opcao
#' @author Ivan L M Ricarte
#' @export
inscritosPorExperiencia <- function(inscritos, grupos=c(0,5,10,15,20,25,30,35,40)) 
{
    require(broman)
    # Obtem vetor com anos de experiencia de cada participante
    aexp <- as.numeric(gsub("[^\\d]+", "", data$tempo_atuacao, perl=TRUE))
    # Descarta eventuais NA's
    aexp <- aexp[! is.na(aexp)]
    # Ajusta valor para quem entrou também número de meses
    aexp <- sapply(aexp,function(x) {if (x>100) x/10 else x})
    # Classifica nos grupos indicados
    Nivel <- cut(aexp, breaks=grupos)
    expdf <- as.data.frame(table(Nivel))
    pct <- 100 * expdf$Freq / sum(expdf$Freq)
    pctlab <- myround(pct, 1)
    cbind(expdf, Porcent=pct, PorcentLab=pctlab)
}