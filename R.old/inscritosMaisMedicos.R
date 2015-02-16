#' Demografico: obtem dados dos participantes sobre participacao no Mais Medicos
#' 
#' @param inscritos data frame com todos os dados dos participantes inscritos
#' @return data frame com quantidade de respostas por opcao
#' @author Ivan L M Ricarte
#' @export
inscritosMaisMedicos <- function(inscritos) {
    require(broman)
    qtds <- sum(inscritos$maismedicos1==1)
    total <- sum(inscritos$profissao==12) # médicos
    niv <- factor(c("Sim", "Não"), levels=c("Sim","Não"))
    qtd <- c(qtds, total-qtds)
    pct <- 100 * qtd / total
    pctlab <- myround(pct,1)
    data.frame(Nivel=niv, Freq=qtd, Porcent=pct, PorcentLab=pctlab)
}