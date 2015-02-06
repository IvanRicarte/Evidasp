#' Demografico: obtem dados dos participantes sobre proficiencia de leitura em ingles
#' 
#' @param inscritos data frame com todos os dados dos participantes inscritos
#' @return data frame com quantidade de respostas por opcao
#' @author Ivan L M Ricarte
#' @export
inscritosProficiencia <- function(inscritos) {
    require(broman)
    niv <- factor(c("Boa leitura", "Leitura razoável", "Não lê"))
    qtd <- c(sum(inscritos$proficiencia == 1),
             sum(inscritos$proficiencia == 2),
             sum(inscritos$proficiencia == 3))
    pct <- 100 * qtd / nrow(inscritos)
    pctlab <- myround(pct,1)
    data.frame(Nivel=niv, Freq=qtd, Porcent=pct, PorcentLab=pctlab)
}