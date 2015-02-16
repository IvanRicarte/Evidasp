#' Demografico: obtem dados dos participantes sobre sexo
#' 
#' @param inscritos data frame com todos os dados dos participantes inscritos
#' @return data frame com quantidade de respostas por opcao
#' @author Ivan L M Ricarte
#' @export
inscritosPorSexo <- function(inscritos) {
    require(broman)
    sexos <- factor(c("Masculino", "Feminino"), levels=c("Masculino", "Feminino"))
    qtdd <- c(sum(inscritos$sexo == 1), sum(inscritos$sexo == 2))
    pct <- 100 * qtdd / nrow(inscritos)
    pctl <- myround(pct,1)
    data.frame(Nivel=sexos, Freq=qtdd, Porcent=pct, PorcentLab=pctl)
}