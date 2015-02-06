#' Demografico: obtem dados dos participantes sobre nivel de atencao em que atua
#' 
#' @param inscritos data frame com todos os dados dos participantes inscritos
#' @return data frame com quantidade de respostas por opcao
#' @author Ivan L M Ricarte
#' @export
inscritosPorAtuacao <- function(inscritos) {
    require(broman)
    niv <- factor(c("Primária", "Secundária", 
                    "Terciária", "Não atua na atenção ao paciente"),
                  levels=c("Primária", "Secundária", 
                           "Terciária", "Não atua na atenção ao paciente"))
    qtd <- c(sum(inscritos$area_atuacao___1), 
             sum(inscritos$area_atuacao___2),
             sum(inscritos$area_atuacao___3),
             sum(inscritos$area_atuacao___9))
    pct <- 100 * qtd / nrow(inscritos)
    pctlab <- myround(pct,1)
    data.frame(Nivel=niv, Freq=qtd, Porcent=pct, PorcentLab=pctlab)
}