#' Demografico: obtem dados dos participantes sobre acesso a Internet
#' 
#' @param inscritos data frame com todos os dados dos participantes inscritos
#' @return data frame com quantidade de respostas por opcao
#' @author Ivan L M Ricarte
#' @export
acessoInternet <- function(inscritos) {
    require(broman)
    qtd <- c(sum(inscritos$acesso_internet == 1),
             sum(inscritos$acesso_internet == 2),
             sum(inscritos$acesso_internet == 3),
             sum(inscritos$acesso_internet == 4),
             sum(inscritos$acesso_internet == 5))
    pct <- 100 * qtd / nrow(inscritos)
    pctlab <- myround(pct,1)
    niveis = factor(c("Nunca", "Menos de 1 vez por mês", "1 a 3 vezes por mês", 
                      "1 a 4 vezes por semana", "Diariamente"), 
                    levels = c("Diariamente", "1 a 4 vezes por semana", 
                               "1 a 3 vezes por mês", "Menos de 1 vez por mês",  
                               "Nunca"))
    data.frame(Nivel=niveis, Freq=qtd, Porcent=pct, PorcentLab=pctlab)
}