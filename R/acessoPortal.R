#' Demografico: frequencia de acesso ao portal Saude baseada em evidencias
#' 
#' @param inscritos data frame com todos os dados dos participantes inscritos
#' @return data frame com quantidade de respostas por opcao
#' @author Ivan L M Ricarte
#' @export
acessoPortal <- function(inscritos) {
    acesso <- inscritos$acesso_portal
    # Remover NAs
    acesso <- acesso[!is.na(acesso)]
    qtd <- c(sum(acesso == 1),
             sum(acesso == 2),
             sum(acesso == 3),
             sum(acesso == 4),
             sum(acesso == 5))
    niveis = factor(c("Nunca", "Menos de 1 vez por mês", "1 a 3 vezes por mês", 
                      "1 a 4 vezes por semana", "Diariamente"), 
                    levels = c("Diariamente", "1 a 4 vezes por semana", 
                               "1 a 3 vezes por mês", "Menos de 1 vez por mês",  
                               "Nunca"))
    data.frame(Nivel=niveis, Freq=qtd)
}