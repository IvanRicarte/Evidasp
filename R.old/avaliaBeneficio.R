#' IAM: Qual beneficio esperado pelo uso da evidencia (questao 4)
#' 
#' @param dados data frame com todos os dados dos participantes inscritos
#' @return data frame com proporcao de respostas por opcao
#' @author Ivan L M Ricarte
#' @export
avaliaBeneficio <- function(dados) {
    # filtrar respostas aos resumos 
    resumos <- dados[substr(dados[,2],1,6)=="resumo",]
    # escolher as colunas relevantes, isolando número do resumo
    #  e descartando respostas vazias
    q4 <- data.frame(resumo = as.integer(substr(resumos$redcap_event_name,8,10)),
                      benefesp = resumos$iam_4, 
                      melhorar = resumos$iam_4a___1,
                      prevenir = resumos$iam_4a___2,
                      evitar = resumos$iam_4a___3)
    q4 <- subset(q4, !is.na(benefesp))
    # separar respostas por resumo
    frem <- levels(as.factor(q4$resumo))
    be <- data.frame(resumo=frem, N=0, benefesp=0, melhorar=0, 
                       prevenir=0, evitar=0)
    for (res in frem) {
        r <- subset(q4, resumo==res)
        row <- which(be$resumo==res) 
        be[row,2] <- sum(r$benefesp)
        be[row,3] <- mean(r$benefesp)
        r2 <- subset(r, benefesp==1)
        be[row,4] <- mean(r2$melhorar)
        be[row,5] <- mean(r2$prevenir)
        be[row,6] <- mean(r2$evitar)
    }
    be
}