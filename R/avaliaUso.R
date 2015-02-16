#' IAM: Qual uso potencial da evidencia (questao 3)
#' 
#' @param dados data frame com todos os dados dos participantes inscritos
#' @return data frame com proporcao de respostas por opcao
#' @author Ivan L M Ricarte
#' @export
avaliaUso <- function(dados) {
    # filtrar respostas aos resumos 
    resumos <- dados[substr(dados[,2],1,6)=="resumo",]
    # escolher as colunas relevantes, isolando número do resumo
    #  e descartando respostas vazias
    q3 <- data.frame(resumo = as.integer(substr(resumos$redcap_event_name,8,10)),
                          usopacien = resumos$iam_3, 
                          assistdif = resumos$iam_3a___1,
                          justescol = resumos$iam_3a___2,
                          maiscerto = resumos$iam_3a___3,
                          entmelhor = resumos$iam_3a___4,
                          discussao = resumos$iam_3a___5,
                          convencer = resumos$iam_3a___6)
    q3 <- subset(q3, !is.na(usopacien))
    # separar respostas por resumo
    frem <- levels(as.factor(q3$resumo))
    puso <- data.frame(resumo=frem, N=0, usopacien=0, assistdif=0, justescol=0, 
                         maiscerto=0, entmelhor=0, discussao=0, convencer=0)
    for (res in frem) {
        r <- subset(q3, resumo==res)
        row <- which(puso$resumo==res) 
        puso[row,2] <- sum(r$usopacien)
        puso[row,3] <- mean(r$usopacien)
        r2 <- subset(r, usopacien==1)
        puso[row,4] <- mean(r2$assistdif)
        puso[row,5] <- mean(r2$justescol)
        puso[row,6] <- mean(r2$maiscerto)
        puso[row,7] <- mean(r2$entmelhor)
        puso[row,8] <- mean(r2$discussao)
        puso[row,9] <- mean(r2$convencer)
    }
    puso
}