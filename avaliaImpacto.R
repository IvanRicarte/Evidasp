#' IAM: Qual impacto cognitivo da evidencia (questao 1)
#' 
#' @param dados data frame com todos os dados dos participantes inscritos
#' @return data frame com proporcao de respostas por opcao
#' @author Ivan L M Ricarte
#' @export
avaliaImpacto <- function(dados) {
    # filtrar respostas aos resumos 
    resumos <- dados[substr(dados[,2],1,6)=="resumo",]
    # escolher as colunas relevantes, isolando número do resumo
    #  e descartando respostas vazias
    q1 <- data.frame(resumo = as.integer(substr(resumos$redcap_event_name,8,10)),
                          novo = resumos$iam_1___1, 
                          motv = resumos$iam_1___2,
                          cnfr = resumos$iam_1___3,
                          rcnf = resumos$iam_1___4,
                          lmbr = resumos$iam_1___5,
                          nsat = resumos$iam_1___6,
                          prob = resumos$iam_1___7,
                          disc = resumos$iam_1___8,
                          prej = resumos$iam_1___9)
    # separar respostas por resumo
    frem <- levels(as.factor(q1$resumo))
    resimp <- data.frame(resumo=frem, N=0, novo=0, motv=0, cnfr=0, rcnf=0, 
                          lmbr=0, nsat=0, prob=0, disc=0, prej=0)
    for (res in frem) {
        r <- subset(q1, resumo==res)
        row <- which(resimp$resumo==res)
        resimp[row,2] <- nrow(r)
        resimp[row,3] <- mean(r$novo)
        resimp[row,4] <- mean(r$motv)
        resimp[row,5] <- mean(r$cnfr)
        resimp[row,6] <- mean(r$rcnf)
        resimp[row,7] <- mean(r$lmbr)
        resimp[row,8] <- mean(r$nsat)
        resimp[row,9] <- mean(r$prob)
        resimp[row,10] <- mean(r$disc)
        resimp[row,11] <- mean(r$prej)
    }
    resimp
}