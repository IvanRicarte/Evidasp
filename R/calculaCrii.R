#' IAM: Qual relevancia clinica da evidencia (questao 2)
#' 
#' @param dados data frame com todos os dados dos participantes inscritos
#' @return data frame com proporcao de respostas por opcao e indice de relevancia
#' @author Ivan L M Ricarte
#' @export
calculaCrii <- function(dados) {
    # filtrar respostas aos resumos 
    resumos <- dados[substr(dados[,2],1,6)=="resumo",]
    # escolher as colunas relevantes, isolando número do resumo
    #  e descartando respostas vazias
    coments <- data.frame(resumo = as.integer(substr(resumos$redcap_event_name,8,10)),
                          relev = resumos$iam_2)
    coments <- subset(coments, !is.na(relev))
    # separar respostas por resumo
    frem <- levels(as.factor(coments$resumo))
    rescrii <- data.frame(resumo=frem, N=0, tot=0, par=0, not=0, CRII=0)
    for (res in frem) {
        comres <- subset(coments, resumo==res)
        totrel <- sum(comres$relev==2)
        parrel <- sum(comres$relev==1)
        notrel <- sum(comres$relev==0)
        total <- totrel+parrel+notrel
        crii <- if (totrel+parrel > 0) {
            (2 * totrel * (totrel+parrel))/(total*(2*totrel + parrel))
        }
        else 0
        row <- which(rescrii$resumo==res)
        rescrii[row,2] <- total
        rescrii[row,3] <- totrel
        rescrii[row,4] <- parrel
        rescrii[row,5] <- notrel
        rescrii[row,6] <- crii
    }
    rescrii
}