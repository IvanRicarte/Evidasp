#' IAM: Comentarios sobre a evidencia (campo aberto)
#' 
#' Efeito colateral: criacao de arquivos de texto com comentarios para cada evidencia
#' @param dados data frame com todos os dados dos participantes inscritos
#' @return data frame com todos os comentarios por resumo
#' @author Ivan L M Ricarte
#' @export
extrairComentarios <- function(dados) {
    # filtrar respostas aos resumos 
    resumos <- dados[substr(dados[,2],1,6)=="resumo",]
    # escolher as colunas relevantes, isolando número do resumo e descartando comentários vazios
    coments <- data.frame(resumo = substr(resumos$redcap_event_name,8,10),
                          comentario = resumos$iam_comentario, id = resumos$record_id)
    coments <- subset(coments, comentario != '')
    # separar comentarios por resumo
    frem <- levels(as.factor(coments$resumo))
    for (res in frem) {
        filename <- paste("Coment",res,".txt",sep='')
        comres <- subset(coments, resumo==res)
        lines <- sprintf("%s (%d)", comres$comentario, comres$id)
        writeLines(paste(lines,"\n"),filename)
    }
    coments
}