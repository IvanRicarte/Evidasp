#' Demografico: gera grafico com dados demograficos
#' 
#' @param data data frame com pelo menos quatro colunas:
#'        1. Nivel: Rotulos para os valores do eixo x (factor)
#'        2. Freq: Valores absolutos (contagem) para cada nivel
#'        3. Porcent: Valores relativos (porcentagem)
#'        4. PorcentLab: Strings para serem usadas nas porcentagens
#' @param titulo Titulo do grafico
#' @param eixox Nome do eixo x
#' @param absolute valor logico, se os valores absolutos(TRUE) ou porcentagens
#' (FALSE) devem ser usados no grafico
#' @param labeled valor logico, se cada barra do grafico deve ter rotulo com o valor
#' @param rotatex valor logico, se os rotulos do eixo x devem estar na vertical
#' @return plot com dados do data frame
#' @author Ivan L M Ricarte
#' @export
plotDados <- function(data, titulo = '', eixox = '', 
                      absolute = FALSE, labeled=FALSE, rotatex=FALSE,
                      fontsize = 10) {
    require(ggplot2)
  
    if (absolute == TRUE){
        eixoy <- "Participantes"
        p <- ggplot(data, aes(Nivel, Freq))
        barlabel <- aes(label=Freq)
        ymax <- max(data$Freq) + 10
    }
    else {
        eixoy <- "% Participantes"
        p <- ggplot(data, aes(Nivel, Porcent))
        barlabel <- aes(label=PorcentLab)
        ymax <- ceiling(max(data$Porcent) + 10)
    }
    if (labeled == TRUE) {
        p <- p + geom_text(barlabel, vjust=-.5, size=4)
    }
    if (rotatex == TRUE) {
        p <- p + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
    }
    p + geom_bar(stat="identity", fill="darkblue") +
        xlab(eixox) + ylab(eixoy) + ggtitle(titulo) +
        theme(axis.text=element_text(size=fontsize), 
              axis.title=element_text(size=fontsize, face="plain"),
              title=element_text(size=fontsize+2, face="bold")) +
        ylim(0,ymax)
}