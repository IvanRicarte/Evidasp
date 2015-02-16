#' Demografico: obtem dados dos participantes sobre municipio de atuacao
#' 
#' @param inscritos data frame com todos os dados dos participantes inscritos
#' @param ordered valor logico, TRUE se os dados devem ser ordenados por maior 
#' quantidade de inscritos por municipio
#' @param threshold valor inteiro, quantidade de inscritos para que municipio seja
#' agrupado na categoria 'Outros'
#' @return data frame com quantidade de respostas por opcao
#' @author Ivan L M Ricarte
#' @export
inscritosPorMunicipio <- function(inscritos, ordered = TRUE, threshold = 0) {
    require(broman)
    # Obter dados do municpios do data.frame inscritos
    dmun <- as.data.frame(table(inscritos$inicio_municipio))
    # Ajustar nome da primeira coluna
    colNames <- names(dmun)
    colNames[1] <- "Nivel"
    names(dmun) <- colNames
    # Acentuacao
    mnames <- as.vector(dmun[,1])
    Encoding(mnames) <- 'UTF-8'
    dmun$Nivel <- mnames
    # Qtde de entradas não consideradas
    qtdOutros <- sum(subset(dmun, dmun$Nivel == "" | 
               dmun$Nivel == "_Ignorada ou não listada" | 
               dmun$Freq <= threshold)$Freq)
    # Remover entradas não consideradas
    dmun <- subset(dmun, dmun$Nivel != "" & 
                       dmun$Nivel != "_Ignorada ou não listada" & 
                       dmun$Freq > threshold)
    # Ordenar por maior numero de inscritos
    if (ordered) {
        smun <- dmun[order(-dmun$Freq,dmun$Nivel), ]
        smun$Nivel <- reorder(smun$Nivel, -smun$Freq)
    }
    else
        smun <- dmun
    # Incluir linha 'Outros'
    if (qtdOutros > 0) {
        outros <- data.frame(Nivel='Outros', Freq=qtdOutros)
        smun <- rbind(smun, outros)
    }
    # Incluir coluna 'Porcent'
    total <- sum(smun$Freq)
    porcent <- smun$Freq / total * 100
    pctlab <- myround(porcent, 1)
    cbind(smun, Porcent=porcent, PorcentLab=pctlab)
}