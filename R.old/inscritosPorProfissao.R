#' Demografico: obtem dados dos participantes sobre profissao
#' 
#' @param inscritos data frame com todos os dados dos participantes inscritos
#' @param ordered valor logico, TRUE se os dados devem ser ordenados por maior 
#' quantidade de inscritos por profissao
#' @return data frame com quantidade de respostas por opcao
#' @author Ivan L M Ricarte
#' @export
inscritosPorProfissao <- function(inscritos, ordered = FALSE) {
    require(broman)
    # Obter dados de profissao do data.frame inscritos
    dprof <- as.data.frame(table(inscritos$profissao))
    # Ajustar nome da primeira coluna
    colNames <- names(dprof)
    colNames[1] <- "CodProf"
    names(dprof) <- colNames
    # Nomes das profissões
    pnames <- factor(c("Assistente social", "Biomédico", "Enfermeiro", 
                       "Farmacêutico", "Fisioterapeuta", "Fonoaudiólogo", 
                       "Médico", "Nutricionista", "Odontólogo", "Psicólogo", 
                       "Terapeuta ocupacional", "Outros"), 
                     levels=c("Assistente social", "Biomédico", "Enfermeiro", 
                              "Farmacêutico", "Fisioterapeuta", "Fonoaudiólogo", 
                              "Médico", "Nutricionista", "Odontólogo", "Psicólogo", 
                              "Terapeuta ocupacional", "Outros"))
    dprof <- cbind(Nivel=pnames,dprof)
    # Incluir porcentagens
    pct <- 100 * dprof$Freq / sum(dprof$Freq)
    pctlab <- myround(pct,1)
    dprof <- cbind(dprof, Porcent=pct, PorcentLab=pctlab)
    
    # Ordenar por maior numero de inscritos
    if (ordered) {
        dprof <- dprof[order(-dprof$Freq,dprof$Nivel), ]
        dprof$Nivel <- reorder(dprof$Nivel, -dprof$Freq)
    }
    
    dprof
}