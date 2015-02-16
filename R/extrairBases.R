#' Resumos: obtem bases de evidencias utilizadas
#' 
#' @param resumos data frame com dados importados do instrumento Resumos
#' @return data frame com nome da base e os resumos extraidos dessa base
#' @author Ivan L M Ricarte
#' @export
extrairBases <- function(resumos) {
    # lista de todas as bases de evidências efetivamente utilizadas
    bases <- split(resumos$record_id,resumos$base, drop=TRUE)
    qtbases <- length(bases)
    nBase <- vector(mode='character',length=qtbases)
    qtdPorBase <- vector(mode='integer', length=qtbases)
    resPorBase <- vector(mode='character',length=qtbases)
    for (b in 1:qtbases) {
        nBase[b] <- nomeBase(as.integer(names(bases[b])))
        lstres <- bases[[b]][1]
        qtdres <- length(bases[[b]])
        if (qtdres > 1) {
            for (r in 2:qtdres) {
                sepc <- if (r <= qtdres) ", " else ""
                lstres <- paste(lstres, bases[[b]][r], sep=sepc)
            }
        }
        qtdPorBase[b] <- qtdres
        resPorBase[b] <- lstres
    }
    data.frame(Base=nBase, N=qtdPorBase, Resumos=resPorBase)
}

nomeBase <- function(base) {
    bases <- c("Access - Emergency Medicine", "Access - Medicine", "Access - Physiotherapy",
        "Atheneu - O melhor da saúde", "BMJ - BestPractice", "BMJ - Learning",
        "BVS - Atenção primária à saúde", "DynaMed", "Micromedex 2.0",
        "ProQuest - Hospital collection", "Revealed - Anatomy, physiology", "Rebrats",
        "Bulário eletrônico", "ProQualis", "ProQuest - Enfermagem", 
        "ProQuest - Psicologia", "ProQuest - Saúde da Família", "ProQuest - Gestão em Saúde",
        "UNA-SUS", "Conselho Federal de Farmácia", "Micromedex CareNotes",
        "Micromedex DrugReax", "Social Work References Center", "Rehabilitation Reference Center",
        "Nursing Reference Center", "Sports Medicine and Exercise Science")
    bases[base]
}