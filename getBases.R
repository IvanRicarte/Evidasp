#' Resumos: obtem bases de evidencias utilizadas
#' 
#' @param resumos data frame com dados importados do instrumento Resumos
#' @return data frame com nome da base e os resumos extraidos dessa base
#' @author Ivan L M Ricarte
#' @export
getBases <- function(resumos) {
    # lista de todas as bases de evidências efetivamente utilizadas
    bases <- split(resumos$record_id,resumos$base.factor, drop=TRUE)
    qtbases <- length(bases)
    qtdPorBase <- vector(mode='integer', length=qtbases)
    resPorBase <- vector(mode='character',length=qtbases)
    for (b in 1:qtbases) {
        lstres <- ""
        qtdres <- length(bases[[b]])
        for (r in 1:qtdres) {
            sep <- if (r < qtdres) "," else " "
            lstres <- paste(lstres, bases[[b]][r], sep)
        }
        qtdPorBase[b] <- qtdres
        resPorBase[b] <- lstres
    }
    data.frame(Base=names(bases), N=qtdPorBase, Resumos=resPorBase)
}