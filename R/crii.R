crii <- function(relev) {
    frem <- levels(as.factor(relev$Resumo))
    rescrii <- data.frame(resumo=frem, N=0, tot=0, par=0, not=0, CRII=0)
    for (res in frem) {
        comres <- subset(relev, Resumo==res)
        totrel <- sum(comres$Relevancia==2)
        parrel <- sum(comres$Relevancia==1)
        notrel <- sum(comres$Relevancia==0)
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