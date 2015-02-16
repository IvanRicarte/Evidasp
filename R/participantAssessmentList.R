#' Demographic + IAM: get all participant names and quantity of assessments for each
#' 
#' This function should be used to obtain the data that will be on certificates
#' @param data data frame with full REDCap exported data
#' @param drop logic, TRUE if output list excludes participants with no assessment
#' @return data frame name, document (CPF) number, CPF validity, and number of assessments
#' @author Ivan L M Ricarte
#' @export
participantAssessmentList <- function(fullDataset, drop=TRUE) {
    # extract info from full data set
    part <- subset(fullDataset, nome!="", select=c(record_id,nome,cpf))
    # name and document number formatting
    part$nome <- formataNomes(as.vector(part$nome))
    part$cpf <- formataCPF(as.vector(part$cpf))
    cpfOk <- verificaCPF(part$cpf)
    part <- cbind(part,CpfOk=cpfOk)
    # compute number of assessments
    resaval <- numeric(nrow(part))
    for (p in 1:nrow(part)) {
        id <- part[p,1]
        resaval[p] <- sum(id==fullDataset$record_id &
                        substr(fullDataset$redcap_event_name,1,6)=="resumo")
    }
    part <- cbind(part,ResAval = resaval)
    if (drop==TRUE)
        part <- subset(part, ResAval > 0)
    
    names(part) <- c("id", "name", "cpf", "validcpf", "assessments")
    part
}

#' Ajusta maiusculas e minusculas nos nomes 
#' 
#' @param nomes vetor de nomes com grafia do instrumento
#' @return vetor de nomes com grafia ajustada
#' @author Ivan L M Ricarte
formataNomes <- function(nomes) {
    Encoding(nomes) <- 'UTF-8'
    nomes <- gsub("\\.", " ", nomes)
    nomes <- gsub("  ", " ", nomes)
    cnomes <- sapply(nomes, function(strn)
    { s <- strsplit(strn, "\\s")[[1]]
      paste0(toupper(substring(s, 1,1)), 
             tolower(substring(s, 2)),
             collapse=" ")}, USE.NAMES=FALSE)
    for (s in c(" De ", " Da ", " Dos ", " Das "))
        cnomes <- gsub(s, tolower(s), cnomes)
    cnomes
}

#' Ajusta formatacao de CPF 
#' 
#' @param cpfs vetor de CPF com grafia nao uniforme
#' @return vetor de CPF com formato ajustado
#' @author Ivan L M Ricarte
formataCPF <- function(cpfs) {
    # remover não dígitos
    numcpf <- gsub("[ \\.\\-/]", "", cpfs, perl=TRUE)
    # limitar comprimento em 11 digitos
    numcpf <- as.vector(
        sapply(numcpf, function(s) {
            s <- if(nchar(s) > 11) {
                substr(s,1,11)
            }
            else if(nchar(s) < 11) {
                sbl <- sprintf("%011s",s) 
                gsub(" ","0",sbl) 
            } 
            else 
                s
            s
    }))
    # incluir ponto e traço
    verif <- paste0("-",substr(numcpf,10,11))
    numcpf <- paste(substr(numcpf,1,3), substr(numcpf,4,6), substr(numcpf,7,9),sep=".")
    numcpf <- paste0(numcpf,verif)
}

#' Verifica dígitos de um CPF  
#' 
#' @param cpf um CPF no formato ###.###.###-##
#' @return TRUE se digitos verificadores estao coerentes com CPF
#' @author Ivan L M Ricarte
verificaCPF <- function(cpf) {
    # digitos verificadores declarados
    d1 <- as.integer(substr(cpf,13,13))
    d2 <- as.integer(substr(cpf,14,14))
    # cálculo dos dígitos verificadores
    rem1 <- (10*as.integer(substr(cpf,1,1)) +
        9*as.integer(substr(cpf,2,2)) +
        8*as.integer(substr(cpf,3,3)) +
        7*as.integer(substr(cpf,5,5)) +
        6*as.integer(substr(cpf,6,6)) +
        5*as.integer(substr(cpf,7,7)) +
        4*as.integer(substr(cpf,9,9)) +
        3*as.integer(substr(cpf,10,10)) +
        2*as.integer(substr(cpf,11,11))) %% 11
    vf1 <- ifelse(rem1 < 2,0,11-rem1)
    rem2 <- (11*as.integer(substr(cpf,1,1)) +
                 10*as.integer(substr(cpf,2,2)) +
                 9*as.integer(substr(cpf,3,3)) +
                 8*as.integer(substr(cpf,5,5)) +
                 7*as.integer(substr(cpf,6,6)) +
                 6*as.integer(substr(cpf,7,7)) +
                 5*as.integer(substr(cpf,9,9)) +
                 4*as.integer(substr(cpf,10,10)) +
                 3*as.integer(substr(cpf,11,11)) +
                 2*vf1) %% 11
    vf2 <- ifelse(rem2 < 2,0,11-rem2)
    # declarados iguais aos calculados?
    d1 == vf1 & d2 == vf2    
}