#' Extract personal data (age, sex) from full data set
#' 
#' @param fullDataset data frame with REDCap exported data
#' @param referenceDate date (YYYY-MM-DD) to compute age; if NA, assumes today
#' @return data frame with id, age e sex for each participant. 
#' @author Ivan L M Ricarte
#' @export
personalData <- function(fullDataset, referenceDate = NA) {
    require(lubridate)
    # Extract 
    df <- subset(fullDataset, 
                 redcap_event_name=="inscricao_arm_1" & tcle_concord==1 & !is.na(profissao),
                 select = c(record_id, datanasc, sexo))
    if (is.na(referenceDate)) {
        baseDate <- today()
    }
    else {
        baseDate <- ymd(referenceDate)
    }
    ageInterval <- new_interval(ymd(df[,2]), baseDate)
    agePeriod <- as.period(ageInterval, unit="years")
    ageTokens <- strsplit(as.character(agePeriod), "y")
    ages <- sapply(ageTokens, 
                   function(el) { 
                       if (length(el) > 1) 
                           as.numeric(el[[1]]) 
                       else 
                           NA
                   }
    )
    df[,2] <- ages
    names(df) <- c("pid","age","sex")
    df
}