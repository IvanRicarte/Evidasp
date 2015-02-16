#' Get data directly from REDCap
#' 
#' @param url  URI for REDCap API interface
#' @param myToken token for REDCap project
#' @return data frame full data set
#' @author Ivan L M Ricarte
#' @export
getRedcap <- function(url="http://redcap.fmrp.usp.br/api/", mytoken) {
    if (length(mytoken) == 0)
        stop("REDCap token is required")
    
    # transfer data from REDCap
    require(RCurl)
    out <- postForm(url, token = mytoken, 
                    content="record", format="csv", type="flat")
    
    # read transferred data to data frame
    con <- textConnection(out)
    read.csv(con, encoding="UTF-8-BOM")
}