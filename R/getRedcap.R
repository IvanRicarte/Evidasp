#' Get data from REDCap. If a token is given and an Internet connection is available, data is directly imported from the REDCap platform. Otherwise, a the name of a CSV file with data exported from REDCap is expected. If neither is provided, the function stops. 
#' 
#' @param url  URI for REDCap API interface
#' @param rtoken token for REDCap project
#' @param filename name of CSV file exported by REDCap
#' @return data frame full data set
#' @author Ivan L M Ricarte
#' @export
getRedcap <- function(url="http://redcap.fmrp.usp.br/api/", rtoken=character(0), filename=character(0)) {
    if (length(rtoken) != 0) {
        src <- postForm(url, token = rtoken, 
                        content="record", format="csv", type="flat")
        con <- textConnection(src)
    }
    else if(length(filename) != 0) {
        con <- filename;
    }
    else {
        stop("REDCap token or filename is required")
    }
    
    # read REDCap data to data frame
    read.csv(con, encoding="UTF-8-BOM")
}