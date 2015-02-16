#' Tabulate by profession
#'
#' @param profdata data frame with professional data
#' @return data frame with profession and number of professionals 
#' (total and by level of attention)
#' @author Ivan L M Ricarte
#' @export
byProfession <- function(profdata) {
    profdata %>% 
        group_by(profissao) %>% 
        summarise(total=n(), a1=sum(atencaoprimaria), 
                  a2=sum(atencaosecundaria),a3=sum(atencaoterciaria))
}