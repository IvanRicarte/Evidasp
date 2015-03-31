#' Demografico: obtem dados dos participantes sobre recursos informacionais usados
#' 
#' @param inscritos data frame com todos os dados dos participantes inscritos
#' @return data frame com quantidade de respostas por opcao
#' @author Ivan L M Ricarte
#' @export
recursosInformacionais <- function(fullDataset) {
    # extract professional data
    inscritos <- fullDataset %>%
        filter(redcap_event_name=="inscricao_arm_1", tcle_concord==1, 
               !is.na(profissao)) %>%
        select(recursos___11, recursos___12, recursos___13, recursos___14,
               recursos___15, recursos___16, recursos___17, recursos___18,
               recursos___19, recursos___20, recursos___21, recursos___22, 
               recursos___23, recursos___24, recursos___99)
    # Recursos
    recursos <- c("Scientific papers", "Bibliographic databases",
                  "Evidence databases", "Self knowledge",
                  "Dictionaries, terminologies and classifications",
                  "Clinical guidelines", "Events", "Books",
                  "Search engines", "Other health professionals",
                  "Patient", "Brazilian portal health-based evidence",
                  "Patient record", "Web speciliazed sites",
                  "Other resources")
    niveis <- factor(recursos, levels=recursos)
    # Usos declarados
    qtd <- c(sum(inscritos$recursos___11),
             sum(inscritos$recursos___12),
             sum(inscritos$recursos___13),
             sum(inscritos$recursos___14),
             sum(inscritos$recursos___15),
             sum(inscritos$recursos___16),
             sum(inscritos$recursos___17),
             sum(inscritos$recursos___18),
             sum(inscritos$recursos___19),
             sum(inscritos$recursos___20),
             sum(inscritos$recursos___21),
             sum(inscritos$recursos___22),
             sum(inscritos$recursos___23),
             sum(inscritos$recursos___24),
             sum(inscritos$recursos___99))
    df <- data.frame(Nivel=niveis,Freq=qtd)
}