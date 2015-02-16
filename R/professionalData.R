#' Demographic: participants' professional experience
#' 
#' @param fullDataset data frame with REDCap exported data
#' @return data frame with participant id, 
#' @author Ivan L M Ricarte
#' @export
professionalData <- function(fullDataset) {
    # extract professional data
    profdata <- fullDataset %>%
        filter(redcap_event_name=="inscricao_arm_1", tcle_concord==1, 
               !is.na(profissao)) %>%
        select(record_id, profissao, outra_profissao, especialidade,
                inicio_municipio, maismedicos1, area_atuacao___1,
                area_atuacao___2, area_atuacao___3, area_atuacao___9,
               tempo_atuacao) %>%
        mutate(prof = ifelse(profissao==6, "Assistente social", 
                          ifelse(profissao==7, "Biomédico", 
                                 ifelse(profissao==8, "Enfermeiro",
                                        ifelse(profissao==9, "Farmacêutico", 
                                               ifelse(profissao==10, "Fisioterapeuta", 
                                                      ifelse(profissao==11, "Fonoaudiólogo",
                                                             ifelse(profissao==12, "Médico", 
                                                                    ifelse(profissao==13, "Nutricionista",
                                                                           ifelse(profissao==14, "Odontólogo",
                                                                                  ifelse(profissao==15, "Psicólogo", 
                                                                                         ifelse(profissao==16, "Terapeuta ocupacional",
                                                                                                formataNomes(as.character(outra_profissao))
                                                                                         )))))))))))) %>%
       mutate(exper = as.numeric(gsub("[^0-9]*([0-9]+).*$", "\\1", tempo_atuacao))) %>%
        select(id=record_id,profissao=prof,especialidade,
               municipio=inicio_municipio,experiencia=exper,
               maismedicos=maismedicos1,atencaoprimaria=area_atuacao___1,
               atencaosecundaria=area_atuacao___2,atencaoterciaria=area_atuacao___3)
}