professionLabel <- function(professionCode) {
    profName <- c("Assistente social", "Biomédico", "Enfermeiro", 
                       "Farmacêutico", "Fisioterapeuta", "Fonoaudiólogo", 
                       "Médico", "Nutricionista", "Odontólogo", "Psicólogo", 
                       "Terapeuta ocupacional", "Outros")
    profCodes <- c(6:16,99)
    index <- which(profCodes == professionCode)
    profName[index]
}