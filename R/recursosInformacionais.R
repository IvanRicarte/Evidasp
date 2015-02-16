#' Demografico: obtem dados dos participantes sobre recursos informacionais usados
#' 
#' @param inscritos data frame com todos os dados dos participantes inscritos
#' @return data frame com quantidade de respostas por opcao
#' @author Ivan L M Ricarte
#' @export
recursosInformacionais <- function(inscritos) {
    require(broman)
    # Recursos
    recursos <- c("Artigos científicos", "Bases de dados bibliográficas",
                  "Bases de evidências", "Conhecimento próprio",
                  "Dicionários, terminologias e classificações",
                  "Diretrizes clínicas", "Eventos", "Livros",
                  "Mecanismos de busca", "Outros profissionais da saúde",
                  "Paciente", "Portal Saúde Baseada em Evidência",
                  "Prontuário do paciente", "Sites especializados",
                  "Outros recursos")
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
    data.frame(Nivel=niveis,Freq=qtd)
}