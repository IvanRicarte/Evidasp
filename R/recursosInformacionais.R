#' Demografico: obtem dados dos participantes sobre recursos informacionais usados
#' 
#' @param inscritos data frame com todos os dados dos participantes inscritos
#' @return data frame com quantidade de respostas por opcao
#' @author Ivan L M Ricarte
#' @export
recursosInformacionais <- function(inscritos) {
    require(broman)
    # Recursos
    recursos <- c("Artigos cient�ficos", "Bases de dados bibliogr�ficas",
                  "Bases de evid�ncias", "Conhecimento pr�prio",
                  "Dicion�rios, terminologias e classifica��es",
                  "Diretrizes cl�nicas", "Eventos", "Livros",
                  "Mecanismos de busca", "Outros profissionais da sa�de",
                  "Paciente", "Portal Sa�de Baseada em Evid�ncia",
                  "Prontu�rio do paciente", "Sites especializados",
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