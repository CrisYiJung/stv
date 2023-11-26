#' @title Tabela das Principais Estatísticas
#'
#' @param dados Vetor numérico contendo os valores da série.
#'
#' @description
#' Função que retorna uma tabela com as principais estatísticas da série temporal
#'
#' @return Uma tabela das principais estatísticas da série temporal.
#'
#' @export
stv_tabela = function(dados){
  if(class(dados) %in% c("numeric", "integer")){
    max = max(dados)
    min = min(dados)
    med = mean(dados)
    sd = sd(dados)
    median = median(dados)
    df = data.frame(Estatistica = c("Mínimo", "Máximo", "Desvio Padrão", "Mediana", "Média"),
                    Valor = c(min, max, sd, median, med))
  }else{stop("Erro: o argumento 'dados' precisa ser numérico.")}
  return(df)
}
