#' @title Gráfico de Sequência de Séries Temporais
#'
#' @param dados Objeto do tipo \code{Data.Frame} contendo os dados.
#' @param x String contendo o nome de uma coluna de \code{dados} indicando data ou índice. A coluna precisa ser numérica.
#' @param y String contendo o nome de uma coluna de \code{dados} indicando valores da série. A coluna precisa ser numérica.
#' @param eixo_x String contendo o nome do eixo x.
#' @param eixo_y String contendo o nome do eixo y.
#' @param titulo String contendo o título do gráfico.
#' @param bolinha Argumento do tipo lógico indicando a inclusão pontos.
#' @param cor String contendo a cor do gráfico, podendo ser no formato hexadecimal.
#' @param tema String contendo o tema do gráfico. Precisa ser necessariamente 'claro' ou 'escuro'.
#' @param anim Argumento do tipo lógico indicando a inclusão de animação.
#'
#' @return Um gráfico de sequência da série temporal.
#'
#' @examples
#' library(stv)
#' library(ggdark)
#' library(ggplot2)
#' teste <- data.frame(data = seq(1,100), valor = rnorm(100))
#' stv_seq(teste, "data", "valor")
#' stv_seq(teste, "data", "valor", "Índice", "Valor da Série", bolinha = TRUE, tema = "escuro")
#' stv_seq(teste, "data", "valor", cor = "magenta", anim = TRUE)
#'
#' @import tidyverse
#' @import gganimate
#' @import ggplot2
#' @import ggdark
#' @import gifski
#'
#' @export
stv_seq <- function(dados, x, y, eixo_x = x, eixo_y = y, titulo = "Gráfico de Sequência", bolinha = FALSE, cor = "purple", tema = "claro", anim = FALSE){
  if(class(dados) == "data.frame" & class(x) == "character" & class(y) == "character" &
     class(eixo_x) == "character" & class(eixo_y) == "character" & class(titulo) == "character" &
     class(bolinha) == "logical" & class(cor) == "character" & class(tema) == "character" &
     class(dados[,x]) %in% c("integer", "numeric") & class(dados[,y]) %in% c("integer", "numeric")){
    if(tema == "claro"){
      if(bolinha == TRUE){
        resultado <- ggplot(dados, aes(x = .data[[x]], y = .data[[y]])) + geom_line(color = cor, size = 1) +
          labs(title = titulo, x = eixo_x, y = eixo_y) + geom_point(color = cor) +
          theme(plot.background = element_rect(fill = "#D3D3D3"),
                panel.background = element_blank(),
                panel.grid.major = element_line(color = "#A9A9A9", size = 0.2),
                panel.grid.minor = element_line(color = "#A9A9A9", size = 0.2),
                axis.ticks = element_blank())
      }else{
        resultado <- ggplot(dados, aes(x = .data[[x]], y = .data[[y]])) + geom_line(color = cor, size = 1) +
          labs(title = titulo, x = eixo_x, y = eixo_y) +
          theme(plot.background = element_rect(fill = "#D3D3D3"),
                panel.background = element_blank(),
                panel.grid.major = element_line(color = "#A9A9A9", size = 0.2),
                panel.grid.minor = element_line(color = "#A9A9A9", size = 0.2),
                axis.ticks = element_blank())

      }
    }else{
      if(tema == "escuro"){
        if(bolinha == TRUE){
          resultado <- ggplot(dados, aes(x = .data[[x]], y = .data[[y]])) + geom_line(color = cor, size = 1) +
            labs(title = titulo, x = eixo_x, y = eixo_y) + geom_point(color = cor) +
            ggdark::dark_theme_gray() +
            theme(plot.background = element_rect(fill = "grey10"),
                  panel.background = element_blank(),
                  panel.grid.major = element_line(color = "grey30", size = 0.2),
                  panel.grid.minor = element_line(color = "grey30", size = 0.2),
                  axis.ticks = element_blank())
        }else{
          resultado <- ggplot(dados, aes(x = .data[[x]], y = .data[[y]])) + geom_line(color = cor, size = 1) +
            labs(title = titulo, x = eixo_x, y = eixo_y) +
            ggdark::dark_theme_gray() +
            theme(plot.background = element_rect(fill = "grey10"),
                  panel.background = element_blank(),
                  panel.grid.major = element_line(color = "grey30", size = 0.2),
                  panel.grid.minor = element_line(color = "grey30", size = 0.2),
                  axis.ticks = element_blank())
        }
      }else{stop("Erro: tema inválido. O argumento 'tema' precisa estar necessariamente definida como 'claro' ou como 'escuro'.")}
    }
  }else{stop("Erro: Classe de um ou mais argumentos incorretos ou as colunas 'x' e 'y' não são do tipo numérico.")}
  if (anim == TRUE){
    resultado + transition_reveal(.data[[x]])
  }
  else{
    resultado
  }
}

