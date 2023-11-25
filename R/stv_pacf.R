#' @title Gráfico da Função de Autocorrelação Parcial de uma Série Temporal.
#'
#' @param vetor Vetor contendo os valores da série. Precisa ser numérica.
#' @param cor String contendo a cor do gráfico, podendo ser no formato hexadecimal.
#' @param bolinha Argumento do tipo lógico. \code{bolinha = TRUE} retorna o PACF em um gráfico lollipop.
#' @param linha Valor numérico indicando a expessura da linha.
#' @param lagg Defasagem máxima para o cálculo do ACF.
#' @param tema String contendo o tema do gráfico. Precisa ser necessariamente 'claro' ou 'escuro'.
#'
#' @return Um gráfico da função de autocorrelação parcial de uma série temporal.
#'
#' @examples
#' library(stv)
#' library(ggdark)
#' library(ggplot2)
#' teste <- data.frame(data = seq(1,100), valor = rnorm(100))
#' stv_pacf(teste$valor)
#' stv_pacf(teste$valor, "magenta", TRUE)
#' stv_pacf(teste$valor, linha = 2, lag = 10, tema = "escuro")
#'
#' @import tidyverse
#' @import ggplot2
#' @import ggdark
#'
#' @export
stv_pacf <- function(vetor, cor = "purple", bolinha = FALSE, linha = 1, lagg = 20, tema = "claro"){
  if(class(vetor) %in% c("numeric", "integer") & class(cor) == "character" & class(bolinha) == "logical" &
     class(linha) == "numeric" & class(lagg) == "numeric" & class(tema) == "character"){
    pacf = pacf(vetor, plot = FALSE, lag.max = lagg)
    pacf_dataframe = with(pacf, data.frame(lag, acf))
    limite_superior_pacf = qnorm((1 + (1 - 0.05))/2)/sqrt(pacf$n.used)
    limite_inferior_pacf = -limite_superior_pacf
    if(tema == "claro"){
      if(bolinha == FALSE){
        ggplot(pacf_dataframe, aes(x = lag, y = acf)) +
          geom_hline(yintercept = 0, color = "black") +
          geom_hline(yintercept = limite_superior_pacf, linetype = 2, color = "black") +
          geom_hline(yintercept = limite_inferior_pacf, linetype = 2, color = "black") +
          geom_segment(aes(x = lag, xend = lag, y = 0, yend = acf), color = cor, size = linha) +
          labs(title = "PACF") +
          theme(plot.background = element_rect(fill = "#D3D3D3"),
                panel.background = element_blank(),
                panel.grid.major = element_line(color = "#A9A9A9", size = 0.2),
                panel.grid.minor = element_line(color = "#A9A9A9", size = 0.2),
                axis.ticks = element_blank())
      }else{
        ggplot(pacf_dataframe, aes(x = lag, y = acf)) +
          geom_hline(yintercept = 0, color = "black") +
          geom_hline(yintercept = limite_superior_pacf, linetype = 2, color = "black") +
          geom_hline(yintercept = limite_inferior_pacf, linetype = 2, color = "black") +
          geom_point(size = 3.5, color = cor, fill = alpha(cor, 0.1), alpha = 0.5, shape = 21, stroke = 1.5) +
          geom_segment(aes(x = lag, xend = lag, y = 0, yend = acf), color = cor, size = linha) +
          labs(title = "ACF") +
          theme(plot.background = element_rect(fill = "#D3D3D3"),
                panel.background = element_blank(),
                panel.grid.major = element_line(color = "#A9A9A9", size = 0.2),
                panel.grid.minor = element_line(color = "#A9A9A9", size = 0.2),
                axis.ticks = element_blank())
      }
    }else{
      if(tema == "escuro"){
        if(bolinha == FALSE){
          ggplot(pacf_dataframe, aes(x = lag, y = acf)) +
            geom_hline(yintercept = 0, color = "white") +
            geom_hline(yintercept = limite_superior_pacf, linetype = 2, color = "white") +
            geom_hline(yintercept = limite_inferior_pacf, linetype = 2, color = "white") +
            geom_segment(aes(x = lag, xend = lag, y = 0, yend = acf), color = cor, size = linha) +
            labs(title = "PACF") +
            dark_theme_gray() +
            theme(plot.background = element_rect(fill = "grey10"),
                  panel.background = element_blank(),
                  panel.grid.major = element_line(color = "grey30", size = 0.2),
                  panel.grid.minor = element_line(color = "grey30", size = 0.2),
                  axis.ticks = element_blank())
        }else{
          ggplot(pacf_dataframe, aes(x = lag, y = acf)) +
            geom_hline(yintercept = 0, color = "white") +
            geom_hline(yintercept = limite_superior_pacf, linetype = 2, color = "white") +
            geom_hline(yintercept = limite_inferior_pacf, linetype = 2, color = "white") +
            geom_point(size = 3.5, color = cor, fill = alpha(cor, 0.1), alpha = 0.5, shape = 21, stroke = 1.5) +
            geom_segment(aes(x = lag, xend = lag, y = 0, yend = acf), color = cor, size = linha) +
            labs(title = "PACF") +
            dark_theme_gray() +
            theme(plot.background = element_rect(fill = "grey10"),
                  panel.background = element_blank(),
                  panel.grid.major = element_line(color = "grey30", size = 0.2),
                  panel.grid.minor = element_line(color = "grey30", size = 0.2),
                  axis.ticks = element_blank())
        }
      }else{stop("Erro: tema inválido. O argumento 'tema' precisa estar necessariamente definida como 'claro' ou como 'escuro'.")}
    }
  }else{stop("Erro: Classe de um ou mais argumentos incorretos.")}
}
