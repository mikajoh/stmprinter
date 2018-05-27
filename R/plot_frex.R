#' Alternative plotting of FREX
#'
#' @param fit An ‘STM’ model object.
#' @param n Number of words to print.
#'
#' @importFrom stm labelTopics
#' @importFrom magrittr %>% extract2
#' @importFrom ggplot2 ggplot aes_string geom_text scale_x_continuous labs
#'   theme_bw theme element_blank element_text element_rect
#'
#' @export
plot_frex <- function(fit, n = 15) {

  frex <- fit %>%
    labelTopics(n = n) %>%
    extract2("frex") %>%
    apply(1, paste, collapse = ", ")

  frex_df <- data.frame(
    x = 0,
    topic = factor(1:ncol(fit$theta), rev(1:ncol(fit$theta))),
    label = frex
  )
  
  frex_df %>%
    ggplot(aes_string(x = "x", y = "topic", label = "label")) +
    geom_text(hjust = 0, size = 2.5) +
    scale_x_continuous(limits = c(0, 1), expand = c(.01, .01)) +
    labs(x = "Frex", y = "Topic") +
    theme_bw() +
    theme(
      panel.background = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_text(size = 9),
      axis.line = element_blank(),
      axis.ticks.x = element_blank(),
      strip.background = element_rect(
        colour = "grey",
        fill = "grey",
        size = 3
      )
    )
}
