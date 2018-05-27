#' Plot topic correlation
#'
#' @param fit An ‘STM’ model object.
#'
#' @importFrom magrittr %>% extract2 set_names
#' @importFrom ggplot2 ggplot aes_string coord_cartesian geom_tile geom_text scale_fill_continuous labs theme_bw theme element_blank element_text
#' @importFrom tidyr gather
#' @importFrom dplyr group_by summarize n row_number
#' @importFrom stats cor
#'
#' @export
plot_corr <- function(fit) {

  thetas <-
    fit %>%
    extract2("theta") %>%
    as.data.frame()   %>%
    set_names(paste0("T", seq_len(length(.))))
  
  thetas_cor <-
    thetas %>%
    gather(topic_1, theta) %>%
    group_by(topic_1) %>%
    summarize(
      cor = list(sapply(thetas, cor, x = theta)),
      topic_2 = list(names(thetas))
    ) %>%
    unnest() %>%
    mutate(
      cor = ifelse(topic_1 == topic_2 | cor == 0, NA, cor),
      label = ifelse(topic_1 == topic_2, topic_1, round(cor, 1)),
      alpha = abs(cor)
    ) %>%
    filter(duplicated(cor) | topic_1 == topic_2)
  
  thetas_cor %>%
    ggplot(
      aes_string(
        x = "topic_1", y = "topic_2",
        fill = "cor", label = "label",
        alpha = "alpha")) +
    coord_cartesian() +
    geom_tile(colour = "white") +
    geom_text(size = 2) +
    scale_fill_continuous(
      low = "#3B9AB2",
      high = "orange",
      na.value = "white"
    ) +
    labs(
      x = "\nCorrelation between topics\n(pairwise pearson)",
      y = "\n"
    ) +
    theme_bw() +
    theme(
      panel.background = element_blank(),
      panel.grid.major = element_blank(),
      axis.title = element_text(size = 9),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      legend.position = "none"
    )  
}
