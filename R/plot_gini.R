#' Plot gini of documents within topics
#'
#' @param fit An \code{STM} model object.
#'
#' @importFrom magrittr %>%
#' @importFrom ggplot2 ggplot aes_string geom_segment geom_point
#'   scale_x_continuous labs theme_bw theme element_text
#'
#' 
#' @export
plot_gini_theta <- function(fit) {

  gini_df <- data.frame(
    gini = apply(fit$theta, 2, calc_gini),
    topic = factor(1:ncol(fit$theta), rev(1:ncol(fit$theta)))
  )

  gini_df %>%
    ggplot(aes_string(x = "gini", y = "topic")) +
    geom_segment(
      aes_string(yend = "topic", x = 0, xend = "gini"),
      size = .5) +
    geom_point(size = 1) +
    scale_x_continuous(limits = c(0, .75)) +
    labs(
      x = "Gini (inequality) of documents\nwithin topics",
      y = "Topic"
    ) +
    theme_bw() +
    theme(
      axis.title = element_text(size = 9),
      axis.text =  element_text(size = 9)
    )
}

#' Plot gini coefficient of topics within documents
#'
#' @param fit An \code{STM} model object.
#'
#' @importFrom magrittr %>%
#' @importFrom ggplot2 ggplot aes_string geom_density geom_vline labs
#'   theme_bw theme element_text
#' 
#' @export
plot_gini_docs <- function(fit) {

  gini_df <- data.frame(gini = apply(fit$theta, 1, gini))
  
  gini_df %>%
    ggplot(aes(x = "gini")) +
    geom_density(fill = "grey", alpha = .5) +
    geom_vline(
      aes(xintercept = mean(gini)),
      linetype = "dashed",
      colour = "orange"
    ) +
    labs(x = "Gini (inequality) of topics\nwithin documents", y = "Density") +
    theme_bw() +
    theme(
      axis.title = element_text(size = 9),
      axis.text =  element_text(size = 9)
    )
  
}

#' Calculate gini coefficient
#'
#' @param x Values to calculate gini for.
#'
#' @importFrom stats na.omit
#'
calc_gini <- function(x) {
  x <- as.numeric(na.omit(x))
  x <- sort(x)
  n <- length(x)  
  gini <- sum(x * 1:n)
  gini <- 2 * gini / sum(x) - (n + 1L)

  gini / n
}
