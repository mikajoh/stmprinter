#' Alternative plotting of topic prevelance
#' 
#' @importFrom magrittr %>% extract2
#' @importFrom ggplot2 ggplot aes_string geom_text scale_x_continuous labs
#'   theme theme_bw element_rect element_blank element_text
#'   geom_segment
#' @importFrom tidyr gather
#' @importFrom dplyr group_by summarize n row_number arrange desc
#' @importFrom stats sd 
#'
#' @param fit An ‘STM’ model object.
#'
#' @export
plot_prev <- function(fit) {

  prev <-
    fit %>%
    extract2("theta") %>%
    as.data.frame() %>%
    gather(topic, prev, factor_key = TRUE) %>%
    arrange(desc(topic)) %>%
    group_by(topic) %>%
    summarize(
      est = mean(prev),
      se = sd(prev) / sqrt(n()),
      upr = est + (1.96 * se),
      lwr = est - (1.96 * se)
    ) %>%
    mutate(
      topic = row_number(),
      topic = factor(topic, rev(topic))
    )
  
  prev %>%
    ggplot(aes_string(x = "est", y = "topic")) +
    geom_segment(
      aes_string(yend = "topic", x = 0, xend = "est"),
      size = .5) +
    geom_point(size = 1) +
    scale_x_continuous(limits = c(0, max(prev$est + prev$upr))) +
    labs(
      x = "Topic Prevalence\n",
      y = "Topic"
    ) +
    theme_bw() +
    theme(
      axis.title = element_text(size = 9),
      axis.text =  element_text(size = 9)
    )
}
