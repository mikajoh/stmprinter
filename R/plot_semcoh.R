#' Extract semantic coherence and exclusivity form a list of models
#'
#' @param stm_models A list of stm model object created by
#'   \code{selectModels()}.
#'
#' @importFrom magrittr %>% extract2
#' @importFrom dplyr data_frame mutate
#' @importFrom tidyr unnest
#' 
#' @export
get_stats <- function(stm_models) {
  stats <- lapply(seq_along(stm_models), function(x) {
    stm_mod <- stm_models[[x]]    
    stat <- data_frame(
      model = x,
      n_topics = length(stm_mod$exc[[1]]),
      semcoh = stm_mod$semcoh,
      exc = ifelse(
        stm_mod$exclusivity == "Exclusivity not calculated for models with content covariates", 0,
        stm_mod$exclusivity
      ),
      run = 1:length(exc),
      topic = rep(list(1:length(semcoh[[1]])), length(semcoh))
    )
    stat %>%
      unnest() %>%
      mutate(run = factor(run))    
  })
  do.call("rbind", stats)
}

#' Plot semantic coherence and exclusivity for one model.
#'
#' @param stats Output from \code{get_stats()}, which in turn were fed
#'   a list of stm model object created by \code{selectModels()}.
#' @param mod Which n model to print (corresponding to it's place in
#'   the list of model for \code{get_stats()}.
#' @param n_run = Which run of the model to highlight.
#'
#' @importFrom dplyr filter mutate
#' @importFrom ggplot2 ggplot aes_string scale_x_continuous
#'   scale_y_continuous labs guides theme_bw theme element_text
#'   geom_point geom_text
#' @importFrom magrittr %>% extract2
#' 
#' @export
plot_semcoh <- function(stats, mod, 
                        n_run = NULL) {

  fig <- stats %>%
    filter(model == mod) %>%
    ggplot(
      aes_string(
        x = "semcoh", y = "exc",
        label = "topic")) +
    scale_x_continuous(
      limits = c(min(stats$semcoh), max(stats$semcoh))) +
    scale_y_continuous(
      limits = c(min(stats$exc), max(stats$exc))) +
    labs(
      x = "Semantic Coherence\n(by topic)",
      y = "Exclusivity") +
    guides(colour = FALSE) +
    theme_bw() +
    theme(
      axis.title = element_text(size = 9),
      axis.text =  element_text(size = 9)
    )

  if (!is.null(n_run)) {
    stats_run <-
      stats %>%
      filter(model == mod) %>%
      mutate(
        highlight = ifelse(run == n_run, "orange", "grey"),
        alpha = ifelse(run == n_run, 1, .5)
      )
    highlight <- stats_run %>% extract2("highlight")
    alph <- stats_run %>% extract2("alpha")
    fig <- fig + geom_text(
      colour = highlight,
      alpha = alph,
      fontface = "bold"
    )
  } else {
    fig <- fig + geom_point()
  }

  fig  
}

#' Plot semantic coherence for all stm models
#'
#' @param stats Output from \code{get_stats()}, which in turn were fed
#'   a list of stm models.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom ggplot2 ggplot aes_string facet_wrap geom_point labs
#'   theme_bw theme
#' 
#' @export
plot_semcoh_all <- function(stats) {
  stats %>%
    mutate(
      facet = paste("Topic", n_topics),
      facet = factor(facet, unique(facet))
    ) %>%
    ggplot(aes_string(x = "semcoh", y = "exc", shape = "run")) +
    facet_wrap(~ facet) +
    geom_point() +
    labs(
      x = "Semantic coherence",
      y = "Exclusivity",
      shape = "Run"
    ) +
    theme_bw() +
    theme(legend.position = c(.75, .15))
}
