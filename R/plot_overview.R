#' Get overview of topics/runs
#' 
#' @param stm_models A list of stm model objects from
#'   \code{stm::selectModels()}.
#' 
get_overview <- function(stm_models) {
  view <- lapply(stm_models, function(x) {
    data.frame(
      topic = ncol(x$runout[[1]]$theta),
      run = 1:length(x$runout)
    )
  })
  do.call("rbind", view)
}

#' Plot the overview used in plot_models
#'
#' @param fit A stm model object.
#' @param overview The output from \code{get_overview()}, which in
#'   turn where fed a list of stm model object from
#'   \code{selectModels()}.
#' @param run_n_current Current run to highlight.
#' 
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate case_when
#' @importFrom ggplot2 ggplot aes_string geom_tile labs theme_minimal theme
#'   element_blank element_text unit
#' 
plot_overview <- function(fit, overview, run_n_current) {

  view <-
    overview %>%
    mutate(
      colour = case_when(
        topic == ncol(fit$theta) & run == run_n_current ~ "orange",
        topic == ncol(fit$theta)                        ~ "grey40",
        run == run_n_current                            ~ "grey40",
        TRUE                                            ~ "grey80"
      ),
      topic = factor(topic),
      run = factor(run, rev(unique(run)))
    )

  view %>%
    ggplot(
      aes_string(
        x = "topic", y = "run",
        fill = "colour", colour = "colour")
    ) +
    geom_tile(colour = "grey", fill = view$colour) +
    labs(x = "N Topics", y = "Run") +
    theme_minimal() +
    theme(
      axis.ticks = element_blank(),
      axis.ticks.length = unit(0.3, "mm"),
      axis.title = element_text(size = 9),
      axis.text =  element_text(size = 9)
    )  
}

#' Plot the title used in plot_models
#'
#' @param fit A stm model object.
#' @param run_n_current Current run.
#' 
#' @importFrom magrittr %>%
#' @importFrom ggplot2 ggplot aes_string geom_text theme_void
plot_title <- function(fit, run_n_current) {

  title <- paste0(
    "N Topics: ", ncol(fit$theta), "\n",
    "      Run: ", run_n_current
  )
  
  data.frame(title = title) %>%
    ggplot(aes_string(x = 0, y = 0, label = "title")) +
    geom_text(size = 6) +
    theme_void()
}

#' Plot the header on the first in plot_models.
#'
#' @param title Title to print over session info.
#' 
#' @importFrom magrittr %>%
#' @importFrom ggplot2 ggplot aes aes_string geom_text geom_hline
#'   scale_x_continuous scale_y_continuous theme_void
#' 
plot_main_header <- function(title = NULL) {  
  header_text <- paste(
    "user:", Sys.getenv("USER"),
    "| platform:", R.Version()$platform,
    "| printed:", Sys.time()
  )
  header <- data.frame(
    label = header_text,
    x = 0, y = .2,
    face = "plain"
  )
  if (!is.null(title)) {  
    add <- data.frame(
      label = title,
      x = 0, y = .8,
      face = "bold")
    header <- rbind(add, header)
  }  
  ggplot(header, aes_string(x = "x", y = "y", label = "label")) +
    geom_text(aes(fontface = face), hjust = 0) +
    geom_hline(aes(yintercept = -.2)) +
    scale_x_continuous(limits = c(0, 1)) +
    scale_y_continuous(limits = c(-.2, 1)) +
    theme_void()  
}
