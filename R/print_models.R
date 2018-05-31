#' Print a list of models to a multi-page pdf file.
#'
#' @param stm_models Either a list of stm model objects from
#'   \code{selectModels()} or the output of \code{manyTopics()}.
#' @param texts A character vector where each entry contains the text
#'   of a document. Must be in the same order as the documents
#'   object. NOTE: This is not the documents which are passed to ‘stm’
#'   and come out of ‘prepDocuments’, this is the actual text of the
#'   document.
#' @param file either a character string naming a file or a connection
#'   open for writing.
#' @param title Title to printed on top of the first page.
#' @param verbose If it should show a progress bar in the terminal.
#' 
#' @importFrom progress progress_bar
#' @importFrom gridExtra arrangeGrob grid.arrange
#' @importFrom grDevices pdf dev.off
#' 
#' @export
print_models <- function(stm_models,
                         texts,
                         file = "printed_stm.pdf",
                         title = NULL,
                         verbose = TRUE) {

  ## For model lists from the manyTopics function.
  if ("exclusivity" %in% names(stm_models)) {
    stm_models <- lapply(seq_along(stm_models$out), function(x) {
      list(
        runout = list(stm_models$out[[x]]),
        exclusivity = list(stm_models$exclusivity[[x]]),
        semcoh = list(stm_models$semcoh[[x]])
      )
    })
  }
  
  on.exit({
    if (!is.null(dev.list())) dev.off()
  })
  
  if (verbose) {    
    total <- 1 + sum(sapply(stm_models, function(x) length(x$runout)))
    pb <- progress_bar$new(
      format = "  printing [:bar] :percent eta: :eta",
      total = total, clear = TRUE
    )    
  }

  view <- get_overview(stm_models)
  stats <- get_stats(stm_models)

  pdf(
    file,
    width = 9,
    height = 11.79,
    paper = "a4"
  )

  if (verbose) pb$tick()

  ## The first page.
  p_header <- plot_main_header(title)
  p_semcoh_all <- plot_semcoh_all(stats)
  p_conv_all <- plot_conv_all(stm_models)
  grid.arrange(
    p_header, p_conv_all, p_semcoh_all,
    heights = c(3/40, 18.5/20, 18.5/20),
    ncol = 1
  )

  ## We want each run on its own page.
  for (n_model in 1:length(stm_models)) {
    for (n_run in 1:length(stm_models[[n_model]]$runout)) {

      fit <- stm_models[[n_model]]$runout[[n_run]]
      
      p_title  <- plot_title(fit, n_run)
      p_over <- plot_overview(fit, view, n_run)
      p_frex <- plot_frex(fit)
      p_semcoh <- plot_semcoh(stats, n_model, n_run)
      p_gini_theta <- plot_gini_theta(fit)
      p_prop <- plot_prev(fit)
      p_corr <- plot_corr(fit)
      p_texts <- plot_thoughts(fit, texts)

      p_left <- arrangeGrob(
        arrangeGrob(p_title, p_over, nrow = 1), p_frex,
        ncol = 1, heights = c(1/3, 2/3)
      )
      p_right <- arrangeGrob(
        p_corr, p_semcoh, p_prop, p_gini_theta,
        nrow = 2
      )
      p_top <- arrangeGrob(p_left, p_right, ncol = 2)

      p_out <- arrangeGrob(
        p_top, p_texts,
        ncol = 1, heights = c(3/12, 9/12)
      )

      suppressWarnings(grid.arrange(p_out))

      if (verbose) pb$tick()
    }
  }
  
  invisible(stm_models)
}
