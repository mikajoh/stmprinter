#' Plot convergence for a list of stm models
#'
#' @param stm_models A list of stm model (from \code{selectModels()}).
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#'
#' @importFrom ggplot2 ggplot aes_string facet_wrap geom_line
#'   scale_colour_manual labs theme_bw theme
#'
#' @export
plot_conv_all <- function(stm_models) {
  
  get_conv <- function(y, mod) {
    .mod <- mod$runout[[y]]
    data.frame(
      topics = ncol(.mod$theta),
      run = y,
      bound = .mod$convergence$bound,
      iter = 1:length(.mod$convergence$bound),
      conv = ifelse(
        .mod$convergence$converged,
        "Converged", "Did not converge"
      )
    )
  }

  conv <- lapply(stm_models, function(mod) {
    conv_inner <- lapply(seq_along(mod$runout), get_conv, mod = mod)
    do.call("rbind", conv_inner)
  })
  conv <- do.call("rbind", conv)

  colors <- c("grey35", "#D55E00")
  names(colors) <- c("Converged", "Did not converge")

  conv %>%
    mutate(
      run = factor(run),
      facet = paste("Topic", topics),
      facet = factor(facet, unique(facet)),
      conv = factor(conv, c("Converged", "Did not converge"))
    ) %>%
    ggplot(
      aes_string(
        x = "iter", y = "bound",
        group = "run",
        linetype = "run",
        colour = "conv")
    ) +
    facet_wrap(~ facet) +
    geom_line() +
    scale_colour_manual(name = "conv", values = colors) +
    labs(
      x = "Convergence (iterations)",
      y = "Approximate Objective",
      colour = NULL,
      linetype = "Run"
    ) +
    theme_bw() +
    theme(legend.position = c(.75, .15))
}
