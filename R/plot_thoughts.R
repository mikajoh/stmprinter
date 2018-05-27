#' Alternative plotting of most representative texts
#'
#' @param fit An ‘STM’ model object.
#' @param texts A character vector where each entry contains the text
#'   of a document.  Must be in the same order as the documents
#'   object. NOTE: This is not the documents which are passed to ‘stm’
#'   and come out of ‘prepDocuments’, this is the actual text of the
#'   document.
#' @param split_width The character width to split texts/thoughts into
#'   a new line.
#' @param n_sentences The number of sentences to print per topic.
#' @param cutoff The number of characters before the text/thought
#'   should be cut off with \code{(...)}.
#'
#' @importFrom stm findThoughts labelTopics
#' @importFrom magrittr %>% extract2
#' @importFrom ggplot2 ggplot aes_string geom_text scale_x_continuous
#'   labs theme element_rect element_blank element_text theme_bw
#'
#' @export
plot_thoughts <- function(fit, texts,
                          split_width = 62,
                          n_sentences = 25,
                          cutoff = 190) {

  frex <- fit %>%
    labelTopics(n = 3) %>%
    extract2("frex") %>%
    apply(1, paste0, collapse = ", ")
  
  thoughts <- lapply(1:ncol(fit$theta), function(x) {

    thoughts_raw <-
      findThoughts(
        model = fit,
        texts = texts,
        n = n_sentences,
        topics = x
      ) %>%
      extract2("docs") %>%
      extract2(1)

    thoughts <-
      paste0(seq_len(n_sentences), ": ", thoughts_raw) %>%
      cut_thoughts(cutoff = cutoff) %>%
      strwrap(width = split_width, exdent = 5) %>%
      paste0(collapse = "\n")

    data.frame(
      topic = x,
      topic_label = paste0("Topic ", x, ": ", frex[x]),
      text = thoughts
    )    
  })
  thoughts <- do.call("rbind", thoughts)

  thoughts %>%
    ggplot(aes_string(x = 0, y = 1, label = "text")) +
    facet_wrap(~ topic_label, ncol = 3) +
    geom_text(hjust = 0, vjust = 1, size = 2.5) +
    scale_x_continuous(limits = c(0, 1), expand = c(.01, .01)) +
    scale_y_continuous(limits = c(0, 1), expand = c(.01, .01)) +
    theme(
      panel.background = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      axis.text = element_blank(),
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      strip.background = element_rect(
        colour = "grey",
        fill = "grey",
        size = 3
      )
    )
}

cut_thoughts <- function(texts, cutoff = 190) {
  cuts <- nchar(texts) >= cutoff
  adds <- rep("", length(texts))
  adds[cuts] <- "(...)"
  out <- paste0(substr(texts, 1, cutoff), adds)
  out
}

