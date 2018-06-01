#' Performs several STM estimations for different number of topics in
#' parallel
#' 
#' In practice, runs stm::selectModel() for each provided K topics in
#' parallel. The function generally works the same way, except the
#' user specifies a range of numbers of topics that they want the
#' model fitted for and optionally how many cores to use. For example,
#' models with 3 to 12 topics. Then, for each number of topics,
#' \code{stm::selectModel()}, which performs several STM runs per
#' topic, is run. It retuns a list where each item is the
#' \code{stm::selectModel()} output for the K number of topics.
#'
#' @param K A vector of positive integers representing the desired number of
#' topics for separate runs of \code{stm::selectModel}.
#' @param documents The documents to be modeled.  Object must be a list of with
#' each element corresponding to a document.  Each document is represented as
#' an integer matrix with two rows, and columns equal to the number of unique
#' vocabulary words in the document.  The first row contains the 1-indexed
#' vocabulary entry and the second row contains the number of times that term
#' appears.
#' 
#' This is similar to the format in the \pkg{lda} package except that
#' (following R convention) the vocabulary is indexed from one. Corpora can be
#' imported using the reader function and manipulated using the
#' \code{\link{prepDocuments}}.
#' @param vocab Character vector specifying the words in the corpus in the
#' order of the vocab indices in documents. Each term in the vocabulary index
#' must appear at least once in the documents.  See
#' \code{\link{prepDocuments}} for dropping unused items in the vocabulary.
#' @param prevalence A formula object with no response variable or a matrix
#' containing topic prevalence covariates.  Use \code{s()}, \code{ns()} or
#' \code{bs()} to specify smooth terms. See details for more information.
#' @param content A formula containing a single variable, a factor variable or
#' something which can be coerced to a factor indicating the category of the
#' content variable for each document.
#' @param runs Total number of STM runs used in the cast net stage.
#' Approximately 15 percent of these runs will be used for running a STM until
#' convergence.
#' @param data Dataset which contains prevalence and content covariates.
#' @param init.type The method of initialization.  Must be either Latent
#' Dirichlet Allocation (LDA), Dirichlet Multinomial Regression Topic Model
#' (DMR), a random initialization or a previous STM object.
#' @param seed Seed for the random number generator. \code{stm} saves the seed
#' it uses on every run so that any result can be exactly reproduced.  Setting
#' the seed here simply ensures that the sequence of models will be exactly the
#' same when respecified.  Individual seeds can be retrieved from the component
#' model objects.
#' @param max.em.its The maximum number of EM iterations.  If convergence has
#' not been met at this point, a message will be printed.
#' @param emtol Convergence tolerance.  EM stops when the relative change in
#' the approximate bound drops below this level.  Defaults to .001\%.
#' @param verbose A logical flag indicating whether a progress bar
#'   should be printed to screen.
#' @param frexw Weight used to calculate exclusivity
#' @param net.max.em.its Maximum EM iterations used when casting the net
#' @param M Number of words used to calculate semantic coherence and
#' exclusivity.  Defaults to 10.
#' @param N Total number of models to retain in the end. Defaults to .2 of
#' runs.
#' @param to.disk Boolean. If TRUE, each model is saved to disk at the current
#' directory in a separate RData file.  This is most useful if one needs to run
#' \code{multiSTM()} on a large number of output models.
#' @param cores Number of CPU cores to use for parallel
#'   computation. Defaults to the number of cores available.
#' @param \dots Additional options described in details of \code{stm::stm}.
#' @return 
#'
#' It returns a list with length equal to K (i.e., one model for each
#'   provide K topics) of \code{stm::selectModel()} outputs. See
#'   \code{?stm::selectModel}.
#'
#' @importFrom parallel mclapply detectCores
#' @importFrom stm selectModel
#' 
#' @examples
#' 
#' \dontrun{
#' processed <- textProcessor(
#'   documents = gadarian$open.ended.response,
#'   metadata = gadarian
#' )
#' out <- prepDocuments(
#'   documents = processed$documents,
#'   vocab = processed$vocab,
#'   meta = processed$meta
#' )
#' 
#' set.seed(02138)
#' 
#' stm_models <- many_models(
#'   K = 3:12,
#'   documents = out$documents,
#'   vocab= out$vocab,
#'   prevalence = ~ treatment + s(pid_rep), 
#'   data = out$meta,
#'   runs = 5
#' )
#' 
#' # To select a particular model to work one, just extract it by
#' # index. For instance, to extract the first run of the model with 3
#' # topics, you can run the following.
#'
#' fit <- stm_models[[1]]$runout[[1]]
#' 
#' plot(fit)
#' }
#' 
#' @export
many_models <- function(K, documents, vocab,
                        prevalence = NULL, content = NULL, data = NULL,
                        max.em.its=100, init.type = "LDA",
                        emtol = 1e-05, seed = NULL, runs = 50,
                        frexw = .7, net.max.em.its = 2, M = 10,
                        N = NULL, to.disk = FALSE, verbose = TRUE,
                        cores = parallel::detectCores(),
                        ...) {

  args <- list(
    documents = documents,
    vocab = vocab,
    prevalence = prevalence,
    content = content,
    data = data, 
    max.em.its = max.em.its,
    init.type = init.type,
    emtol = emtol,
    seed = seed,
    runs = runs,
    frexw = frexw,
    net.max.em.its = net.max.em.its,
    M = M,
    N = N,
    to.disk = to.disk,
    verbose = FALSE
  )
  
  if (verbose) {    
    pb <- progress_bar$new(
      format = " Running multiple STM models [:bar] :current of :total (:percent) finished ",
      total = length(K), clear = TRUE, show_after = 0
    )    
  }

  selectModel2 <- function(K, args, verbose, pb, ...) {
    out <- do.call("selectModel", c(K, args, ...))
    if (verbose) pb$tick()
    out
  }

  if (verbose) pb$tick(0)
  
  models <- mclapply(
    K, selectModel2, args, verbose, pb,
    mc.cores = cores, mc.silent = TRUE
  )
  
  if (verbose) pb$tick(length(K))
  
  names(models) <- paste0("k", K)
  class(models) <- "many_models"
  
  models
}
