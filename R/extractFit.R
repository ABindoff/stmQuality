#' Extract topic quality statistics from a `manyTopics` fitted object
#'
#' Allows customisable semantic coherence, exclusivity, and theta parameters and returns
#' an easy to work with data frame
#' @param models A list returned by `stm::manyTopics`
#' @param documents The documents to be modeled. Object must be a list of with each element corresponding to a document. Each document is represented as an integer matrix w\ ith two rows, and columns equal to the number of unique vocabulary words in the document. The first row contains the 1-indexed vocabulary entry and the sec\ ond row contains the number of times that term appears. Corpora can be imported using the reader function and manipulated using the prepDocuments.
#' @param M The number of word tokens to calculate semantic coherence and exclusivity statistics from.
#' @param n The number of exemplars returned by `stmQuality::findThoughts0` to estimate thetas from.
#' @export
#' @return A data-frame with K, topics, semcoh, exclusivity, theta
#' @importFrom stm semanticCoherence
#' @importFrom stm exclusivity
#' @importFrom reshape2 melt
#' @examples
#'  fit <- manyTopics(out$documents, out$vocab, K = K,
#'  prevalence =~ cat * s(age), max.em.its = 100,
#'  data = out$meta, runs = 3, init.type = "Spectral", verbose = F)
#'  k <- extractFit(fit, M = 20, n = 5)
extractFit <- function(models, documents, M = 10, n = 20){
  m <- lapply(models$out, function(model) extractFit0(model, documents = documents, M = M, n = n))
  n <- sapply(m, function(x) list(semcoh = x$semcoh,
                                  exclusivity = x$exclusivity,
                                  theta = x$theta,
                                  K = x$K,
                                  topic = x$topic))
  x <- data.frame(melt(n[1,], value.name = "semcoh", level = "1"),
                  melt(n[2,], value.name = "exclusivity", level = "2"),
                  melt(n[3,], value.name = "theta", level = "3"),
                  melt(n[4,], value.name = "K", level = "4"),
                  melt(n[5,], value.name = "topic", level = "5"))
  x <- x[,c(7, 9, 1, 3, 5)]
  names(x) <- c("K", "topic", "semcoh", "exclusivity", "theta")
  x$K <- factor(x$K)
  x$topic <- factor(x$topic)
  x
}

extractFit0 <- function(model, documents, M = 10, n = 20){
  semcoh <- stm:::semanticCoherence(model, documents = documents, M = M)
  exclusivity <- stm:::exclusivity(model, M = M)
  thetas <- colMeans(as.data.frame(findThoughts0(model, n = n)$theta))
  K <- rep(model$settings$dim$K, model$settings$dim$K)
  return(list(K = K, semcoh = semcoh, exclusivity = exclusivity, theta = thetas, topic = c(1:K[1L])))
}
