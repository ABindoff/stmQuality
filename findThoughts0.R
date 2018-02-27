#' Find Thoughts0
#' 
#' Outputs most representative documents for a particular topic. Use this in
#' order to get a better sense of the content of actual documents with a high
#' topical content.
#' 
#' Returns the top \code{n} documents ranked by the MAP estimate of the topic's
#' theta value (which captures the modal estimate of the proportion of word
#' tokens assigned to the topic under the model). Setting the \code{thresh}
#' argument allows the user to specify a minimal value of theta for returned
#' documents. Returns document indices and top thoughts.
#' 
#' Sometimes you may want to find thoughts which have more conditions than simply 
#' a minimum threshold.  For example, you may want to grab all documents which satisfy
#' certain conditions on the metadata or other topics.  You can supply a query in the
#'  style of \pkg{data.table} to the \code{where} argument.  Note that in \code{data.table}
#'   variables are referenced by their names in the \code{data.table} object.  The topics
#'   themselves are labeled \code{Topic1}, \code{Topic2} etc.  If you supply the metadata
#'    to the \code{meta} argument, you can also query based on any available metadata. 
#'     See below for examples.
#'
#' If you want to pass even more complicated queries, you can use the function \code{\link{make.dt}} 
#' to generate a \code{data.table} object where you can write your own queries.
#'
#' The \code{plot.findThoughts} function is a shortcut for the \code{plotQuote}
#' function.
#' 
#' @aliases findThoughts0
#' @param model Model object created by \code{stm}.
#' @param texts A character vector where each entry contains the text of a
#' document.  Must be in the same order as the documents object. NOTE: This is not the
#' documents which are passed to \code{stm} and come out of \code{prepDocuments}, 
#' this is the actual text of the document.
#' @param topics The topic number or vector of topic numbers for which you want
#' to find thoughts.  Defaults to all topics.
#' @param n The number of desired documents to be displayed per topic.
#' @param thresh Sets a minimum threshold for the estimated topic proportion
#' for displayed documents.  It defaults to imposing no restrictions.
#' @param where An expression in the form of a \code{data.table} query. This is passed to the \code{i} argument in data.table and a custom query is passed to \code{j}.  This cannot be used with \code{thresh}.  See below for more details.
#' @param meta The meta data object to be used with \code{where}.
#' @return A \code{findThoughts} object:
#' \item{index}{List with one entry per
#' topic.  Each entry is a vector of document indices.} 
#' \item{docs}{List with
#' one entry per topic.  Each entry is a character vector of the corresponding
#' texts.}
#' @seealso \code{\link{plotQuote}}
#' @examples
#' findThoughts0(gadarianFit, texts=gadarian$open.ended.response, topics=c(1,2), n=3)
#' 
#' #We can plot findThoughts objects using plot() or plotQuote
#' thought <- findThoughts(gadarianFit, texts=gadarian$open.ended.response, topics=1, n=3)
#' 
#' #plotQuote takes a set of sentences
#' plotQuote(thought$docs[[1]])
#' 
#' #we can use the generic plot as a shorthand which will make one plot per topic
#' plot(thought)
#' 
#' #we can select a subset of examples as well using either approach
#' plot(thought,2:3)
#' plotQuote(thought$docs[[1]][2:3])
#' 
#' 
#' #gather thoughts for only treated documents
#' thought <- findThoughts(gadarianFit, texts=gadarian$open.ended.response, topics=c(1,2), n=3, 
#'                        where = treatment==1, meta=gadarian)
#' plot(thought)
#' #you can also query in terms of other topics
#' thought <- findThoughts(gadarianFit, texts=gadarian$open.ended.response, topics=c(1), n=3, 
#'                         where = treatment==1 & Topic2>.2, meta=gadarian)
#' plot(thought)         
#' #these queries can be really complex if you like
#' thought <- findThoughts(gadarianFit, texts=gadarian$open.ended.response, topics=c(1), n=3, 
#'                        where = (treatment==1 | pid_rep > .5) & Topic3>.2, meta=gadarian)
#' plot(thought)         
#' @export
#' @import data.table

findThoughts0 <- function (model, texts = NULL, topics = NULL, n = 3, thresh = 0) 
{
  theta <- model$theta
  if (is.null(topics)) 
    topics <- 1:ncol(theta)
  if (!is.null(texts) && length(texts) != nrow(theta)) 
    stop("Number of provided texts and number of documents modeled do not match")
  if (n > nrow(theta)) 
    n <- nrow(theta)
  if (n < 1) 
    stop("Must request at least one returned document.")
  out <- list()
  for (i in 1:length(topics)) {
    k <- topics[i]
    index <- order(theta[, k], decreasing = TRUE)[1:n]
    val <- sort(theta[, k], decreasing = TRUE)[1:n]
    index <- index[which(val >= thresh)]
    out$index[[i]] <- index
    out$theta[[i]] <- val[val >= thresh]
    if (!is.null(texts)) 
      out$docs[[i]] <- texts[index]
  }
  names(out$index) <- paste("Topic", topics)
  names(out$theta) <- paste("Topic", topics)
  if (!is.null(texts)) 
    names(out$docs) <- paste("Topic", topics)
  class(out) <- "findThoughts"
  if (is.null(texts)) 
    return(out)
  out$docs <- lapply(out$docs, unlist)
  if (is.factor(texts)) {
    warning("texts are of type 'factor.'  Converting to character vectors.  Use 'as.character' to avoid this warning in the future.")
    out$docs <- lapply(out$docs, as.character)
  }
  return(out)
}