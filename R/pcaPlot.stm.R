#' Visualise models returned by `stm::manyTopics`
#' @param x A data-frame returned by `extractFit`
#' @export
#' @import ggplot2
#' @import ggfortify
#' @import ggalt

pcaPlot.stm <- function(x){
  autoplot(prcomp(x[,3:5], center = T, scale. = T), data = x, colour = "K",
           group = "K",
           loadings = T,
           loadings.label = T) +
    geom_encircle(aes(colour = K))
}
