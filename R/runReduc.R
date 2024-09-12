#' Run reduction for diffMIN_object
#'
#' @param diffMIN_object An object of class diffMIN_object.
#' @param reduction Methods to perform
#' @param slot Edge matrix or degree matrix, degree matrix is defaulted
#' @param seed A random seed for performing reduction
#' @param perplexity A parameter for tsne
#' @param ... Some parameters for umap
#'
#' @return An updated `diffMIN` object with reduction value added to the `reduction` slot.
#' @export
#'
#' @examples
#' \dontrun{
#' library(diffMIN)
#' data(obj)
#' obj <- transform_EM_to_DM(obj)
#' obj <- RunReduc(diffMIN_object = obj,reduction = c("umap","tsne","pca"),seed = 1)
#' }
RunReduc <- function(diffMIN_object,reduction = c("umap","tsne","pca"),
                     seed = 1, perplexity = 4, ...){
  # Prepare data
  data <- diffMIN_object@net.DM

  # Scale the data
  data <- data[,colSums(data) > 0]
  data <- scale(log2(data+1))
  data <- as.data.frame(data)
  data <- data[rowSums(abs(data)) > 0,]
  umap_result <- NULL
  tsne_result <- NULL
  pca_result <- NULL
  set.seed(seed)

  # Reduction
  if("umap" %in% reduction){
    umap_result <- umap::umap(t(data), ...)
    umap_result <- umap_result$layout
    colnames(umap_result) <- c("x","y")
    umap_result <- as.data.frame(umap_result)
    diffMIN_object@reduction$umap <- umap_result
  }

  if("tsne" %in% reduction){
    tsne_result <- Rtsne::Rtsne(t(data),dims = 2,pca = F,check_duplicates = F,
                                perplexity = perplexity)
    tsne_result <- tsne_result$Y
    colnames(tsne_result) <- c("x","y")
    tsne_result <- as.data.frame(tsne_result)
    diffMIN_object@reduction$tsne <- tsne_result
  }

  if("pca" %in% reduction){
    pca_result <- stats::prcomp(t(data),center = TRUE,scale. = F)
    pca_result <- pca_result$x[,c(1:2)]
    colnames(pca_result) <- c("x","y")
    pca_result <- as.data.frame(pca_result)
    diffMIN_object@reduction$pca <- pca_result
  }
  return(diffMIN_object)
}




