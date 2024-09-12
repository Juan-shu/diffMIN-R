#' Cluster Data in a `diffMIN` Object
#'
#' This function performs clustering on the dimensionality-reduced data stored in a `diffMIN` object.
#'
#' @param diffMIN_object An object of class `diffMIN` containing dimensionality-reduced data.
#' @param reduction.use A character string specifying which reduction method to use. Options are `"umap"`, `"tsne"`, or `"pca"`.
#' @param centers An integer specifying the number of clusters for the clustering algorithm.
#' @param method A character string specifying the clustering method. Options are `"hc"` for hierarchical clustering and `"kmean"` for k-means clustering.
#'
#' @return The modified `diffMIN` object with an additional column `cluster` in the `meta.data` slot, which contains the cluster assignments.
#' @export
#'
#' @examples
#' \dontrun{
#' library(diffMIN)
#' data(exp)
#' obj <- create_diffMIN_object(rawdata = exp)
#' obj <- cluster(diffMIN_object = obj, reduction.use = "umap", centers = 3, method = "kmean")
#' }
cluster <- function(diffMIN_object, reduction.use = c("umap", "tsne", "pca"), centers, method = c("hc", "kmean")) {

  # Validate arguments
  reduction.use <- match.arg(reduction.use)
  method <- match.arg(method)

  # Prepare data
  if (reduction.use == "umap") {
    data <- diffMIN_object@reduction$umap
  } else if (reduction.use == "tsne") {
    data <- diffMIN_object@reduction$tsne
  } else if (reduction.use == "pca") {
    data <- diffMIN_object@reduction$pca
  }

  # Perform clustering
  if (method == "hc") {
    hc <- stats::hclust(stats::dist(data, method = "euclidean"), method = "average")
    result <- stats::cutree(hc, k = centers)
  } else if (method == "kmean") {
    km <- stats::kmeans(data, centers = centers)
    result <- km$cluster
  }

  result <- factor(result, levels = unique(result))

  # Add clustering information to the meta.data
  diffMIN_object@meta.data$cluster <- result

  # Return the modified diffMIN object
  return(diffMIN_object)
}
