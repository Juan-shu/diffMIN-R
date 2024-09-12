#' Plot Dot Plot for diffMIN_object
#'
#' This function creates a dot plot of the dimensionality-reduced data in a `diffMIN_object` and colors the points based on a specified grouping from the metadata.
#'
#' @param diffMIN_object An object of class `diffMIN_object`.
#' @param reduction Character string specifying the reduction method to use. Options are "umap", "tsne", or "pca".
#' @param group.by Character string specifying the column name in `meta.data` to use for grouping.
#'
#' @return A ggplot object displaying the dot plot.
#' @export
#'
#' @examples
#' \dontrun{
#' library(diffMIN)
#' data(obj)
#' obj <- transform_EM_to_DM(obj)
#' obj <- RunReduc(diffMIN_object = obj,reduction = c("umap","tsne","pca"),seed = 1)
#' plotDot(obj, reduction = "tsne", group.by = "group")
#' }
plotDot <- function(diffMIN_object, reduction = c("umap", "tsne", "pca"), group.by = "group") {
  # Validate reduction argument
  reduction <- match.arg(reduction)

  # Prepare data
  if (reduction == "umap") {
    data <- diffMIN_object@reduction$umap
  }
  if (reduction == "tsne") {
    data <- diffMIN_object@reduction$tsne
  }
  if (reduction == "pca") {
    data <- diffMIN_object@reduction$pca
  }

  # Add grouping information
  if (!group.by %in% colnames(diffMIN_object@meta.data)) {
    stop(paste("Column", group.by, "not found in meta.data"))
  }
  data$group <- diffMIN_object@meta.data[[group.by]]

  # Define color palette
  color.pal <- c("#349ed9", "#25489d", "#6fc197", "#a6cd37", "#f6bd3d", "#d9376f",
                 "#903b6f", "#37a6cd", "#90d937", "#d94a37", "#cd37a6", "#bd6ff6")
  color.pal <- color.pal[sample(1:12,12)]

  unique_groups <- unique(data$group)

  # Check if palette length is sufficient
  if (length(unique_groups) > length(color.pal)) {
    warning("Number of groups exceeds the color palette. Custom colors should be specified.")
    color.pal <- rep(color.pal, length.out = length(unique_groups))
  } else {
    color.pal <- color.pal[1:length(unique_groups)]
  }

  # Plot
  p <- ggplot2::ggplot(data, ggplot2::aes(x = x, y = y, color = group)) +
    ggplot2::geom_point() +
    ggplot2::scale_color_manual(values = color.pal) +
    ggplot2::xlab("Dim 1") +
    ggplot2::ylab("Dim 2") +
    ggplot2::theme_test()

  return(p)
}

