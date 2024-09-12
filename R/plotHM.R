#' Plot Heatmap for diffMIN_object
#'
#' This function generates a heatmap based on the specified type of data from a `diffMIN_object`.
#'
#' @param diffMIN_object An object of class `diffMIN_object`.
#' @param scale Logical indicating whether to scale the data.
#' @param group.by Character string specifying the column in `meta.data` to use for grouping.
#' @param type Character string specifying the type of data to plot. Options are "degree", "exp", or "edge".
#'
#' @return A heatmap plot created using `ComplexHeatmap`.
#' @export
#'
#' @examples
#' \dontrun{
#' library(diffMIN)
#' data(obj)
#' obj <- transform_EM_to_DM(obj)
#' obj <- RunReduc(diffMIN_object = obj, reduction = c("umap", "tsne", "pca"), seed = 1)
#' plotHM(obj, group.by = "group", type = "exp")
#' }
plotHM <- function(diffMIN_object, group.by = "group", type = "degree", scale = TRUE) {
  # Validate type argument
  type <- match.arg(type, choices = c("degree", "exp", "edge"))

  # Prepare data
  if (type == "degree") {
    exp <- diffMIN_object@net.DM
  } else if (type == "exp") {
    exp <- diffMIN_object@rawdata
  } else if (type == "edge") {
    exp <- do.call(rbind, diffMIN_object@net.EM)
  }

  # Scale data if specified
  if (scale) {
    exp <- exp[rowSums(exp) > 0, , drop = FALSE]
    exp <- t(scale(t(exp)))
  }

  # Group information
  if (!group.by %in% colnames(diffMIN_object@meta.data)) {
    stop(paste("Column", group.by, "not found in meta.data"))
  }

  group <- factor(diffMIN_object@meta.data[[group.by]], levels = unique(diffMIN_object@meta.data[[group.by]]))

  # Color palette
  color.pal <- c("#349ed9", "#25489d", "#6fc197", "#a6cd37", "#f6bd3d", "#d9376f",
                 "#903b6f", "#37a6cd", "#90d937", "#d94a37", "#cd37a6", "#bd6ff6")
  color.pal <- color.pal[sample(1:12,12)]

  # Create heatmap
  if (length(unique(group)) > length(color.pal)) {
    warning("Number of groups exceeds the color palette. Consider customizing colors.")
    ComplexHeatmap::Heatmap(exp,
                            show_column_names = FALSE,
                            show_row_names = FALSE,
                            cluster_columns = FALSE,
                            width = grid::unit(6, "cm"),
                            height = grid::unit(8, "cm"),
                            heatmap_legend_param = list(title = type))
  } else {
    n_groups <- length(unique(group))
    colors <- color.pal[1:n_groups]
    named_colors <- setNames(colors, levels(group))

    row_annotation <- ComplexHeatmap::HeatmapAnnotation(
      df = data.frame(Group = group),
      col = list(Group = named_colors),
      annotation_legend_param = list(title = group.by)  # Update legend title to reflect grouping
    )

    ComplexHeatmap::Heatmap(exp,
                            show_column_names = FALSE,
                            show_row_names = FALSE,
                            cluster_columns = FALSE,
                            width = grid::unit(6, "cm"),
                            height = grid::unit(8, "cm"),
                            top_annotation = row_annotation,
                            heatmap_legend_param = list(title = type))
  }
}
