#' Select background cells (or samples) and assign groups
#'
#' This function classifies cells (or samples) into background ('bg') and foreground ('fg') groups based on the provided column names.
#'
#' @param diffMIN_object A `diffMIN_object` S4 object containing raw data and metadata.
#' @param group A character vector specifying the column names of background cells (or samples).
#'
#' @return The updated `diffMIN_object` S4 object with an additional `group` column in `meta.data`, indicating whether each cell (or sample) is a background ('bg') or foreground ('fg').
#' @export
#'
#' @examples
#' \dontrun{
#' library(diffMIN)
#' library(stringr)
#' data(exp)
#' obj <- create_diffMIN_object(rawdata = exp)
#' group <- colnames(exp)[str_detect(colnames(exp), "Sham")]
#' obj <- addGroup(obj, group)
#' }
addGroup <- function(diffMIN_object, group) {
  add.group <- ifelse(colnames(diffMIN_object@rawdata) %in% group, "bg", "fg")
  diffMIN_object@meta.data$group <- add.group
  return(diffMIN_object)
}

