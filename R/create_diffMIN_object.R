#' Create a diffMIN_object S4 Object
#'
#' This function creates an S4 object of class `diffMIN_object`.
#'
#' @param rawdata A matrix containing the raw expression data.
#' @param dat A matrix containing the discretized expression data.
#' @param net.DM A data frame representing the diffMIN degree matrix.
#' @param net.EM A list containing the diffMIN edge matrices. Should include elements `bg` and `fg`, each being a list.
#' @param reduction A list containing dimensionality reduction results (e.g., UMAP, PCA, t-SNE).
#' @param meta.data A data frame containing metadata associated with the samples.
#' @param IN A list containing intermediate network data used for final network calculations.
#'
#' @return An object of class `diffMIN_object`.
#' @export
#' @examples
#' \dontrun{
#' data(exp)
#' exp <- pre_process(exp)
#' obj <- create_diffMIN_object(rawdata = exp)
#' print(obj)
#' }
create_diffMIN_object <- function(rawdata, dat = data.frame(),
                                  net.DM = data.frame(),
                                  net.EM = list(bg = list(),
                                                fg = list()),
                                  reduction = list(),
                                  meta.data = data.frame(),
                                  IN = list(bg.all.expr = list(),
                                            fg.all.expr = list(),
                                            bg.ran.net.up = list(),
                                            bg.ran.net.down = list(),
                                            fg.ran.net.up = list(),
                                            fg.ran.net.down = list())) {
  orig.meta.data <- data.frame(ID = colnames(rawdata),row.names = colnames(rawdata))
  if(!(nrow(meta.data) == 0 || ncol(meta.data) == 0)){
    meta.data <- cbind(orig.meta.data,meta.data)
  }else{
    meta.data <- orig.meta.data
  }
  new("diffMIN_object", rawdata = rawdata, dat = dat,
      net.DM = net.DM,net.EM = net.EM, reduction = reduction,
      meta.data = meta.data, IN = IN)
}

#' Print method for diffMIN_object
#'
#' @param x Object of class diffMIN_object
#' @param ... Additional arguments (currently ignored)
#'
#' @export
print.diffMIN_object <- function(x, ...) {
  cat("diffMIN_object summary:\n")
  cat("Dimensions of raw data:", dim(x@rawdata), "\n")
  cat("Dimensions of discretized data:", dim(x@dat), "\n")
  cat("Number of samples in degree matrix:", ncol(x@net.DM), "\n")
  cat("Reduction data information:", names(x@reduction), "\n")
  cat("Metadata columns:", names(x@meta.data), "\n")
}

#' Extract method for $ operator for diffMIN_object
#'
#' @param x Object of class diffMIN_object
#' @param name Character string specifying the column name in meta.data
#'
#' @return The column of meta.data corresponding to 'name'
#' @export
"$.diffMIN_object" <- function(x, name) {
  if (name %in% colnames(x@meta.data)) {
    return(x@meta.data[[name]])
  } else {
    stop(paste("'$", name, "' not found in 'meta.data'"))
  }
}

