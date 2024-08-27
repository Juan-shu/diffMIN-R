#' diffMIN_object Class Definition
#'
#' @slot rawdata The raw expression matrix.
#' @slot dat The discretized expression matrix.
#' @param net.DM Data frame: The diffMIN degree matrix.
#' @param net.EM list: The diffMIN edge matrix.
#' @slot reduction Reduction data containing UMAP, PCA, t-SNE, etc.
#' @slot meta.data Metadata associated with the data.
#' @slot IN Intermediate network used for calculating the final network
#' @name diffMIN_object
#' @aliases diffMIN_object-class
#' @rdname diffMIN_object-class
#' @exportClass diffMIN_object
setClass("diffMIN_object",
         slots = c(
           rawdata = "ANY",
           dat = "ANY",
           net.DM = "data.frame",
           net.EM = "list",
           reduction = "list",
           meta.data = "data.frame",
           IN = "list"
         ))
