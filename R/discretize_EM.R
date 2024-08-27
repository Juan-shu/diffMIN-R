#' Discretize expression matrix
#'
#' @param diffMIN_object A diffMIN_object S4 Object
#'
#' @return A diffMIN_object S4 Object
#' @export
#'
#' @examples
#' library(diffMIN)
#' data(exp)
#' exp <- pre_process(exp)
#' obj <- create_diffMIN_Object(rawdata = exp)
#' obj <- discretize_EM(obj)
discretize_EM <- function(diffMIN_object, ...) {
  diffMIN_object@dat <- infotheo::discretize(t(log2(diffMIN_object@rawdata + 1)), ...)
  rownames(diffMIN_object@dat) <- colnames(diffMIN_object@rawdata)
  diffMIN_object@dat <- as.data.frame(diffMIN_object@dat)
  return(diffMIN_object)
}
