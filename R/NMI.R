#' Calculate Normalized Mutual Information (NMI)
#'
#' This function computes the normalized mutual information (NMI) between each pair of columns in a data frame or matrix.
#'
#' @param dis A data frame or matrix where each column represents a discrete variable.
#'
#' @return A named vector containing the NMI values for each pair of variables.
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage
#' dis <- data.frame(A = sample(1:3, 10, replace = TRUE), B = sample(1:3, 10, replace = TRUE))
#' NMI(dis)
#' }
NMI <- function(dis,...){

  # Compute mutual information
  res <- infotheo::mutinformation(dis, ...)

  #Compute entrophy and normalized mutual information
  H_gene <- apply(dis,2,function(x){infotheo::entropy(table(x))})
  Entrophy <- outer(H_gene,H_gene,FUN = function(x,y){(x+y)/2})
  NMI_value <- reshape2::melt(res/(Entrophy+.Machine$double.eps))[reshape2::melt(upper.tri(res/Entrophy))[,3],]
  name <- paste0(NMI_value[,1],"_",NMI_value[,2])
  NMI_value <- NMI_value[,3]

  # Eliminates floating point arithmetic errors
  NMI_value[NMI_value < 0 ] <- 0
  names(NMI_value) <- name
  return(NMI_value)
}
