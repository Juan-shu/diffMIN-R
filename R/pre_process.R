#' Pre-process Expression Matrix
#'
#' This function pre-processes expression data by removing low expression cells (or samples),
#'    removing low expression genes, and selecting highly variable genes. It also provides a warning
#'    if the number of cells (or samples) exceeds 1000, recommending a reduction for better performance.
#'
#' @param exp A data.frame or matrix. The expression data where rows are genes and columns are samples.
#' @param minExpr_sample A numeric vector of length 2. The first value is the minimum expression level required per sample,
#'                       and the second value is the minimum number of genes that must exceed this expression level for a sample
#'                       to be retained. Default is c(0.1, 5000).
#' @param minExpr_gene A numeric vector of length 2. The first value is the minimum expression level required per gene,
#'                     and the second value is the minimum number of samples that must exceed this expression level for a gene
#'                     to be retained. Default is c(0, 20).
#' @param var.genes Integer. The number of highly variable genes to select. Default is 400.
#'
#' @return A data frame with pre-processed expression data.
#' @export
#'
#' @examples
#' data(exp)
#' dim(exp)
#' result <- pre_process(exp)
#' dim(result)
pre_process <- function(exp,minExpr_sample = c(0.1,5000),
                        minExpr_gene = c(0,20),
                        var.genes = 400){
  # remove low expression cells (or samples)
  exp <- exp[,colSums(exp > minExpr_sample[1]) > minExpr_sample[2]]

  # remove low expression genes
  exp <- exp[rowSums(exp > minExpr_gene[1]) > minExpr_gene[2],]

  # select high varible genes
  var.gene <- base::apply(exp,1,stats::var)
  select.gene <- names(sort(var.gene,decreasing = T))
  exp <- exp[select.gene[1:var.genes],]

  exp <- as.data.frame(exp)
  if(dim(exp)[2] > 1000){
    message("Warning : too many cells (or samples), we recommand reduce to less than 1000 cells (or samples) by random!")
  }
  return(exp)
}
