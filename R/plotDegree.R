#' Plot Dot Plot for Node Degree
#'
#' @param diffMIN_object An object of class `diffMIN_object`.
#' @param feature Character string specifying the feature (degree) to plot.
#' @param reduction Character string specifying the reduction result to use. Options are "umap", "tsne", or "pca".
#'
#' @return A ggplot object showing the dotplot of node degree.
#' @export
#'
#' @examples
#' \dontrun{
#' library(diffMIN)
#' data(obj)
#' obj <- transform_EM_to_DM(obj)
#' obj <- RunReduc(diffMIN_object = obj,reduction = c("umap","tsne","pca"),seed = 1)
#' plotDegree(diffMIN_object = obj, "Nppa", reduction = "tsne")
#' }
plotDegree <- function(diffMIN_object, feature, reduction = "tsne"){
  # Prepare data
  if(reduction == "umap"){
    data <- diffMIN_object@reduction$umap
  }else{if(reduction == "tsne"){
    data <- diffMIN_object@reduction$tsne
  }else{if(reduction == "pca"){
    data <- diffMIN_object@reduction$pca
  }}}
  data <- cbind(data,t(as.data.frame(diffMIN_object@net.DM[feature,])))
  colnames(data) <- c("x","y","degree")

  # plot
  p <- ggplot2::ggplot(data = data,ggplot2::aes(x = x, y = y))+
    ggplot2::geom_point(size = 2)+
    ggplot2::geom_point(ggplot2::aes(color = degree))+
    ggplot2::ylab("Dim 2")+ggplot2::xlab("Dim 1")+ggplot2::theme_test()+
    ggplot2::ggtitle(feature)+ggplot2::labs(color = "Degree")+
    scale_color_gradient(low = "#349ed9", high = "#d94a37")
  p
  return(p)
}

