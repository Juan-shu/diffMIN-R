#' Transform Edge Matrix to Degree Matrix
#'
#' @param diffMIN_object An object of class diffMIN_object.
#'
#' @return An object of class diffMIN_object with updated degree matrices.
#' @export
#'
#' @examples
#' \dontrun{
#' library(diffMIN)
#' data(obj)
#' obj <- transform_EM_to_DM(obj)
#' }
transform_EM_to_DM <- function(diffMIN_object){
  # Detecting EM and show related info
  if(is.null(diffMIN_object@net.EM$bg) & is.null(diffMIN_object@net.EM$fg)){
    stop("You need to use function compute_NMI_network() to compute 'net.EM' first!!! The process has been terminated due to missing 'net.EM'.")
  }
  if(is.null(diffMIN_object@net.EM$bg)){
    cat("\033[31mWarning: Background 'net.EM' are not found!!!\033[0m\n")
    DM.bg <- NULL
    bg.em <- NULL
  }else{
    cat("Background net 'net.EM' will be computed...\n")
  }
  if(is.null(diffMIN_object@net.EM$fg)){
    cat("\033[31mWarning: Foreground 'net.EM' are not found!!!\033[0m\n")
    DM.fg <- NULL
    fg.em <- NULL
  }else{
    cat("Foreground net 'net.EM' will be computed...\n")
  }
  # Detecting background EM
  if(!(is.null(diffMIN_object@net.EM$bg))){
    bg.em <- diffMIN_object@net.EM$bg
  }

  # Detecting foreground EM
  if(!(is.null(diffMIN_object@net.EM$fg))){
    fg.em <- diffMIN_object@net.EM$fg
  }

  edge.all <- c(bg.em,fg.em)
  edge.name <- NULL
  for(i in 1:length(edge.all)){
    edge.name <- unique(c(edge.name,names(edge.all[[i]])))
  }
  EM.bg <- data.frame(edge = edge.name,row.names = edge.name)
  EM.fg <- data.frame(edge = edge.name,row.names = edge.name)
  # Begin to transform
  gene <- rownames(diffMIN_object@rawdata)
  dim1 <- dim(diffMIN_object@rawdata)[1]
  dim2.bg <- sum(diffMIN_object@meta.data$group == "bg")
  dim2.fg <- sum(diffMIN_object@meta.data$group == "fg")

  if(!(is.null(bg.em))){
    cat("\n Transform background edge matrix to degree matrix...\n")
    matrix0.bg <- matrix(0,dim1,dim1)
    colnames(matrix0.bg) <- rownames(diffMIN_object@rawdata)
    rownames(matrix0.bg) <- rownames(diffMIN_object@rawdata)
    degree.matrix <- data.frame(gene = rownames(matrix0.bg))
    pb1 <- utils::txtProgressBar(min = 0, max = sum(diffMIN_object@meta.data$group == "bg"), style = 3)
    for (i in 1:dim2.bg) {
      edge <- bg.em[[i]]
      EM.bg <- cbind(EM.bg,edge[edge.name])
      node <- t(matrix(unlist(strsplit(names(edge),"_")),nrow = 2))
      node <- data.frame(V1 = match(node[,1],rownames(matrix0.bg)),V2 = match(node[,2],rownames(matrix0.bg)))
      dir1 <- node$V1+(node$V2-1)*dim1
      dir2 <- node$V2+(node$V1-1)*dim1
      sample <- matrix0.bg
      sample[c(dir1,dir2)] <- 1
      degree.matrix <- cbind(degree.matrix,apply(sample,1,sum))

      # Update progress bar
      utils::setTxtProgressBar(pb1, i)
    }
    degree.matrix <- degree.matrix[,-1]
    DM.bg <- degree.matrix
    colnames(DM.bg) <- colnames(diffMIN_object@rawdata)[diffMIN_object@meta.data$group == "bg"]
    colnames(EM.bg) <- colnames(DM.bg)
  }

  if(!(is.null(fg.em))){
    cat("\n Transform foreground edge matrix to degree matrix...\n")
    matrix0.fg <- matrix(0,dim1,dim1)
    colnames(matrix0.fg) <- rownames(diffMIN_object@rawdata)
    rownames(matrix0.fg) <- rownames(diffMIN_object@rawdata)
    degree.matrix <- data.frame(gene = rownames(matrix0.fg))
    pb2 <- utils::txtProgressBar(min = 0, max = sum(diffMIN_object@meta.data$group == "fg"), style = 3)
    for (i in 1:dim2.fg) {
      edge <- fg.em[[i]]
      EM.fg <- cbind(EM.fg,edge[edge.name])
      node <- t(matrix(unlist(strsplit(names(edge),"_")),nrow = 2))
      node <- data.frame(V1 = match(node[,1],rownames(matrix0.fg)),V2 = match(node[,2],rownames(matrix0.fg)))
      dir1 <- node$V1+(node$V2-1)*dim1
      dir2 <- node$V2+(node$V1-1)*dim1
      sample <- matrix0.fg
      sample[c(dir1,dir2)] <- 1
      degree.matrix <- cbind(degree.matrix,apply(sample,1,sum))

      # Update progress bar
      utils::setTxtProgressBar(pb2, i)
    }
    degree.matrix <- degree.matrix[,-1]
    DM.fg <- degree.matrix
    colnames(DM.fg) <- colnames(diffMIN_object@rawdata)[diffMIN_object@meta.data$group == "fg"]
    colnames(EM.fg) <- colnames(DM.fg)
  }

  DM <-cbind(DM.bg,DM.fg)
  EM.bg <- EM.bg[,-1]
  EM.fg <- EM.fg[,-1]
  cat("\n The transforming has been finished. \n")

  diffMIN_object@net.DM <- DM
  diffMIN_object@net.EM$bg <- EM.bg
  diffMIN_object@net.EM$fg <- EM.fg
  return(diffMIN_object)
}
