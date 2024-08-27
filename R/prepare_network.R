#' Prepare Data for Computing Network
#'
#' This function prepares random background and foreground networks for a `diffMIN_object`.
#'
#' @param diffMIN_object An object of class `diffMIN_object`.
#' @param nRanNet Integer: The number of random networks to generate.
#' @param seed Integer: Random seed for reproducibility.
#'
#' @return An updated `diffMIN_object` with prepared networks.
#' @export
#'
#' @examples
#' # Example usage
#' data(exp)
#' exp <- pre_process(exp)
#' obj <- create_diffMIN_object(exp)
#' obj <- prepare_network(obj, nRanNet = 100, seed = 123)
prepare_network <- function(diffMIN_object, nRanNet = 100, seed = 123){

  # Identify background and foreground Expression Matrix
  diffMIN_object@IN$bg.all.expr <- diffMIN_object@dat[rownames(subset(diffMIN_object@meta.data,group == "bg")),]
  diffMIN_object@IN$fg.all.expr <- diffMIN_object@dat[rownames(subset(diffMIN_object@meta.data,group == "fg")),]
  bg.expr <- diffMIN_object@IN$bg.all.expr
  fg.expr <- diffMIN_object@IN$fg.all.expr

  # Setting random seed
  set.seed(seed)
  seed.matrix.bg <- matrix(data = sample(1:(sum(diffMIN_object@meta.data$group == "bg") * nRanNet),
                                         sum(diffMIN_object@meta.data$group == "bg") * nRanNet, replace = T),
                           nrow = sum(diffMIN_object@meta.data$group == "bg"),
                           ncol = nRanNet)
  set.seed(seed+1)
  seed.matrix.fg <- matrix(data = sample(1:(sum(diffMIN_object@meta.data$group == "fg") * nRanNet),
                                         sum(diffMIN_object@meta.data$group == "fg") * nRanNet, replace = T),
                           nrow = sum(diffMIN_object@meta.data$group == "fg"),
                           ncol = nRanNet)

  # Set progress bar
  cat("Prepare random background for network\n")
  pb1 <- utils::txtProgressBar(min = 0, max = sum(diffMIN_object@meta.data$group == "bg"), style = 3)
  # Prepare for background data for computing network
  bg.ran.net.up <- list()
  bg.ran.net.down <- list()
  for (i in 1:sum(diffMIN_object@meta.data$group == "bg")){
    ran.sample <- list(bg.expr)
    for (j in 1:nRanNet){
      # Set random seed
      set.seed(seed.matrix.bg[i,j])
      # Shuffling per gene
      sample.net <- apply(bg.expr,2,function(x){
        x <- sample(x,length(x))})
      sample.net <- as.data.frame(sample.net)
      rownames(sample.net) <- rownames(bg.expr)
      ran.sample <- c(ran.sample,list(sample.net))
    }
    ran.sample2 <- lapply(ran.sample,function(x){x <- x[-i,]})
    bg.ran.net.up <- c(bg.ran.net.up,list(ran.sample))
    bg.ran.net.down <- c(bg.ran.net.down,list(ran.sample2))

    # Update progress bar
    utils::setTxtProgressBar(pb1, i)
  }

  # Set progress bar
  cat("\nPrepare random foreground for network\n")
  pb2 <- txtProgressBar(min = 0, max = sum(diffMIN_object@meta.data$group == "fg"), style = 3)
  # Prepare for foreground data for computing network
  fg.ran.net.up <- list()
  fg.ran.net.down <- list()
  for (i in 1:sum(diffMIN_object@meta.data$group == "fg")){
    ran.sample <- list(rbind(fg.expr[i,],bg.expr))
    for (j in 1:nRanNet){
      # Set random seed
      set.seed(seed.matrix.fg[i,j])
      # Shuffling per gene
      sample.net <- apply(bg.expr,2,function(x){
        x <- sample(x,length(x))})
      sample.net <- as.data.frame(sample.net)
      rownames(sample.net) <- rownames(bg.expr)
      ran.sample <- c(ran.sample,list(sample.net))
    }
    ran.sample2 <- lapply(ran.sample,function(x){x <- x[-1,]})
    fg.ran.net.up <- c(fg.ran.net.up,list(ran.sample2))
    fg.ran.net.down <- c(fg.ran.net.down,list(ran.sample))

    # Update progress bar
    utils::setTxtProgressBar(pb2, i)
  }
  # Output result
  diffMIN_object@IN$bg.ran.net.down <- bg.ran.net.down
  diffMIN_object@IN$fg.ran.net.up <- fg.ran.net.up
  diffMIN_object@IN$fg.ran.net.down <- fg.ran.net.down
  return(diffMIN_object)
}
