#' Compute Background or Foreground NMI Network
#'
#' This function computes the background or foreground network using Normalized Mutual Information (NMI) on data stored in a `diffMIN` object. The function supports parallel processing and batch computations.
#'
#' @param diffMIN_obj An object of class `diffMIN` containing the necessary data for network computation.
#' @param type A character string indicating the type of network to compute. Use `"bg"` for background network or `"fg"` for foreground network.
#' @param cores The number of CPU cores to use for parallel processing. Use `"Max"` to utilize all available cores.
#' @param batch_size An integer specifying the number of samples processed per batch.
#' @param p.value.cutoff A numeric threshold for significant edges. Edges with p-values below this cutoff are considered significant.
#' @param permutation.test.times The number of permutations to perform for the permutation test.
#' @param file.dir A character string specifying the directory to save the RData file.
#' @param filename A character string specifying the name of the RData file.
#' @param whether_to_save A logical value indicating whether to save the `diffMIN_obj` to file.
#' @param use_adj.p Whether use BH method to adjust p value.
#'
#' @return An updated `diffMIN` object with computed networks added to the `net.EM` slot.
#' @export
#'
#' @examples
#' \dontrun{
#' library(diffMIN)
#' data(exp)
#' obj <- create_diffMIN_object(rawdata = exp)
#' obj <- compute_NMI_network(
#'   diffMIN_obj = obj,
#'   type = "bg",
#'   cores = "Max",
#'   batch_size = 30,
#'   p.value.cutoff = 0.05,
#'   permutation.test.times = 1000,
#'   file.dir = "./",
#'   filename = "diffMIN_obj",
#'   whether_to_save = TRUE,
#'   use_adj.p = FALSE
#' )
#' }
compute_NMI_network <- function(diffMIN_obj = obj, type = NULL,cores = "Max",
                                batch_size = 30,
                                p.value.cutoff = 0.05,
                                permutation.test.times = 1000,
                                file.dir = "./",
                                filename = "diffMIN_obj",
                                whether_to_save = TRUE,
                                use_adj.p = FALSE){
  # Setting cores
  if(cores == "Max"){
    cores <- detectCores()
  }
  cl <- parallel::makeCluster(cores)

  if(type == "bg"){
    # Check prepare
    if(length(diffMIN_obj@IN$bg.ran.net.down) > 0){
      nRanNet <- length(diffMIN_obj@IN$bg.ran.net.down[[1]]) - 1
    }else{
      cat("\n The function prepare_network() is needed first!!! \n")
    }

    # Build data flow
    all_size <- length(diffMIN_obj@IN$bg.ran.net.down)
    overall_times <- ceiling(all_size/batch_size)

    add_index <- 0
    if (all_size %% batch_size == 1 && overall_times > 1) {
      overall_times <- overall_times - 1
      add_index <- 1
    }

    cat(paste0("\n We need to perform ",overall_times," sequential data stream computations.\n"))

    # Compute net
    ### Compute net.up
    diffMIN_obj@IN$bg.ran.net.up <- NMI(diffMIN_obj@IN$bg.all.expr)
    diffMIN_obj@IN$bg.ran.net.up <- as.data.frame(diffMIN_obj@IN$bg.ran.net.up)
    colnames(diffMIN_obj@IN$bg.ran.net.up) <- "all.bg.net"
    result1 <- diffMIN_obj@IN$bg.ran.net.up

    ### Compute net.down
    for(i in 1:overall_times){
      cat(paste0("Batch ",i,"...\n"))
      # Setting index
      start_index <- (i - 1) * batch_size + 1
      end_index <- min(i * batch_size, length(diffMIN_obj@IN$bg.ran.net.down))

      if(i == overall_times){
        end_index <- end_index + add_index
      }

      result2 <- pbapply::pbsapply(X = diffMIN_obj@IN$bg.ran.net.down[start_index:end_index],function(x){
        all.net <- list()
        result.bg <- lapply(x,function(x){
          x <- diffMIN::NMI(x)
        })
        result.bg <- do.call(cbind,result.bg)
        colnames(result.bg) <- c("Cell",paste0("Cell_random",1:nRanNet))

        all.net <- c(all.net,list(result.bg))
        return(all.net)
      },cl = cl)

      ### Compute diff net
      net.list <- lapply(result2,function(x){
        x <- as.data.frame(x)
        net <- apply(x,2,FUN = function(y){
          edge.list <- rownames(result1)
          y <- result1/y
          rownames(y) <- edge.list
          return(y)
        })
        net <- as.data.frame(net)
        colnames(net) <- colnames(x)
        net <- na.omit(net)
        return(net)
      })

      # Pre-process
      cat("\nPre-process before permutation-test... \n")
      processed.ran.net <- pbapply::pbsapply(X = net.list, function(x){
        x <- as.data.frame(x)
        edge <- rownames(x)
        select.edge <- apply(x,1,function(y){
          y <- as.numeric(y)
          value <- c(y[1] > quantile(y[-1],0.95)) + c(y[1] < quantile(y[-1],0.05))
          value <- as.logical(value)
          return(value)
        })
        x <- x[select.edge,]
        x <- do.call(cbind,x)
        rownames(x) <- edge[select.edge]
        return(x)
      }, cl = cl)

      # Permutation test
      cat("\nPermutation-test: \n")
      result_net <- pbapply::pbsapply(X = processed.ran.net,function(x){
        result <- apply(x,1,function(y){
          y[is.infinite(y)] <- max(y[!is.infinite(y) & !is.na(y)],
                                   na.rm = TRUE)
          a <- y[1]
          b <- y[-1]
          result <- diffMIN::permutation_test(a,b,B = permutation.test.times)
          return(result)
        })
        if(use_adj.p){
          result <- stats::p.adjust(result, method = "BH")
        }
        x <- x[result < p.value.cutoff,]
        return(x)
      },cl = cl)

      # Removing random edge
      for(i in 1:length(result_net)){
        result_net[[i]] <- result_net[[i]][,1]
        cat("Network bg",i,"has been finishied \n")
      }

      # Save result
      diffMIN_obj@net.EM$bg <- c(diffMIN_obj@net.EM$bg,result_net)
      # Freeing memory
      rm(result2, processed.ran.net, result_net)
    }
  }else{
    if(type == "fg"){
      # Check prepare
      if(length(diffMIN_obj@IN$fg.ran.net.up) > 0){
        nRanNet <- length(diffMIN_obj@IN$fg.ran.net.up[[1]]) - 1
      }else{
        cat("\n The function prepare_network() is needed first!!! \n")
      }

      # build data flow
      all_size <- length(diffMIN_obj@IN$fg.ran.net.up)
      overall_times <- ceiling(all_size/batch_size)

      add_index <- 0
      if (all_size %% batch_size == 1 && overall_times > 1) {
        overall_times <- overall_times - 1
        add_index <- 1
      }

      cat(paste0("We need to perform ",overall_times," sequential data stream computations.\n"))
      ### Compute net.up
      for(i in 1:overall_times){
        cat(paste0("Batch ",i,"...\n"))
        # Setting index
        start_index <- (i - 1) * batch_size + 1
        end_index <- min(i * batch_size, length(diffMIN_obj@IN$fg.ran.net.up))

        if(i == overall_times){
          end_index <- end_index + add_index
        }

        result1 <- pbapply::pbsapply(X = diffMIN_obj@IN$fg.ran.net.up[start_index:end_index],function(x){
          all.net <- list()
          result.bg <- lapply(x,function(x){
            x <- diffMIN::NMI(x)
          })
          result.bg <- do.call(cbind,result.bg)
          colnames(result.bg) <- c("Cell",paste0("Cell_random",1:nRanNet))

          all.net <- c(all.net,list(result.bg))
          return(all.net)
        },cl = cl)

        ### Compute net.down
        result2 <- pbapply::pbsapply(X = diffMIN_obj@IN$fg.ran.net.down[start_index:end_index],function(x){
          all.net <- list()
          result.bg <- lapply(x,function(x){
            x <- diffMIN::NMI(x)
          })
          result.bg <- do.call(cbind,result.bg)
          colnames(result.bg) <- c("Cell",paste0("Cell_random",1:nRanNet))

          all.net <- c(all.net,list(result.bg))
          return(all.net)
        },cl = cl)

        ### Compute diff net
        net.list <- list()
        for(i in 1:length(result1)){
          up <- result1[[i]]
          up <- as.data.frame(up)
          down <- result2[[i]]
          down <- as.data.frame(down)
          net <- up/down
          net <- as.data.frame(net)
          net <- na.omit(net)
          net.list <- c(net.list,list(net))
        }

        # Pre-process
        cat("\n  Pre-process before permutation-test... \n")
        processed.ran.net <- pbapply::pbsapply(X = net.list, function(x){
          x <- as.data.frame(x)
          edge <- rownames(x)
          select.edge <- apply(x,1,function(y){
            y <- as.numeric(y)
            value <- c(y[1] > quantile(y[-1],0.95)) + c(y[1] < quantile(y[-1],0.05))
            value <- as.logical(value)
            return(value)
          })
          x <- x[select.edge,]
          x <- do.call(cbind,x)
          rownames(x) <- edge[select.edge]
          return(x)
        }, cl = cl)

        # Permutation test
        cat("\n Permutation-test: \n")
        result_net <- pbapply::pbsapply(X = processed.ran.net,function(x){
          result <- apply(x,1,function(y){
            y[is.infinite(y)] <- max(y[!is.infinite(y) & !is.na(y)],
                                     na.rm = TRUE)
            a <- y[1]
            b <- y[-1]
            result <- diffMIN::permutation_test(a,b,B = permutation.test.times)
            return(result)
          })
          if(use_adj.p){
            result <- stats::p.adjust(result, method = "BH")
          }
          x <- x[result < p.value.cutoff,]
          return(x)
        },cl = cl)

        # Removing random edge
        for(i in 1:length(result_net)){
          result_net[[i]] <- result_net[[i]][,1]
          cat("Network fg",i,"has been finishied \n")
        }

        # Save result
        diffMIN_obj@net.EM$fg <- c(diffMIN_obj@net.EM$fg,result_net)
        # Freeing memory
        rm(result1, result2, processed.ran.net, result_net)
      }
    }
  }

  # Stop clusters
  stopCluster(cl = cl)
  # Saving data
  if(whether_to_save == T){
    save(diffMIN_obj,file = paste0(file.dir,filename,".RData"))
    cat("\n The task has been saved. \n")
  }else{
    cat("\n The task has been finished. \n")
  }
  return(diffMIN_obj)
}
