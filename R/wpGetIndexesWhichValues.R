# Authors: Maksym Bondarenko mb4@soton.ac.uk
# Date :  March 2018
# Version 0.1
#
#' wpGetindexesWhichValues to get indexes of raster pixels
#' Script is pirilised and allow to work with big raster
#' @param x Raster* object
#' @param v value of the pixel we would like to get indexes
#' @param cores Integer. Number of corest to be used
#' @param tp Type of the data to be return. Numberic or 
#' @param minblocks Integer. Minimum number of blocks. If NULL then it will be calculated automaticly
#' @param silent If FALSE then the progress will be shown
#' @rdname wpGetindexesWhichValues
#' @return numeric
#' @export
#' @examples
#' wpGetindexesWhichValues( x=raster("E:/asm_grid_100m_ccidadminl1.tif"), v=1, cores=4)
wpGetindexesWhichValues <- function(x, v, 
                                    cores=NULL,
                                    tp='numeric',
                                    minblocks=NULL,
                                    silent=FALSE) {
  
  tStart <- Sys.time()
  
  x.table <- data.frame(CellIndex=integer(),stringsAsFactors=FALSE)
  
  # get real physical cores in a computer
  
  if (is.null(cores)) {
    max.cores <- parallel:::detectCores(logical = TRUE)
    cores <- max.cores - 1
  }  
  
  if (is.null(minblocks)) {
    minblocks <- wpGetBlocksNeed(x,cores)
  }
  
  blocks <- blockSize(x,minblocks=minblocks)
  
  if (!silent) { 
    cat(paste0('\nTotal blocks ',blocks$n))
    cat('\n')
  }    
  
  cl <- makeCluster(cores)
  registerDoSNOW(cl)
  
  clusterExport(cl, c("x", "v"), envir=environment())
  clusterExport(cl, "blocks", envir=environment())
  
  
  pb <- txtProgressBar(min = 1, 
                       max = blocks$n, 
                       style = 3, 
                       width = 80)
  
  progress <- function(n) {
    
    ch.pb <- unlist(lapply(1:cores,
                           function(i) { 
                             return(i*round(blocks$n/cores))
                           }), 
                    use.names=FALSE)
    
    if (n %in% ch.pb & !silent) { 
      setTxtProgressBar(pb, n)
    }else if(n==blocks$n & !silent){
      setTxtProgressBar(pb, n)
    }
  }
  
  opts <- list(progress = progress)
  
  oper <- foreach(i=1: blocks$n , 
                  .combine=rbind, 
                  .inorder=TRUE,  
                  .packages='raster', 
                  .multicombine=TRUE, 
                  .options.snow = opts) %dopar% {
                    
                    x_row_data <- getValues(x, row=blocks$row[i], nrows=blocks$nrows[i])
                    
                    nncol <- ncol(x)
                    
                    if (i==1){
                      start.df <- 1
                      end.df <- blocks$nrows[i]*nncol
                    }else{
                      start.df <- nncol*blocks$row[i] - nncol + 1
                      end.df <- (nncol*blocks$row[i] + blocks$nrows[i]*nncol) - nncol
                    }
                    
                    df <- data.frame(CellIndex = as.numeric(start.df:end.df) )
                    df$v <- as.numeric(x_row_data)
                    
                    df2 <- df[!is.na(df$v),]
                    
                    x.table <- df2[df2$v == v, ]
                    
                    return(x.table)
                    
                  }
  stopCluster(cl)
  
  close(pb)
  
  names(oper) <- c("CellIndex", "v")

  tEnd <-  Sys.time()
  
  if (!silent) print(paste("Elapsed Processing Time:", wpTimeDiff(tStart,tEnd)))
  
  if (tp =='numeric'){
    return(oper$CellIndex)
  }else{
    return(oper)
  }
  
}