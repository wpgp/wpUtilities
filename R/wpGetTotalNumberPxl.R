# Authors: Maksym Bondarenko mb4@soton.ac.uk
# Date :  March 2018
# Version 0.1
#
#' wpGetTotalNumberPx get total number of not NA pixels
#' 
#' @param x Raster* object
#' @param cores Integer. Number of cores for parallel calculation
#' @param tp Type of the data to be return. Numberic or 
#' @param cores Integer. Number of cores for parallel calculation
#' @param minblocks Integer. Minimum number of blocks. If NULL then it will be calculated automaticly
#' @param nl Integer. param to controle min number of blocks during paralisation
#' @param silent If FALSE then the progress will be shown
#' @rdname wpGetTotalNumberPx
#' @return numerical
#' @export
#' @examples
#' wpGetTotalNumberPx( x=raster("E:/asm_grid_100m_ccidadminl1.tif"), cores=4)
wpGetTotalNumberPx <- function(r1, 
                               cores=NULL, 
                               minblocks=NULL, 
                               nl=1, 
                               silent=FALSE) {
  
  tStart <- Sys.time()

  # get real physical cores in a computer
  max.cores <- parallel:::detectCores(logical = TRUE)
  
  if (is.null(cores)) {
    cores <- max.cores - 1
  }
  
  if (is.null(minblocks)) {
    minblocks <- wpGetBlocksNeed(r1, cores=cores, n=nl)  
  }
  

  
  blocks <- blockSize(r1,minblocks=minblocks)
  nblocks <- blocks$n
  
  cl <- makeCluster(cores)
  registerDoSNOW(cl)
  
  clusterExport(cl, c("r1"), envir=environment())
  clusterExport(cl, "blocks", envir=environment())
  
  
  pb <- txtProgressBar(min = 1, max = blocks$n, style = 3, width = 80)
  progress <- function(n) {
    
    ch.pb <- unlist(lapply(1:cores, function(i) {return(i*round(blocks$n/cores))}), use.names=FALSE)
    if (n %in% ch.pb & !silent) { 
      setTxtProgressBar(pb, n)
    }else if(n==blocks$n & !silent){
      setTxtProgressBar(pb, n)
    }
    
  }
  
  opts <- list(progress = progress)
  
  oper <- foreach(i=1: blocks$n , .combine='+',  .packages=c('raster','dplyr'), .options.snow = opts) %dopar% {
    
    
    row_data_r1 <- getValues(r1, row=blocks$row[i], nrows=blocks$nrows[i])

    
    nncol <- ncol(r1)
    
    if (i==1){
      start.df <- 1
      end.df <- blocks$nrows[i]*nncol
    }else{
      start.df <- nncol*blocks$row[i] - nncol + 1
      end.df <- (nncol*blocks$row[i] + blocks$nrows[i]*nncol) - nncol
    }
    
    df <- data.frame(CellIndex = as.numeric(start.df:end.df) )
    df$r1 <- as.numeric(row_data_r1)

    
    nrow.df <- as.numeric(nrow(df[!is.na(df$r1),]))
    
    
    return(nrow.df)
    
  }
  
  stopCluster(cl)
  
  close(pb)
  
  tEnd <-  Sys.time()
  
  if (!silent) print(paste("Elapsed Processing Time. Calc total number pixel not NA::", wpTimeDiff(tStart,tEnd)))
  
  return(oper)
}
