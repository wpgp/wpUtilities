# Authors: Maksym Bondarenko mb4@soton.ac.uk
# Date :  March 2018
# Version 0.1
#
#' wpGetValuesbyInds get values of pixels by indexes
#' 
#' @param x Raster* object
#' @param v indexes of the pixels to be replace with value v
#' @param tp Type of the data to be return. Numberic or 
#' @param cores Integer. Number of cores for parallel calculation
#' @param minblocks Integer. Minimum number of blocks. If NULL then it will be calculated automaticly
#' @param nl Integer. param to controle min number of blocks during paralisation
#' @param silent If FALSE then the progress will be shown
#' @rdname wpGetValuesbyInds
#' @return raster
#' @export
#' @examples
#' wpGetValuesbyInds( x=raster("E:/asm_grid_100m_ccidadminl1.tif"),v=1, cores=4)
wpGetValuesbyInds <- function(x, 
                              v, 
                              tp='numeric',
                              cores=NULL, 
                              minblocks=NULL, 
                              nl=1, 
                              silent=TRUE) {
  
  tStart <- Sys.time()
  
  # get real physical cores in a computer
  
  if (is.null(cores)) {
    max.cores <- parallel:::detectCores(logical = TRUE)
    cores <- max.cores - 1
  }
  
  x.table <- data.table(CellIndex=integer(), value=numeric(), stringsAsFactors=FALSE)
  
  if (is.null(minblocks)) {
    minblocks <- wpGetBlocksNeed(x,cores,n=nl)
  }
  
  blocks <- blockSize(x,minblocks=minblocks)
  
  if (!silent) { 
    cat(paste0('\nTotal blocks ',blocks$n))
    cat('\n')
  }    
  
  cl <- makeCluster(cores)
  registerDoSNOW(cl)
  
  clusterExport(cl, c("x", "v","x.table"), envir=environment())
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
  
  
  oper <- foreach(i=1: blocks$n , 
                  .combine=rbind, 
                  .inorder=TRUE,  
                  .packages=c('raster','data.table'), 
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
                    
                    df <- data.table(CellIndex = as.numeric(start.df:end.df) )
                    df$value <- as.numeric(x_row_data)
                    

                    x.table <- df[df$CellIndex %in% v,]
                    
                    return(x.table)
                    
                  }
  stopCluster(cl)
  
  
  close(pb)
  
  tEnd <-  Sys.time()
  
  if (!silent) print(paste("Elapsed Processing Time:", wpTimeDiff(tStart,tEnd)))
  
  setnames(oper, c("CellIndex", "value"))
  
  if (tp =='numeric'){
    return(oper$value)
  }else{
    return(oper)
  }  
  
}
