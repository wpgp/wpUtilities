# Authors: Maksym Bondarenko mb4@soton.ac.uk
# Date :  March 2018
# Version 0.1
#
#' wpSetValueWhichindexes set pixels with value based on indexes
#' 
#' @param x Raster* object
#' @param y indexes of the pixels to be replace with value v
#' @param v value of the pixel we would like to get indexes
#' @param filename File of a new raster file.
#' @param NAflag NO data value will be used for a new raster
#' @param datatype Type of raster. Avalible are INT1S/INT2S/INT4S/FLT4S/LOG1S/INT1U/INT2U/INT4U/FLT8S
#' @param overwrite Overwrite existing file
#' @param cores Integer. Number of cores for parallel calculation
#' @param minblk Integer. Minimum number of blocks. If NULL then it will be calculated automaticly
#' @param cblk Integer. param to controle min number of blocks during paralisation
#' @param silent If FALSE then the progress will be shown
#' @rdname wpSetValueWhichindexes
#' @return raster
#' @export
#' @examples
#' wpSetValueWhichindexes( x=raster("E:/asm_grid_100m_ccidadminl1.tif"), y=1,v=1, cores=4)
wpSetValueWhichindexes <- function(x, 
                                   y,
                                   v, 
                                   filename=rasterTmpFile(), 
                                   NAflag=NULL, 
                                   datatype=NULL, 
                                   overwrite=TRUE, 
                                   cores=NULL, 
                                   minblk=NULL, 
                                   cblk=NULL, 
                                   silent=TRUE) {
  
  if (!file.exists(dirname(filename))){
    stop(paste0("Directory  ",dirname(filename)," for file ", basename(filename) ," does not exist"))
  }
  
  if (is.null(NAflag)) NAflag=255
  if (is.null(datatype)) datatype='INT1U'
  if (is.null(cblk)) cblk=1
  
  if (!is(NAflag, "numeric")) stop(paste0("NAflag should be  numeric"))
  if (!is(overwrite, "logical")) stop(paste0("overwrite should be  logical (e.g., TRUE, FALSE)"))
  if (!is(silent, "logical")) stop(paste0("silent should be logical (e.g., TRUE, FALSE)"))
  #if (!is(y, "integer")) stop(paste0("y should be integer"))
  if (!is(v, "numeric")) stop(paste0("v should be numeric"))
  
  datatype <- toupper(datatype)
  
  if (!(datatype %in% c('INT1S', 'INT2S', 'INT4S', 'FLT4S', 'LOG1S', 'INT1U', 'INT2U', 'INT4U', 'FLT8S'))) {
    stop('not a valid data type. Avalible are INT1S/INT2S/INT4S/FLT4S/LOG1S/INT1U/INT2U/INT4U/FLT8S')
  }
  
  if (!cblk%%1==0) stop(paste0("cblk should be integer"))
  
  
  if ( file.exists(filename) & overwrite==FALSE) {
    stop(paste0("File ",filename," exist. Use option overwrite=TRUE"))
  } else{
    if ( file.exists(filename) ) file.remove(filename)
  }
  
  stopifnot(hasValues(x))
  
  # get real physical cores in a computer
  max.cores <- parallel:::detectCores(logical = TRUE)
  
  if (is.null(cores)) {
    cores <- max.cores - 1
  }
  
  if (cores > max.cores) {
    stop(paste0("Number of cores ",cores," more then real physical cores in PC ",max.cores ))
  }
  
  
  if (is.null(minblk)) {
    
    minblk <- wpGetBlocksNeed(x,cores,n=cblk)
    
  }
  
  beginCluster(n=cores)
  
  tStart <- Sys.time()
  
  blocks <- raster:::blockSize(x,minblocks=minblk)
  
  if (!silent) { 
    cat(paste0('\nTotal blocks ', blocks$n))
    cat('\n')
  }        
  
  cl <- getCluster()
  
  nodes <- length(cl)
  
  if (is.null(minblk)) {
    minblk <- nodes
  }   
  
  clusterExport(cl, c("blocks", "x","y","v"), envir=environment())
  
  
  wpSetValue <- function(i) {
    
    tryCatch({
      
      r.val <- raster:::getValues(x, row=blocks$row[i], nrows=blocks$nrows[i])
      
      nncol <- ncol(x)
      
      if (i==1){
        start.df <- 1
        end.df <- blocks$nrows[i]*nncol
      }else{
        start.df <- nncol*blocks$row[i] - nncol + 1
        end.df <- (nncol*blocks$row[i] + blocks$nrows[i]*nncol) - nncol
      }
      
      df <- data.frame(CellIndex = as.numeric(start.df:end.df) )
      df$v <- as.numeric(r.val)  
      
      
      #df[!is.na(df$v),"v"] <- 0
      
      df[df$CellIndex %in% y,"v"] <- v
      
      
    }, error = function(e) stop(paste0("The block '", blocks$row[i], "'",
                                       " caused the error: '", e, "'")))
    
    return(df$v)
  }     
  
  for (i in 1:nodes) {
    parallel:::sendCall(cl[[i]], wpSetValue, i, tag=i)
  }      
  
  out <- x
  
  out <- raster:::writeStart(out, 
                             filename=filename, 
                             format="GTiff", 
                             datatype=datatype, 
                             overwrite=overwrite, 
                             options=c("COMPRESS=LZW"),
                             NAflag=NAflag)     
  
  for (i in 1:blocks$n) {
    
    d <- parallel:::recvOneData(cl)
    
    if (! d$value$success ) {
      stop('cluster error')
    }
    
    tEnd <-  Sys.time()
    
    b <- d$value$tag
    
    if (!silent) { 
      wpProgressMessage(i, max=blocks$n, label= paste0("received block ",b, " Processing Time: ", wpTimeDiff(tStart,tEnd)))
    }
    out <- raster:::writeValues(out, d$value$value, blocks$row[b])
    
    # need to send more data
    #
    ni <- nodes + i
    if (ni <= blocks$n) {
      parallel:::sendCall(cl[[d$node]], wpSetValue, ni, tag=ni)
    }
  }
  
  out <- raster:::writeStop(out)      
  
  endCluster()
  
  return(out)
}