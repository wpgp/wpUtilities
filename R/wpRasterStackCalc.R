#  Authors: Maksym Bondarenko mb4@soton.ac.uk
#  Date :  Jan 2018
#  Version 0.1
#
#' 
wpRasterStackCalcDo <- function(x, fun, filename, NAflag, datatype, overwrite, cores, minblk, silent) {
  
  tStart <- Sys.time()
  
  blocks <- raster:::blockSize(x,minblocks=minblk)
  
  
  if (!silent) { 
    cat(paste0('\nTotal blocks ',blocks$n))
    cat('\n')
  }  
  
  cl <- raster::getCluster()
  
  nodes <- length(cl)
  
  if (is.null(minblk)) {
    minblk <- nodes
  }
  
  clusterExport(cl, c("blocks", "x","fun"), envir=environment())
  
  
  clwpRasterCalcFun <- function(i) {
    
    tryCatch({
      v <- raster:::getValues(x, row=blocks$row[i], nrows=blocks$nrows[i])
      
      res <- apply(v, 1, fun)
      
    }, error = function(e) stop(paste0("The block '", blocks$row[i], "'",
                                       " caused the error: '", e, "'")))
    
    return(res)
  }
  
  
  for (i in 1:nodes) {
    parallel:::sendCall(cl[[i]], clwpRasterCalcFun, i, tag=i)
  }
  
  
  out <- x[[1]]
  
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
      parallel:::sendCall(cl[[d$node]], clwpRasterCalcFun, ni, tag=ni)
    }
  }
  
  out <- raster:::writeStop(out)
  
  return(out)
}

#' wpRasterStackCalc function to calculate the values 
#' for a new Raster object from other RasterStack object 
#' using a formula. Function is paralyzed and suitable for big rasters
#'
#' @param x RasterStack object
#' @param fun is typically a function that can take a single vector as input
#' @param filename File of a new raster file.
#' @param NAflag NO data value will be used for a new raster
#' @param datatype Type of raster. Avalible are INT1S/INT2S/INT4S/FLT4S/LOG1S/INT1U/INT2U/INT4U/FLT8S
#' @param overwrite Overwrite existing file
#' @param cores Integer. Number of cores for parallel calculation
#' @param minblk Integer. Minimum number of blocks. If NULL then it will be calculated automaticly
#' @param cblk Integer. param to controle min number of blocks during paralisation
#' @param silent If FALSE then the progress will be shown
#' @param na.rm Optional
#' @return Raster* object
#' @export
#' @examples
#' wpRasterStackCalc( x, fun='sum')
#' wpRasterStackCalc( x, fun=function(x){ sum(as.numeric(x))})
#' wpRasterStackCalc( x, fun=function(x){ (x[1]-x[2])/x[3]})
#' wpRasterStackCalc( x, fun=function(x){ if (any(!is.na(x))) { return(0) } else { return(NA) }}
#' 			
wpRasterStackCalc <- function(x, 
                              fun='mean', 
                              filename=rasterTmpFile(), 
                              NAflag=NULL, 
                              datatype=NULL, 
                              overwrite=TRUE, 
                              cores=NULL,
                              minblk=NULL, 
                              cblk=NULL, 
                              silent=TRUE, 
                              na.rm) {
  
  
  if (!file.exists(dirname(filename))){
    stop(paste0("Directory  ",dirname(filename)," for file ", basename(filename) ," does not exist"))
  }
  
  if (is.null(NAflag)) NAflag=-99999
  if (is.null(datatype)) datatype='FLT4S'
  if (is.null(cblk)) cblk=1
  
  if (!is(NAflag, "numeric")) stop(paste0("NAflag should be  numeric"))
  if (!is(overwrite, "logical")) stop(paste0("overwrite should be  logical (e.g., TRUE, FALSE)"))
  if (!is(silent, "logical")) stop(paste0("silent should be logical (e.g., TRUE, FALSE)"))
  
  datatype <- toupper(datatype)
  
  if (!(datatype %in% c('INT1S', 'INT2S', 'INT4S', 'FLT4S', 'LOG1S', 'INT1U', 'INT2U', 'INT4U', 'FLT8S'))) {
    stop('not a valid data type. Avalible are INT1S/INT2S/INT4S/FLT4S/LOG1S/INT1U/INT2U/INT4U/FLT8S')
  }
  
  if (!cblk%%1==0) stop(paste0("cblk should be integer"))
  
  
  
  if (!missing(na.rm)) {
    if (!is(na.rm, "logical")) stop(paste0("na.rm should be  logical (e.g., TRUE, FALSE)"))
  }
  
  if ( file.exists(filename) & overwrite==FALSE) {
    stop(paste0("File ",filename," exist. Use option overwrite=TRUE"))
  } else{
    if ( file.exists(filename) ) file.remove(filename)
  }
  
  stopifnot(hasValues(x))
  
  if ( (class(x)[1] != 'RasterStack')) {
    stop("Argument x should be 'RasterStack'")
  }  

  nl <- nlayers(x)
  
  if ( nl < 2 ) {
    stop("'RasterStack' should have atleast two Raster layers")
  }  
  
  
  if (class(fun) == 'character') {
    
    fun <- tolower(fun)
    if (! fun %in% c('sum', 'mean', 'min', 'max')) {
      stop("fun can be 'sum', 'mean', 'min', or 'max'")
    }
    
  }else{
    
    testMatrix = matrix( seq(1, nl, by = 1), 
                         nrow=1, 
                         ncol=nl, 
                         byrow = TRUE)
    
    if (!missing(na.rm)) {
      
      test <- try( apply(testMatrix, 1, fun, na.rm=na.rm), silent=TRUE)
      
      if (class(test) == 'try-error') {
        
        test <- try(fun(testMatrix, na.rm=na.rm), silent=TRUE)
        
        if (class(test) == 'try-error') {
          stop("cannot use this function. Perhaps add '...' or 'na.rm' to the function arguments?") 
        }
      } 
    } else {
      test <- try( apply(testMatrix, 1, fun), silent=TRUE)
      if (class(test) == 'try-error') {
        
        test <- try(fun(testMatrix), silent=TRUE)
        
        if (class(test) == 'try-error') {
          stop("cannot use this function") 
        }
      }
    } 
  }
  
  
  # get real physical cores in a computer
  max.cores <- parallel:::detectCores(logical = TRUE)
  
  if (is.null(cores)) {
    cores <- max.cores - 1
  }
  
  if (cores > max.cores) {
    stop(paste0("Number of cores ",cores," more then real physical cores in PC ",max.cores ))
  }
  
  
  if (is.null(minblk)) {
    
    if (nl > 1) nln <- nl
    minblk <- wpGetBlocksNeed(x,cores,n=nln*cblk)
    
  }

  beginCluster(n=cores)
  
  out <- wpRasterStackCalcDo(x, fun, filename,NAflag,datatype, overwrite, cores, minblk, silent)
  
  endCluster()
  
  return(out)
}