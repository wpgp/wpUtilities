% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wpSetAllValuesTo.R
\name{wpSetAllValuesTo}
\alias{wpSetAllValuesTo}
\title{wpSetAllValuesTo set all pixels with value}
\usage{
wpSetAllValuesTo(x, v, filename = rasterTmpFile(), NAflag = NULL,
  datatype = NULL, overwrite = TRUE, cores = NULL, minblk = NULL,
  cblk = NULL, silent = TRUE)
}
\arguments{
\item{x}{Raster* object}

\item{v}{indexes of the pixels to be replace with value v}

\item{filename}{File of a new raster file.}

\item{NAflag}{NO data value will be used for a new raster}

\item{datatype}{Type of raster. Avalible are INT1S/INT2S/INT4S/FLT4S/LOG1S/INT1U/INT2U/INT4U/FLT8S}

\item{overwrite}{Overwrite existing file}

\item{cores}{Integer. Number of cores for parallel calculation}

\item{minblk}{Integer. Minimum number of blocks. If NULL then it will be calculated automaticly}

\item{cblk}{Integer. param to controle min number of blocks during paralisation}

\item{silent}{If FALSE then the progress will be shown}
}
\value{
raster
}
\description{
wpSetAllValuesTo set all pixels with value
}
\examples{
wpSetAllValuesTo( x=raster("E:/asm_grid_100m_ccidadminl1.tif"),v=1, cores=4)
}
