% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wpGetTotalNumberPxl.R
\name{wpGetTotalNumberPx}
\alias{wpGetTotalNumberPx}
\title{wpGetTotalNumberPx get total number of not NA pixels}
\usage{
wpGetTotalNumberPx(r1, cores = NULL, minblocks = NULL, nl = 1,
  silent = FALSE)
}
\arguments{
\item{cores}{Integer. Number of cores for parallel calculation}

\item{minblocks}{Integer. Minimum number of blocks. If NULL then it will be calculated automaticly}

\item{nl}{Integer. param to controle min number of blocks during paralisation}

\item{silent}{If FALSE then the progress will be shown}

\item{x}{Raster* object}

\item{tp}{Type of the data to be return. Numberic or}

\item{cores}{Integer. Number of cores for parallel calculation}
}
\value{
numerical
}
\description{
wpGetTotalNumberPx get total number of not NA pixels
}
\examples{
wpGetTotalNumberPx( x=raster("E:/asm_grid_100m_ccidadminl1.tif"), cores=4)
}
