\name{divider}

\alias{divider}

\title{
Divide a region into zones with equal crop area.
}

\description{
Divide a region into zones with equal crop area.
}


\usage{
divider(x, n=2, start="ns", border=NULL, rasterize=FALSE)
}
 
\arguments{
  \item{x}{SpatRaster with absolute or relative area of a crop of interest}
  \item{n}{positive integer. The number of splits}
  \item{start}{character. To indicate the initial direction of split. "ns" or "ew"}
  \item{border}{NULL or SpatVector to crop the output with}
  \item{rasterize}{character. Either "no" (return a SpatVector), "yes" (return a SpatRaster) or "both" return a list with a SpatRaster and a SpatVector}
}

\value{
SpatVector, SpatRaster, or list
}

