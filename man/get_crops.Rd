\name{get_crops}

\alias{get_crops}

\title{
Get crops data
}

\description{
Get crops data
}


\usage{
get_crops(crops, border, path=".", africa=TRUE, var="area")
}
 
\arguments{
  \item{crops}{character}
  \item{border}{SpatVector of polygons}
  \item{path}{character}
  \item{africa}{logical. Is this area in Africa?}
  \item{var}{character. One of "area" (ha), "density" (\% of grid cell) or "proportion" (of the area covered by \code{border})}
}

\value{
SpatRaster
}

