\name{divider}

\alias{divider}

\title{
Divide a region into zones with equal crop area.
}

\description{
Divide a region into zones with equal crop area.
}


\usage{
divider(x, n=2, start="ns", border=NULL)
}
 
\arguments{
  \item{x}{SpatRaster with absolute or relative area of a crop of interest}
  \item{n}{positive integer. The number of splits}
  \item{start}{character. To indicate the initial direction of split. "ns" or "ew"}
  \item{border}{SpatVector to crop the output with}
}

\value{
SpatVector
}

