\name{divider}

\alias{divider}

\title{
Divide a region into zones with equal crop area.
}

\description{
Divide a region into zones with equal crop area.
}


\usage{
divider(x, n=2, start="ns")
}
 
\arguments{
  \item{x}{SpatRaster with absolute or relative area of a crop of interest}
  \item{n}{postive integer. The number of splits}
  \item{start}{charater. To indictate the initial direction of split. "ns" or "ew"}
}

\value{
SpatVector
}

