% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{get_tree_eda}
\alias{get_tree_eda}
\title{Get Canopy Height Model stats}
\usage{
get_tree_eda(
  sf,
  infile,
  outfile,
  FUN = function(x) {     x * 0.17 + 3 },
  unit = TRUE
)
}
\arguments{
\item{sf}{A sf polygon or multipolygon object.}

\item{infile}{A file path to the Canopy Height Model (CHM).}

\item{outfile}{A file path where the .tifs will go.}

\item{FUN}{A function to use as lidR::lmf() function in lidR::find_trees().}

\item{unit}{Logical if CHM units are to be converted to feet.}
}
\value{
A masked .tif per polygon within the outfile
path as well as a data.frame with zonal stats per id.
}
\description{
Get Canopy Height Model stats
}
\note{
Assumes that your infile CHM is in meters. Sometimes you will need to double quote your path if there are spaces or unique
characters that CLI needs quoted, e.g. '"C:/josh erickson/some file path/"'.
}
