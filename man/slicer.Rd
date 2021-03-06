% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/slicer.R
\name{slicer}
\alias{slicer}
\title{Slicer: sequential pass of desnaring}
\usage{
slicer(
  m,
  r,
  prob,
  n.sample = 20,
  desnare.reps = 2,
  sequential.only = F,
  cutoff = T,
  contagion.cells,
  hotspots = F
)
}
\arguments{
\item{m}{data.table with cells (prediction, snares, bushiness,xy coordinates ...)}

\item{r}{raster with snares (or any other raster with same resolution and extent)}

\item{prob}{probability of snare detection}

\item{n.sample}{size of slice, default = 20}

\item{desnare.reps}{number of visits to adjacent cells upon snare discovery in neigbouring snare cell}

\item{sequential.only}{whether or not you should visit adjacent cells upon finding a snare}

\item{cutoff}{whether or not to cutoff end result at cumulative visits <= nrows mx (= cells of mx)}

\item{contagion.cells}{which cells will be resnared after snares were removed from a slice; this is output from function `resnare`}

\item{hotspots}{whether or not hotspot desnaring (repeated desnaring of locations with known occurrences of snares) should be carried out}
}
\value{
df data.table with added fields snares found (sequentially and adjacent) and visits (ditto).
}
\description{
Takes cell overview m, removes cells that are neither bushy nor have predicted snares (filtered data.table mx).
Takes slice of size n.sample and desnares. If snares are found, adjacent cells are searched
Optional cutoff for total visits to be equal or smaller than the filtered data.table mx you started with
}
