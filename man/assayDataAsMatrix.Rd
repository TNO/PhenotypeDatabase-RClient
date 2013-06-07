\name{assayDataAsMatrix}
\alias{assayDataAsMatrix}
\title{assayDataAsMatrix}
\description{Convenience function to load all data and samples for given assays.}
\usage{assayDataAsMatrix(assayTokens)}
\arguments{
  \item{assayTokens}{The tokens of the assays to get the data for}
}

\value{Returns a named list with the following items $data (data as pivot table),
$raw (data in list form as returned by getSamplesForAssay and getMeasurementDataForAssay)}

\author{Thomas Kelder <thomas.kelder@tno.nl>}





