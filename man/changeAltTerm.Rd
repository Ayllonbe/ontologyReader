% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ontologyReader.R
\name{changeAltTerm}
\alias{changeAltTerm}
\title{Change the posible alternative ID of the main ID}
\usage{
changeAltTerm(obj = NULL, vector = NULL)
}
\arguments{
\item{obj}{List obj (created by reader function)}

\item{vector}{GO id vector}
}
\value{
A vector object including the main term ID.
}
\description{
Change the posible alternative ID of the main ID
}
\examples{
\dontrun{

data(go)


changeAltTerm(go, c( "GO:0044699", "GO:0008372", "GO:0044702"))
}

}
