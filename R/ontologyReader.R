#' @exportPattern ^[[:alpha:]]+
#' @import Rcpp
#' @import methods
#' @useDynLib ontologyReader
#' @title Get list of descendants terms but with a limited distance
#'
#' @param obj ontology R6 class
#' @param term GO id character
#' @param ordering boolean to choose if the descendants are ordered by level (from high level to low level)
#' @param limit number of maximal steps for exploring the go structure.
#' @return A vector object including the descendants of term.
#'
#' @examples
#' \dontrun{
#'
#' data(go)
#'
#'
#' getDescendantsWithLimit(go, "GO:0007005",ordering=TRUE,limit=2)
#' }
getDescendantsWithLimit <- function(obj=NULL, term=NULL, ordering=FALSE, limit){
  getDescendantsStep <- function(obj=NULL, term=NULL, step,limit){
    if(step<limit){
      desc <- obj$getChildren(term)
      step1 <- step+1

      list.desc <- lapply(desc, function(x){
        return(getDescendantsStep(obj,x,step1,limit))
      })
      desc <- c(desc,unlist(list.desc))

    }
    return(desc)

  }

  desc <- obj$getChildren(term)

  list.desc <- lapply(desc, function(x){
    return(getDescendantsStep(obj,x,1,limit))
  })
  desc <- unique(c(desc,unlist(list.desc)))
  if(ordering){
    return(obj$ids[which(obj$ids%in%desc)])
  }else{
    return(desc)
  }
}
#' @title Change the posible alternative ID of the main ID
#'
#' @param obj List obj (created by reader function)
#' @param vector GO id vector
#' @return A vector object including the main term ID.
#'
#' @examples
#' \dontrun{
#'
#' data(go)
#'
#'
#' changeAltTerm(go, c( "GO:0044699", "GO:0008372", "GO:0044702"))
#' }
#'
changeAltTerm <- function(obj=NULL, vector=NULL){
  vpos <- which(vector%in%names(obj$alternativeIDs))
  altv <- vector[vpos]
  res <- obj$alternativeIDs[altv]
  vector[vpos] = unlist(res)
  return(unlist(vector))
}

#' @title Read ontology task (time-consuming)
#'
#' @param file obo file
#' @return A ontoClass object.
#'
#' @examples
#' \dontrun{
#'
#' onto <- ontologyReader("go.obo")
#'
#'
#' }
#'

ontologyReader <- function(file){
  onto <- ontologyReader::reader(file)
  ontoCl <- ontology$new(onto)
  return(ontoCl)
}

#' \code{ontologyReader} object encapsulating structure of the Gene Ontology (GO) comprising a \code{list} of lists/vectors of properties of GO terms indexed by term ID
#' GO version (Downloaded 04/2020)
#'
#' @name go
#' @title GO index
#' @docType data
#' @usage data(go)
#' @format List of lists and vectors
NULL

