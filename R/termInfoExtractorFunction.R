#' @exportPattern ^[[:alpha:]]+
#' @import utils
#' @importFrom Rcpp evalCpp sourceCpp cppFunction
#' @importFrom methods new
#' @importFrom utils download.file
#' @importFrom xptr xptr_clear
#' @useDynLib ontologyReader
#' @title Gene set examples in Entrez or Symbol ids.
#' 


#' Get list of descendants terms
#' 
#' @param obj List obj (created by reader function)
#' @param term GO id character
#' @param ordering boolean to choose if the descendants are ordered by level (from high level to low level)
#' @return A vector object including the descendants of term.
#' @examples 
#' \dontrun{
#' 
#' data(go)
#' 
#' 
#' getDescendats(go, "GO:0007005",ordering=TRUE)
#' }
getDescendants <- function(obj=NULL, term=NULL, ordering=FALSE){
  pos <- which(obj$name%in%term)
  t <- obj$termOBJ[[pos]]
  desc <- t$getDescendants()
  if(ordering){
    return(obj$name[which(obj$name%in%desc)])
  }else{
    return(desc)
  }
}

#' Get list of descendants terms but with a limited distance
#' 
#' @param obj List obj (created by reader function)
#' @param term GO id character
#' @param ordering boolean to choose if the descendants are ordered by level (from high level to low level)
#' @param limit number of maximal steps for exploring the go structure.
#' @return A vector object including the descendants of term.
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
    pos <- which(obj$name%in%term)
    t <- obj$termOBJ[[pos]]
    desc <- t$getChildren()
    step1 <- step+1
   
      list.desc <- lapply(desc, function(x){
        return(getDescendantsStep(obj,x,step1,limit))
      })
      desc <- c(desc,unlist(list.desc))
      
    }
    return(desc)
    
  }
  
  pos <- which(obj$name%in%term)
  t <- obj$termOBJ[[pos]]
  desc <- t$getChildren()
  
  list.desc <- lapply(desc, function(x){
    return(getDescendantsStep(obj,x,1,limit))
  })
  desc <- unique(c(desc,unlist(list.desc)))
  if(ordering){
    return(obj$name[which(obj$name%in%desc)])
  }else{
    return(desc)
  }
}

#' Get list of ancestor terms
#' 
#' @param obj List obj (created by reader function)
#' @param term GO id character
#' @param ordering boolean to choose if the ancestors are ordered by level (from high level to low level)
#' @return A vector object including the ancestors of term.
#' @examples 
#' \dontrun{
#' 
#' data(go)
#' 
#' 
#' getAncestors(go, "GO:0007005",ordering=TRUE)
#' }
getAncestors <- function(obj=NULL, term=NULL, ordering=FALSE){
  pos <- which(obj$name%in%term)
  t <- obj$termOBJ[[pos]]
  anc <- t$getAncestors()
  if(ordering){
    return(obj$name[rev(which(obj$name%in%anc))])
  }else{
    return(anc)
  }
}

#' Get list of parent terms
#' 
#' @param obj List obj (created by reader function)
#' @param term GO id character
#' @param ordering boolean to choose if the parents are ordered by level (from high level to low level)
#' @return A vector object including the parents of term.
#' @examples 
#' \dontrun{
#' 
#' data(go)
#' 
#' 
#' getParent(go, "GO:0007005",ordering=TRUE)
#' }
getParent <- function(obj=NULL, term=NULL){
  pos <- which(obj$name%in%term)
  t <- obj$termOBJ[[pos]]
  p <- t$getParents()
  return(p)
  
}

#' Get list of child terms
#' 
#' @param obj List obj (created by reader function)
#' @param term GO id character
#' @param ordering boolean to choose if the children are ordered by level (from high level to low level)
#' @return A vector object including the children of term.
#' @examples 
#' \dontrun{
#' 
#' data(go)
#' 
#' 
#' getChildren(go, "GO:0007005",ordering=TRUE)
#' }
getChildren <- function(obj=NULL, term=NULL){
  pos <- which(obj$name%in%term)
  t <- obj$termOBJ[[pos]]
  c <- t$getChildren()
  return(c)
  
}


#' Change the posible alternative ID of the main ID
#' 
#' @param obj List obj (created by reader function)
#' @param vector GO id vector
#' @return A vector object including the main term ID.
#' @examples 
#' \dontrun{
#' 
#' data(go)
#' 
#' 
#' changeAltTerm(go, c( "GO:0044699", "GO:0008372", "GO:0044702"))
#' }
changeAltTerm <- function(obj=NULL, vector=NULL){
  vpos <- which(vector%in%names(obj$alternativeIDs))
  altv <- vector[vpos]
  res <- obj$alternativeIDs[altv]
  vector[vpos] = unlist(res)
  return(unlist(vector))
}

#' \code{ontologyReader} object encapsulating structure of the Gene Ontology (GO) comprising a \code{list} of lists/vectors of properties of GO terms indexed by term ID
#' GO version (Downloaded 04/2020)
#' 
#' @name go 
#' @title GO index
#' @docType data
#' @format List of lists and vectors
NULL


