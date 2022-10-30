
#' @name ci_points
#' @title Confidence interval low, mid and high points
#' 
#' @description 
#' `midpnt()` returns the midpoint from CIs. Utility function
#' for extracting midpoint from bracketed uncertainty ranges.
#' 
#' `lopnt()` returns the low point from CIs. Utility function
#' for extracting low point from bracketed uncertainty ranges.
#' 
#' `hipnt()` returns the high point from CIs. Utility function
#' for extracting high point from bracketed uncertainty ranges.
#' 
#' Expects something like M (Mlo,Mhi) or M (Mlo - Mhi).
#' 
#' 
#' 
#' @param x Confidence interval string 
#' @return numeric
#' @author Pete Dodd
#' @export


#' @rdname ci_points
midpnt <- function(x) {
  x <- gsub("(.?)\\(.*", "\\1", x, perl = TRUE)
  as.numeric(trm(x))
}

#' @rdname ci_points
lopnt <- function(x) {
  x <- gsub(".*\\((.*?)[,|-].*", "\\1", x, perl = TRUE)
  as.numeric(trm(x))
}

#' @rdname ci_points
hipnt <- function(x){
  x <- gsub(".*[,|-](.*?)\\).*", "\\1", x, perl = TRUE)
  as.numeric(trm(x))
}
