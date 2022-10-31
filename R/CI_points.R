
#' @name ci_points
#' @title Utility functions for confidence interval low, mid and high points
#' 
#' @description 
#' `midpnt()` returns the midpoint from CIs. 
#' 
#' `lopnt()` returns the low point from CIs.
#' 
#' `hipnt()` returns the high point from CIs.
#' 
#' Extracts from bracketed uncertainty ranges.
#' Expects something like M (Mlo,Mhi) or M (Mlo - Mhi).
#'
#' @param x Confidence interval string 
#' @return numeric
#' @author Pete Dodd
#' @export
#' @examples
#' midpnt("1 (0,2)")
#' hipnt("1 (0-200)")
#' lopnt("0 (-1,2)")  ##TODO: error


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
