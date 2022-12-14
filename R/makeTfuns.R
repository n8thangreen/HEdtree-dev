##' Prepares functions that evaluate the strings generated by \code{getAQ} to evaluate tree
##'
##' The default quantities for mean calculation over tree are `cost` and `qol`,
##' but others can be specified and will work as long as these variables are defined
##' at all nodes of the tree.
##'
##' @param node Node
##' @param qnt Quantity
##' @return A \code{list} of functions
##' @author Pete Dodd
##' @seealso \link{\code{getAQ}}
##' @export
##' @examples
#' tree_fns <- makeTfuns(Dx, qnt=qnts)
#' 
#' dat <-
#'   data.frame(
#'     p.screen = 1,
#'     p.test = 1,
#'     cfr.tx = 1,
#'     cfr.notx = 1))
#'     
#' tree_fns[[1]](dat)
#' 
makeTfuns <- function(node, qnt=c('cost','qol')) {
  ans <- list()
  
  for (i in seq_along(qnt)) {
    ss <- parse(text = getAQ(node, qnt[i]))
    ans[[i]] <- function(dat) eval(ss, envir = dat)
  }
  
  setNames(ans, paste0(qnt, 'fun'))
}

