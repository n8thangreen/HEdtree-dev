##' Parameters for a lognormal distribution
##' 
##' mu & sdlog from median/mean and variance.
##'
##' @param mid the midpoint of the distribution (median/mean depending on \code{med})
##' @param var the variance of the distribution
##' @param med logical to assume \code{mid} is median (default) or otherwise mean.
##' @return \code{list} mu, sig being mu and sdlog for lognormal distribution
##' @author Pete Dodd
##' @export
getLNparms <- function(mid, var, med = TRUE) {
  if (med) {
    mu <- log(mid)                      #mid as median
    x <- (1 + sqrt(1 + 4*var/mid^2))/2
    return(list(mu = mu,
                sig = sqrt(log(x))))
  } else {
    s2 <- log(1 + var/mid^2)
    mu <- log(mid) - s2/2
    return(list(mu = mu,
                sig = sqrt(s2)))
  }
}

##' Parameters for a beta distribution
##'
##' @param E Mean
##' @param V Variance 
##' @return \code{list} containing a & b parameter values
##' @author Pete Dodd
##' @export
getAB <- function(E,V) {
  ## V <- (U-L)/4 #variance for uppers and lower
  sz <- E*(1 - E)/V - 1
  a <- sz*E
  b <- sz*(1 - E)
  list(a = a, b = b)
}

##' Utility function to trim white space
##'
##' @param x string
##' @return string 
##' @author Pete Dodd
##' @export
trm <- function(x) gsub(" ", "", x)


##' Logit function
##'
##' @param x Probability
##' @return Number on real line
##' @author Pete Dodd
##' @export
logit <- function(x) log(x) - log(1 - x)

##' Inverse logit function
##'
##' @param x Real number
##' @return Probability
##' @author Pete Dodd
##' @export
ilogit <- function(x) exp(x)/(1 + exp(x))



# tree manipulation utility functions -------------------------------------


##' Merge a tree onto another by node name
##'
##' @param rootnode Root node
##' @param nodetoadd Node to add
##' @param nodename Node name
##' @param usecase Match case in \code{nodename}? (must be \code{TRUE} currently)
##' @param leavesonly Merge only onto leaves? Logical
##' @return NULL
##' @author Pete Dodd
##' @export
##' 
MergeByName <- function(rootnode,
                        nodetoadd,
                        nodename,
                        usecase = TRUE,
                        leavesonly = FALSE) {
  if (!leavesonly) {
    rootnode$Do(function(node)
      for (K in nodetoadd$children) node$AddChildNode(Clone(K)),
      filterFun = function(x) (x$name == nodename))
  } else {
    rootnode$Do(function(node)
      for (K in nodetoadd$children) node$AddChildNode(Clone(K)),
      filterFun = function(x) (x$name == nodename) && x$isLeaf)
  }
  ## by side-effect
}

##' Drop the top of tree on reading
##'
##' @param x data.tree object
##' @return data.tree object 
##' @author Pete Dodd
##' @export
top <- function(x) x$children[[1]]

##' Ditch the top of tree on reading
##'
##' @param tree tree to plot
##' @param filename string
##' @return NULL
##' @author Pete Dodd
##' @importFrom data.tree ToDiagrammeRGraph
##' @importFrom DiagrammeR export_graph
##' @export
savetreeplot <- function(tree, filename)
  DiagrammeR::export_graph(
    data.tree::ToDiagrammeRGraph(tree), file_name = filename)


##' Make a tree from a tsv
##' 
##' Simpler text file to tree
##' 
##' @param filename relative to 'here'
##' @return A tree
##' @author Pete Dodd
##' @importFrom here here
##' @seealso \code{MSorg2tree}
##' @export
txt2tree <- function(filename) top(MSorg2tree(here::here(filename)))

##' Write a CSV tree with labels
##'
##' @param TREE the tree
##' @param filename file to write to
##' @param ... Additional arguments passed to \code{ToDataFrameTree}. Label names to include
##' @author Pete Dodd
##' @export
tree2file <- function(TREE, filename,...){
  tmp <- data.tree::ToDataFrameTree(TREE,...)
  tmp <- data.table::as.data.table(tmp)
  data.table::fwrite(tmp, file = filename)
}


##' Append a number of results to data
##'
##' @param dat PSA data
##' @param funs a list of functions
##' @param nmz an optional vector specifying a subset of the functions in \code{funs} to run
##' @param verbose print what's happening (default=\code{TRUE})
##' @author Pete Dodd
##' @export
##' 
appendResults <- function(dat, funs, nmz = NULL, verbose = TRUE){
  ## if not using nmz to specify
  if (is.null(nmz)) {
    nmz <- names(funs)
    nmz <- gsub('fun$','', names(funs))
    nmz <- nmz[nmz != 'p'] #remove p if there
  }
  
  for (nm in nmz) {
    if (verbose) cat('Calculating answers for: ',nm,'\n')
    
    fnm <- paste0(nm, 'fun')
    dat[[nm]] <- funs[[fnm]](dat)
  }
  if (verbose) cat('Done!\n')
  
  dat
}

