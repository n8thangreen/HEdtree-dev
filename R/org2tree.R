##' Utility function for \code{org2LoL}
##' 
##' Does the work underlying \code{org2LoL} and \code{org2tree}.
##' The function recursively evaluates to parse Emacs org-mode files. 
##'
##' @param inp input
##' @param lvl level
##' @param LVLS levels
##' @return A \code{list} of \code{list}s
##' @author Pete Dodd
##' @export
orgAns0 <- function(inp, lvl, LVLS) {
  tpz <- which(LVLS==lvl)
  ans <- list()
  if (length(tpz)>0) {
    for (i in seq_along(tpz)) {
      st <- tpz[i]
      if (i < length(tpz))
        nd <- tpz[i+1]
      else nd <- length(inp)
      ans[[inp[st]]] <- orgAns0(inp[st:nd],
                                lvl + 1,
                                LVLS[st:nd])
    }
  }
  ans
}


##' Emacs org-mode file to list-of-lists
##' 
##' This function reads and Emacs org-mode file and generates a list-of-lists.
##' This is used primarily as an intermediate: \code{org2tree} returns a
##' \code{data.tree} directly.
##'
##' @param filename string
##' @return A \code{list} of \code{list}s
##' @author Pete Dodd
##' @importFrom withr local_connection
##' @export
org2LoL <- function(filename) {
  ## get text
  con <- file(filename)
  txt <- readLines(con)
  close(con)
  ##TODO: replace and test
  # readLines(local_connection(file(filename, "r")))
  
  ## restrict to org bullets
  bulz <- grepl("^\\*+\\s", txt)
  txt <- txt[bulz]
  lvls <- as.numeric(regexpr("\\s", txt)) - 1 # get level of bullet
  txt <- gsub("^\\*+\\s", "", txt)        # strip stars
  txt <- gsub("^\\s+|\\s+$", "", txt)     # strip leading/trailing space
  
  orgAns0(txt, 1, lvls)
}


##' Emacs org-mode file to \code{data.tree} tree
##' 
##' This function reads and Emacs org-mode file and generates
##' a \code{data.tree} tree object. This enables rapid specification
##' of and experimentation with tree logics. Different levels of the
##' hierarchy as specified by the number of asterisks the line begins
##' with (a space must follow the asterisks). Emacs provides folding
##' and other tools for rapidly editing org-mode files.
##'
##' @param filename string
##' @return A \code{data.tree} tree object
##' @author Pete Dodd
##' @importFrom data.tree as.Node
##' @export
org2tree <- function(filename) {
  as.Node(org2LoL(fn = filename))
}

##' Text file to \code{data.tree} tree
##' 
##' This function reads an MS word organisation chart (saved as plain text)
##' file and generates a \code{data.tree} tree object.
##' This enables rapid specification of and experimentation with tree logics.
##' Different levels of the hierarchy are specified in the text file by the
##' number of tabs the line begins with.
##'
##' @param filename string
##' @return A \code{data.tree} tree object
##' @author Pete Dodd
##' @importFrom stringr str_replace
##' @importFrom data.tree as.Node
##' @export
MSorg2tree <- function(filename) {
  txt <- readLines(filename)
  tmp <- gsub("\\t","\\*", txt)
  tmp <- paste0("*", tmp)
  txt <- stringr::str_replace(tmp, "^(\\*+)", "\\1 ")
  ## see above
  bulz <- grepl("^\\*+\\s", txt)
  txt <- txt[bulz]
  lvls <- as.numeric(regexpr("\\s", txt)) - 1 # get level of bullet
  txt <- gsub("^\\*+\\s","", txt)        # strip stars
  txt <- gsub("^\\s+|\\s+$", "", txt)    # strip leading/trailing space
  tmp <- orgAns0(txt, 1, lvls)
  data.tree::as.Node(tmp)
}

