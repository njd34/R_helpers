## print start of a vector or top left corner of a data-frame/matrix
pv <- preview <- function(df, rows = 10, cols = NULL) {
  if(is.null(dim(df))) {
    ## Assume 1D object
    rows <- min(rows, length(df))
    print(df[1:rows])
  } else {
    if(is.null(cols)) {
      cols <- rows
    }
    rows <- min(rows, nrow(df))
    cols <- min(cols, ncol(df))
    print(df[1:rows, 1:cols])
  }
}

## grep-like function that returns matches
grepp <- function (pattern, x, ignore.case = FALSE, perl = FALSE, value = FALSE, 
                   fixed = FALSE, useBytes = FALSE, invert = FALSE) {
  return(x[grep(pattern, x, ignore.case, perl, value, fixed, useBytes, invert)])
}

## table, with NA included by default
tblNA <- function(...) {
  table(..., useNA = "always")
}

## quick visualisation of tblNA
bar.tbl <- function(x, ..., plot.title = NULL) {
  t <- tblNA(x, ...)
  n <- length(t)
  barplot(t, col = c(rep("grey", n-1), "pink"), names.arg = c(names(t)[1:(n-1)], "NA"), main = plot.title)
}

## remove named columns from a data-frame
rm.col <- function(df, colnames, drop = FALSE) {
  return(df[, -which(names(df) %in% colnames), drop = drop])
}

## shortcut to calculate proportion of elements that are TRUE with options for NA handling (also works for 0/1 coding)
proportion.true <- function(x, na.handling = "keep") {
  if (is.logical(x) || sum(x %in% c(0, 1)) == sum(!is.na(x))) {
    if (na.handling == "keep") {
      return(mean(x))
    } else if (na.handling == "remove") {
      return(mean(x, na.rm = TRUE))
    } else if (na.handling == "as.false") {
      x[is.na(x)] <- FALSE
      return(mean(x))
    } else {
      stop("na.handling must be one of 'keep', 'remove' or 'false'")  
    }
  } else {
    stop("Require logical or 0/1 object")
  }
}

## read in data using my default settings
read.default <- function(file,
                         path = NULL,
                         header = TRUE,
                         sep = "\t",
                         quote = "",
                         na.strings = "no na strings",
                         comment.char = "",
                         stringsAsFactors = FALSE,
                         ...) {
  if(is.null(path)) {
    x <- read.table(file = file,
                    header = header,
                    sep = sep,
                    quote = quote,
                    na.strings = na.strings,
                    comment.char = comment.char,
                    stringsAsFactors = stringsAsFactors,
                    ...)
  } else {
    x <- read.table(file = paste0(path, "/", file),
                    header = header,
                    sep = sep,
                    quote = quote,
                    na.strings = na.strings,
                    comment.char = comment.char,
                    stringsAsFactors = stringsAsFactors,
                    ...)
  }
  return(x)
}

## write out data using my default settings
write.default <- function(x, file,
                          need.row.names = FALSE,
                          need.col.names = TRUE,
                          quote = FALSE,
                          sep = "\t") {
  write.table(x,
              file = file,
              quote = quote,
              sep = sep,
              row.names = need.row.names,
              col.names = need.col.names)
}

## Clear global environment
rmall <- function() {
  rm(list=ls(envir = .GlobalEnv), envir = .GlobalEnv)
}

## Reverse levels of a factor
reverse.levels <- function(x) {
  if (!is.factor(x)) {
    stop("x not a factor")
  } else {
    z <- factor(x, levels = rev(levels(x)))
    return(z)
  }
}
