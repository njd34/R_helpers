preview <- function(df, rows=10, cols=NULL) {
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

pv <- preview

grepp <- function (pattern, x, ignore.case = FALSE, perl = FALSE, value = FALSE, 
                   fixed = FALSE, useBytes = FALSE, invert = FALSE) {
  return(x[grep(pattern, x, ignore.case, perl, value, fixed, useBytes, invert)])
}

## table, with NA included by default
tblNA <- function(...) {
  table(..., useNA = "always")
}

hist.tbl <- function(x, ..., plot.title = NULL) {
  t <- tblNA(x, ...)
  n <- length(t)
  barplot(t, col = c(rep("grey", n-1), "pink"), names.arg = c(names(t)[1:(n-1)], "NA"), main = plot.title)
}

rm.col <- function(df, colnames, drop=FALSE) {
  return(df[, -which(names(df) %in% colnames), drop = drop])
}

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
