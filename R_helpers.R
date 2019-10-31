stop("R_helpers is now replaced with a package 'rhelpers'
       Install using devtools as follows:
       > library(devtools)
       > setwd('rhelpers_parent_directory')
       > install('rhelpers')")

# ## return start of a vector or top left corner of a data-frame/matrix
# pv <- preview <- function(df, rows = 10, cols = NULL,
#                           tail = NULL, tail.rows = NULL, tail.cols = NULL) {
#   if ((!is.null(tail.rows) && !is.null(tail) && tail != tail.rows) ||
#       (!is.null(tail.cols) && !is.null(tail) && tail != tail.cols)) {
#     warning("tail.rows and tail.cols override tail argument")
#   }
#   if (is.null(tail)) {
#     tail <- FALSE
#   }
#   if (is.null(tail.rows)) {
#     tail.rows <- tail
#   }
#   if (is.null(tail.cols)) {
#     tail.cols <- tail
#   }
#   if (is.null(dim(df))) {
#     ## Assume 1D object
#     obj.len <- length(df)
#     rows <- min(rows, obj.len)
#     if (tail.rows) {
#       start.row <- obj.len - rows + 1
#       end.row <- obj.len
#     } else {
#       start.row <- 1
#       end.row <- rows
#     }
#     return(df[start.row:end.row])
#   } else {
#     df.nrow <- nrow(df)
#     df.ncol <- ncol(df)
#     if(is.null(cols)) {
#       cols <- rows
#     }
#     rows <- min(rows, df.nrow)
#     cols <- min(cols, df.ncol)
#     if (tail.rows) {
#       start.row <- df.nrow - rows + 1
#       end.row <- df.nrow
#     } else {
#       start.row <- 1
#       end.row <- rows
#     }
#     if (tail.cols) {
#       start.col <- df.ncol - cols + 1
#       end.col <- df.ncol
#     } else {
#       start.col <- 1
#       end.col <- cols
#     }
#     return(df[start.row:end.row, start.col:end.col])
#   }
# }
# 
# ## grep-like function that returns matches
# grepp <- function (pattern, x, ignore.case = FALSE, perl = FALSE, value = FALSE, 
#                    fixed = FALSE, useBytes = FALSE, invert = FALSE) {
#   return(x[grep(pattern, x, ignore.case, perl, value, fixed, useBytes, invert)])
# }
# 
# ## table, with NA included by default
# tblNA <- function(...) {
#   table(..., useNA = "always")
# }
# 
# ## quick visualisation of tblNA
# bar.tbl <- function(x, ..., plot.title = NULL) {
#   t <- tblNA(x, ...)
#   n <- length(t)
#   barplot(t, col = c(rep("grey", n-1), "pink"), names.arg = c(names(t)[1:(n-1)], "NA"), main = plot.title)
# }
# 
# ## remove named columns from a data-frame
# rm.col <- function(df, colnames, drop = FALSE) {
#   if(sum(names(df) %in% colnames) == 0) {
#     warning("Empty colname list provided or no column names match")
#     return(df)
#   } else {
#     return(df[, -which(names(df) %in% colnames), drop = drop])
#   }
# }
# 
# ## shortcut to calculate proportion of elements that are TRUE with options for NA handling (also works for 0/1 coding)
# proportion.true <- function(x, na.handling = "keep") {
#   if (is.logical(x) || sum(x %in% c(0, 1)) == sum(!is.na(x))) {
#     if (na.handling == "keep") {
#       return(mean(x))
#     } else if (na.handling == "remove") {
#       return(mean(x, na.rm = TRUE))
#     } else if (na.handling == "as.false") {
#       x[is.na(x)] <- FALSE
#       return(mean(x))
#     } else {
#       stop("na.handling must be one of 'keep', 'remove' or 'false'")  
#     }
#   } else {
#     stop("Require logical or 0/1 object")
#   }
# }
# 
# ## read in data using my default settings
# read.default <- function(file,
#                          path = NULL,
#                          header = TRUE,
#                          sep = "\t",
#                          quote = "",
#                          na.strings = "no na strings",
#                          comment.char = "",
#                          stringsAsFactors = FALSE,
#                          as.char = FALSE,
#                          ...) {
#   if(is.null(path)) {
#     full.file <- file
#   } else {
#     full.file <- paste0(path, "/", file)
#   }
#   if (as.char) {
#     x <- read.table(file = full.file,
#                     header = header,
#                     sep = sep,
#                     quote = quote,
#                     na.strings = na.strings,
#                     comment.char = comment.char,
#                     stringsAsFactors = stringsAsFactors,
#                     colClasses = "character",
#                     ...)
#   } else {
#     x <- read.table(file = full.file,
#                     header = header,
#                     sep = sep,
#                     quote = quote,
#                     na.strings = na.strings,
#                     comment.char = comment.char,
#                     stringsAsFactors = stringsAsFactors,
#                     ...)
#   }
#   return(x)
# }
# 
# ## write out data using my default settings
# write.default <- function(x, file,
#                           need.row.names = FALSE,
#                           need.col.names = TRUE,
#                           quote = FALSE,
#                           sep = "\t") {
#   write.table(x,
#               file = file,
#               quote = quote,
#               sep = sep,
#               row.names = need.row.names,
#               col.names = need.col.names)
# }
# 
# ## Clear global environment
# rmall <- function() {
#   rm(list=ls(envir = .GlobalEnv), envir = .GlobalEnv)
# }
# 
# ## Reverse levels of a factor
# reverse.levels <- function(x) {
#   if (!is.factor(x)) {
#     stop("x not a factor")
#   } else {
#     z <- factor(x, levels = rev(levels(x)))
#     return(z)
#   }
# }
# 
# ## Quick QQ plot
# quick.qq <- function(pvals, do.it.anyway=FALSE, alternative.main=NULL) {
#   
#   if ((length(pvals) > 10000) && !do.it.anyway) {
#     stop("Too many p-values. Can override with do.it.anyway argument.")
#   }
#   
#   chisq <- qchisq(1 - pvals, 1)
#   lambda <- round(median(chisq)/qchisq(0.5, 1),3)
#   n <- length(pvals)
#   
#   temp <- pvals[order(pvals, na.last = TRUE)]
#   temp.logP <- -log10(temp)
#   temp.exp <- -log10(1:n/n)
#   
#   if(is.null(alternative.main)) {
#     plot(x=temp.exp, y=temp.logP, xlab="expected", ylab="observed", pch=16, main=paste0("(log) QQ plot, n = ", n))
#   } else {
#     plot(x=temp.exp, y=temp.logP, xlab="expected", ylab="observed", pch=16, main=alternative.main)
#   }
#   abline(0, 1, col = "red")
#   legend("topleft", paste0("lambda = ", lambda), bty="n")
#   
# }
# 
# ## Enumerate instances
# enumerate.instances <- function(x) {
#   if(class(x) != "character") {
#     warning("Non-character input is converted to character; this may round numeric types etc")
#   }
#   df <- data.frame(x=as.character(x), stringsAsFactors = FALSE)
#   df$ct <- 1
#   dups <- duplicated(paste0(df$x, df$ct))
#   while (sum(dups) > 0) {
#     df[dups, "ct"] <- df[dups, "ct"] + 1
#     dups <- duplicated(paste0(df$x, df$ct))
#   }
#   return(df$ct)
# }
# 
# ## See most common occurrences
# most.common.vals <- function(..., n = 6) {
#   return(head(sort(tblNA(...), decreasing = TRUE), n = n))
# }
# 
# ## Count NAs
# sumNA <- function(x) {
#   y <- c(sum(is.na(x)), sum(!is.na(x)))
#   names(y) <- c("is_NA", "not_NA")
#   return(y)
# }
# 
# ## Return a range of colours like ggplot
# gg_color_hue <- function(n) {
#   hues = seq(15, 375, length = n + 1)
#   hcl(h = hues, l = 65, c = 100)[1:n]
# }
