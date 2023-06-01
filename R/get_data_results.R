#' Re-worked version of datasets() from (vcdExtra)
#'
#' @param pkg name of package
#' @param allClass logical, include all classes of the item (`TRUE`) or just
#' the last class (`FALSE`)
#' @param incPackage logical, include package name in result?
#' @param maxTitle maximum length of data set Title
#'
#' @return metadata table of data object
#'
#' @export get_data_results
#'
#' @examples
#' get_data_results("lubridate", incPackage = TRUE)
get_data_results <- function(pkg, allClass=FALSE,
		incPackage=length(pkg) > 1,
		maxTitle=NULL)
{
	# make sure requested packages are available and loaded
	for (i in seq_along(pkg)) {
		if (!isNamespaceLoaded(pkg[i]))
			if (requireNamespace(pkg[i], quietly = TRUE))
				cat(paste("Loading package:", pkg[i], "\n"))
			else stop(paste("Package", pkg[i], "is not available"))
	}
	dsitems <- data(package = pkg)$results
	wanted <- c('Package', 'Item','Title')
	if (nrow(dsitems) < 2) {
	ds <- data.frame(Package = dsitems[ , "Package"],
	                 Item = dsitems[ , "Item"],
                   Title = dsitems[ , "Title"],
	                    stringsAsFactors = FALSE)
	} else {
	  ds <- as.data.frame(dsitems[,wanted],
	                    stringsAsFactors = FALSE)
	}

  getData <- function(x, pkg) {
	  # fix items with " (...)" in names, e.g., "BJsales.lead (BJsales)" in datasets
	  objname <- gsub(" .*", "", x)

	  e <- loadNamespace(pkg)
	  if (!exists(x, envir = e)) {
	    dataname <- sub("^.*\\(", "", x)
	    dataname <- sub("\\)$", "", dataname)
	    e <- new.env()
	    data(list = dataname, package = pkg, envir = e)
	  }
	  get(objname, envir = e)
  }
	getDim <- function(i) {
	  data <- getData(ds$Item[i], ds$Package[i])
		if (is.null(dim(data))) length(data) else paste(dim(data), collapse = 'x')
	}
	getClass <- function(i) {
	  data <- getData(ds$Item[i], ds$Package[i])
		cl <- class(data)
		if (length(cl) > 1 && !allClass) cl[length(cl)] else cl
	}
	ds$dim <- unlist(lapply(seq_len(nrow(ds)), getDim ))
	ds$class <- unlist(lapply(seq_len(nrow(ds)), getClass ))
	if (!is.null(maxTitle)) ds$Title <- substr(ds$Title, 1, maxTitle)
	if (incPackage)
		ds[c('Package', 'Item','class','dim','Title')]
	else
		ds[c('Item','class','dim','Title')]
}



# dsitems <- data(package = "dplyr")$results
# dsitems
# dsitem <- data(package = "forcats")$results
#
# dim(dsitems)
# dim(dsitem)
# nrow(dsitems)
# nrow(dsitem)
#
# dsitems[ , "Package"]
# dsitem[ , "Package"]


