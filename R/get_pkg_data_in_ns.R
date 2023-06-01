get_pkg_data_in_ns <- function(package) {
  for (i in seq_along(package)) {
    browser() # add browser()
  		if (!isNamespaceLoaded(package[i]))
  			if (requireNamespace(package[i], quietly = TRUE))
  				cat(paste("Loading package:", package[i], "\n"))
  			else stop(paste("Package", package[i], "is not available"))
  	}
  	dsitems <- data(package = package)$results
  	wanted <- c('Package', 'Item','Title')
  	ds <- as.data.frame(dsitems[,wanted], stringsAsFactors = FALSE)
  	return(ds)
}

get_pkg_data_in_ns("fs")
