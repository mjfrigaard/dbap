unloadNamespace("fs")
loadedNamespaces()
"fs" %in% loadedNamespaces()
isNamespaceLoaded("fs")

requireNamespace("fs")
"fs" %in% loadedNamespaces()
isNamespaceLoaded("fs")

unloadNamespace("fs")
"fs" %in% loadedNamespaces()
# isNamespaceLoaded("utils")
# "utils" %in% loadedNamespaces()
#
#
#
# # separate the namespace items and convert to purrr
# check_ns_pkg <- function(pkg) {
#   if (!isNamespaceLoaded(pkg)) {
#       requireNamespace(pkg, quietly = FALSE)
#   }
# }
# loadedNamespaces()
# "fs" %in% loadedNamespaces()
# isNamespaceLoaded("fs")
# check_ns_pkg("fs")
#
# isNamespaceLoaded("bla")
# requireNamespace("bla", quietly = FALSE)
#
# isNamespaceLoaded("utils")
# "utils" %in% loadedNamespaces()
# check_ns_pkg("fs")
# check_ns_pkg("utils")
#
# get_pkg_ns <- function(package) {
#   pkg_loaded <- purrr::map_vec(package, check_ns_pkg)
#   if (isTRUE(pkg_loaded)) {
#   	dsitems <- data(package = package)$results
#   	wanted <- c('Package', 'Item','Title')
#   	ds <- as.data.frame(dsitems[,wanted], stringsAsFactors = FALSE)
#   	return(ds)
#   }
# }
# loadedNamespaces()
# "fs" %in% loadedNamespaces()
# isNamespaceLoaded("fs")
# isNamespaceLoaded("utils")
# "utils" %in% loadedNamespaces()
# get_pkg_ns("fs")
# get_pkg_ns("dplyr")
#
# get_pkg_data_in_ns("fs")
# get_pkg_data_in_ns("bla")
# get_pkg_data_in_ns("ggplot2")
