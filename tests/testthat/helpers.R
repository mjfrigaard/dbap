log_maker <- function(size, missing = FALSE) {
  if (size <= 2 & isTRUE(missing)) {
    as.vector(c(TRUE, NA), mode = "logical")
  } else if (size <= 2 & isFALSE(missing)) {
    as.vector(c(TRUE, FALSE), mode = "logical")
  } else if (size > 2 & isTRUE(missing)) {
    rep(c(TRUE, FALSE, NA), length.out = size)
  } else if (size > 2 & isFALSE(missing)) {
    rep(c(TRUE, FALSE), length.out = size)
  }
}

int_maker <- function(size, missing = FALSE) {
  if (size < 3 & isTRUE(missing)) {
    int_raw <- c(as.integer(exp(size)), NA_real_)
    int_vec <- as.vector(int_raw, mode = "integer")
    return(int_vec)
  } else if (size < 3 & isFALSE(missing)) {
    int_raw <- seq.int(from = 1, to = as.integer(exp(size)), length.out = size)
    int_vec <- as.vector(int_raw, mode = "integer")
    return(int_vec)
  } else if (size >= 3 & isTRUE(missing)) {
    adj_size <- round(size * 0.66, digits = 0)
    int_seq <- seq.int(from = 1, to = as.integer(exp(size)), length.out = adj_size)
    int_raw <- rep(c(int_seq, NA_integer_), length.out = size)
    int_vec <- as.vector(int_raw, mode = "integer")
    return(int_vec)
  } else {
    int_raw <- seq.int(from = 1, to = as.integer(exp(size)), length.out = size)
    int_vec <- as.vector(int_raw, mode = "integer")
    return(int_vec)
  }
}

dbl_maker <- function(size, missing = FALSE) {
  if (size < 3 & isTRUE(missing)) {
    dbl_raw <- c(as.double(log(size)), NA_real_)
    dbl_vec <- round(as.vector(dbl_raw, mode = "double"), digits = 3)
    return(dbl_vec)
  } else if (size < 3 & isFALSE(missing)) {
    dbl_raw <- seq.int(from = 0.1, to = as.double(size / 2), length.out = size)
    dbl_vec <- round(as.vector(dbl_raw, mode = "double"), digits = 3)
    return(dbl_vec)
  } else if (size >= 3 & isTRUE(missing)) {
    adj_size <- round(size * 0.33, digits = 0)
    dbl_seq <- seq.int(from = 0.1, to = as.double(size / 2), length.out = adj_size)
    dbl_raw <- rep(c(dbl_seq, NA_real_), length.out = size)
    dbl_vec <- round(as.vector(dbl_raw, mode = "double"), digits = 3)
    return(dbl_vec)
  } else {
    dbl_raw <- seq.int(from = 0.1, to = as.double(size / 2), length.out = size)
    dbl_vec <- round(as.vector(dbl_raw, mode = "double"), digits = 3)
    return(dbl_vec)
  }
}

chr_maker <- function(size, lvls, missing = FALSE) {
  if (size < lvls) {
    lvls <- size - 1
  }
  if (isTRUE(missing) & size < 3) {
    chr_vec <- as.vector(c("item: 1", NA_character_),
      mode = "character"
    )
  } else if (isFALSE(missing) & size < 3) {
    chr_vec <- as.vector(c("item: 1", "item: 2"),
      mode = "character"
    )
  } else if (isTRUE(missing) & size >= 3) {
    adj_size <- size - 1
    levs <- paste0("item:", as.integer(1:lvls))
    adj_chr <- rep(c(levs, NA_character_), length.out = adj_size)
    nas <- rep(adj_chr, length.out = size)
    chr_vec <- as.vector(c(nas), mode = "character")
  } else {
    levs <- paste0("item:", as.integer(1:lvls))
    chr_raw <- rep(levs, length.out = size)
    chr_vec <- as.vector(c(chr_raw), mode = "character")
  }
  return(chr_vec)
}

fct_maker <- function(size, lvls, ord = FALSE, missing = FALSE) {
  if (size < lvls) {
    lvls <- size - 1
  }
  if (isTRUE(missing) & isTRUE(ord)) {
    levs <- paste0("level ", as.integer(1:lvls))
    nas <- rep(c(levs, NA_character_), length.out = size)
    chr_raw <- as.vector(c(nas), mode = "character")
    fct_vec <- factor(chr_raw,
      levels = unique(sort(chr_raw)),
      ordered = TRUE
    )
  } else if (isFALSE(missing) & isTRUE(ord)) {
    levs <- paste0("level ", as.integer(1:lvls))
    chr_raw <- rep(levs, length.out = size)
    ord_levels <- sort(unique(chr_raw))
    fct_vec <- factor(chr_raw, levels = ord_levels, ordered = TRUE)
  } else if (isTRUE(missing) & isFALSE(ord)) {
    levs <- paste0("group ", as.integer(1:lvls))
    nas <- rep(c(levs, NA_character_), length.out = 10)
    chr_raw <- as.vector(c(nas), mode = "character")
    fct_vec <- factor(chr_raw, levels = unique(sort(chr_raw)))
  } else {
    levs <- paste0("group ", as.integer(1:lvls))
    chr_raw <- rep(levs, length.out = size)
    fct_levels <- unique(chr_raw)
    fct_vec <- factor(chr_raw, levels = fct_levels)
  }
  return(fct_vec)
}

