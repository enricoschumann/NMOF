##
makeInteger <- function(x, label, min = 1L) {
    x <- as.integer(x)
    if (is.na(x) || x < min)
        stop(label, " must be an integer greater/equal to ", min)
    x
}

##
anyNA <- function(x)
    if (!is.null(x) && !is.function(x)) any(is.na(x)) else FALSE

##
checkList <- function(passedList, defaultList, label = "'algo'") {
    ## NAs in list
    if (any(sapply(passedList,anyNA)))
        stop("NAs are not allowed in list ", label)

    ## unnamed elements in list
    if ("" %in% names(passedList))
        warning("'algo' contained unnamed elements",
                call. = FALSE)

    ## ununsed elements in list
    unusedOptions <- setdiff(names(passedList), names(defaultList))
    unusedOptions <- setdiff(unusedOptions, "")
    if (length(unusedOptions))
        warning("unknown names in 'algo': ",
                paste(unusedOptions, collapse = ", "),
                call. = FALSE)
}

##
mRU <- function(m, n) array(runif(m*n), dim = c(m,n))
mRN <- function(m, n) array(rnorm(m*n), dim = c(m,n))
