## -*- truncate-lines: t; -*-

Shiller <- function(destfile = tempfile(fileext = ".xls"),
                    url = "http://www.econ.yale.edu/~shiller/data/ie_data.xls") {

    download.file(url, destfile = destfile)

    data <- read_xls(destfile, sheet = 3)
    data <- as.data.frame(data)
    data <- data[-(1:6), ] 
    data <- data[ , 1:11]
    data <- data[ ,-6] ## drop column 'Date Fraction' 
    
    colnames(data) <- c("Date", "Price", "Dividend", "Earnings",
                        "CPI", "Long Rate", "Real Price", "Real Dividend",
                        "Real Earnings", "CAPE")
    data[["Date"]] <- gsub("[.]1$", ".10", data[["Date"]])
    data[["Date"]] <- datetimeutils::end_of_month(
                                         as.Date(paste0(data[["Date"]], ".1"),
                                                 "%Y.%m.%d"))

    for (i in 2:10) ## there will be NAs => warnings
        data[[i]] <- suppressWarnings(as.numeric(data[[i]])) 
    data <- data[!is.na(data[["Date"]]), ]
    data
}
