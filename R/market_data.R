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

.ftp <- "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/"

French <- function(dataset = "variance", weighting = "equal",
                   frequency = "monthly", price.series = FALSE,
                   na.rm = TRUE) {
    dataset <- tolower(dataset)
    file <- if (dataset == "variance")        
                "Portfolios_Formed_on_VAR_CSV.zip"
            else if (dataset == "industry49" && frequency == "monthly")
                "49_Industry_Portfolios_CSV.zip"
            else if (dataset == "industry49" && frequency == "daily")
                "49_Industry_Portfolios_daily_CSV.zip"

    file <- paste0(.ftp, file)
    
    tmp <- tempfile()
    download.file(file, tmp, quiet = TRUE)
    
    tmp2 <- unzip(tmp)
    txt <- readLines(tmp2)
    file.remove(tmp2)
    

    i <- if (tolower(weighting) == "equal")
             grep("Equal Weighted Returns", txt)
         else if (tolower(weighting) == "value")
             grep("Value Weighted Returns", txt)
         else
            stop("weighting must be 'equal' or 'value'")
    i <- i[[1]]
    j <- grep("^$", txt)
    j <- j[min(which(j > i))]
    
    ans <- txt[(i+1):(j-1)]
    ans <- read.table(text = ans, header = TRUE,
                      stringsAsFactors = FALSE, sep = ",",
                      check.names = FALSE)
    for (cc in seq_len(ncol(ans)))
        ans[[cc]][ ans[[cc]] < -99 ] <- NA

    if (!requireNamespace("datetimeutils"))
        stop("package ", sQuote("datetimeutils"), " is required")

    if (frequency == "monthly")
        timestamp <- datetimeutils::end_of_month(
                                        as.Date(paste0(ans[[1]], "01"),
                                                format = "%Y%m%d"))
    else if (frequency == "daily")
        timestamp <- as.Date(as.character(ans[[1L]]), format = "%Y%m%d")
    
    ans <- ans[ , -1L] ## drop timestamp
    ans <- ans/100
    
    if (price.series) {
        r0 <- numeric(ncol(ans))
        r0[is.na(ans[1L, ])] <- NA
        ans <- rbind(r0, ans)
        timestamp <- if (frequency == "monthly")
                         c(datetimeutils::end_of_previous_month(timestamp[1L]),
                           timestamp)
                     else if (frequency == "daily")
                         c(datetimeutils::previous_businessday(timestamp[1L]),
                           timestamp)
                         
        for (cc in seq_len(ncol(ans))) {
            if (na.rm && any(is.na(ans[[cc]]))) {
                na <- is.na(ans[[cc]])
                first_num <- min(which(!na))
                ans[[cc]][ na ] <- 0
                ans[[cc]] <- cumprod(1 + ans[[cc]])
                if (first_num > 1)
                    ans[[cc]][seq_len(first_num-1)] <- NA
            } else                
                ans[[cc]] <- cumprod(1 + ans[[cc]])
        }
        
    }
    row.names(ans) <- as.character(timestamp)
    ans
}
