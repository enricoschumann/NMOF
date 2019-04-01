## -*- truncate-lines: t; -*-

Shiller <- function(dest.dir,
                    url = "http://www.econ.yale.edu/~shiller/data/ie_data.xls") {

    f.name <- paste0(format(Sys.Date(), "%Y%m%d_"),
                     "ie_data.xls")
    f.path <- file.path(normalizePath(dest.dir), f.name)

    if (!file.exists(f.path))
        download.file(url, destfile = f.path)

    if (!requireNamespace("readxl", quietly = TRUE))
        stop("package ", sQuote("readxl"), " is not available")
    if (!requireNamespace("datetimeutils", quietly = TRUE))
        stop("package ", sQuote("datetimeutils"), " is not available")

    data <- readxl::read_xls(f.path, sheet = 3)
    data <- as.data.frame(data)
    data <- data[-(1:6), ]
    data <- data[, 1:11]
    data <- data[, -6] ## drop column 'Date Fraction'

    colnames(data) <- c("Date", "Price", "Dividend", "Earnings",
                        "CPI", "Long Rate", "Real Price",
                        "Real Dividend", "Real Earnings", "CAPE")

    data <- data[!is.na(data[["Date"]]), ]
    tmp <- data[["Date"]]
    tmp <- strsplit(format(round(as.numeric(tmp), 2), nsmall = 2), ".",
                    fixed = TRUE)

    tmp <- as.Date(
        sapply(tmp, function(x) paste(x[1], x[2], "1", sep = "-")))
    data[["Date"]] <- datetimeutils::end_of_month(tmp)

    for (i in 2:10) ## there will be NAs => warnings
        data[[i]] <- suppressWarnings(as.numeric(data[[i]]))
    data
}


French <- function(dest.dir,
                   dataset = "F-F_Research_Data_Factors_CSV.zip",
                   weighting = "value",
                   frequency = "monthly",
                   price.series = FALSE,
                   na.rm = FALSE) {

    if (match.call() == "French()") {
        
        files <- c(
            "49_Industry_Portfolios_CSV.zip",
            "49_Industry_Portfolios_daily_CSV.zip",
            "6_portfolios_2x3_CSV.zip",
            "F-F_Research_Data_Factors_daily_CSV.zip",
            "ME_Breakpoints_CSV.zip",
            "Portfolios_Formed_on_BE-ME_CSV.zip",
            "Portfolios_Formed_on_VAR_CSV.zip",
            "Portfolios_Formed_on_VAR_CSV.zip",

            "Siccodes5.zip",
            "Siccodes10.zip",
            "Siccodes12.zip",
            "Siccodes17.zip",
            "Siccodes30.zip",
            "Siccodes38.zip",
            "Siccodes48.zip",
            "Siccodes49.zip"
        )
        cat(sort(files), sep = "\n")
        return(invisible(files))
    }
    cnames <- NULL

    url <- if (dataset == "variance")
               "Portfolios_Formed_on_VAR_CSV.zip"
           else if (dataset == "industry49" && frequency == "monthly")
               "49_Industry_Portfolios_CSV.zip"
           else if (dataset == "industry49" && frequency == "daily")
               "49_Industry_Portfolios_daily_CSV.zip"
           else if (dataset == "ff3" && frequency == "daily")
               "F-F_Research_Data_Factors_daily_CSV.zip"
           else if (dataset == "me_breakpoints")
               "ME_Breakpoints_CSV.zip"
           else
               dataset

    if (grepl("TXT.zip$", dataset)) {
        warning("expected file ending in 'CSV.zip'")
        dataset <- sub("TXT.zip$", "CSV.zip", dataset)
    }
            
    .ftp <- "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/"

    f.name <- paste0(format(Sys.Date(), "%Y%m%d_"), url)
    f.path <- file.path(normalizePath(dest.dir), f.name)

    if (!file.exists(f.path))
        download.file(paste0(.ftp, url), f.path)

    tmp2 <- unzip(f.path)
    txt <- readLines(tmp2)
    file.remove(tmp2)
    dataset <- tolower(dataset)

    if (grepl("daily", dataset) && frequency != "daily")
        warning("daily dataset but frequency not set to daily")
    
    if (grepl("siccodes", tolower(dataset))) {
        ans <- NULL
        for (i in seq_along(txt)) {
            if (grepl("^ ?[0-9]", txt[i])) {
                ind <- txt[i]
            } else if (!grepl("^ *$", txt[i])) {
                ans <- rbind(ans,
                             data.frame(Industry = ind,
                                        Codes = txt[i],
                                        stringsAsFactors = FALSE))
            }
        }
        ans$Industry <- trimws(ans$Industry)
        num <- gsub(" *([0-9]+) .*", "\\1", ans$Industry)
        abbr <- gsub(" *[0-9]+ ([^ ]+) .*", "\\1", ans$Industry)
        industry <- gsub(" *[0-9]+ [^ ]+ (.*)", "\\1", ans$Industry)

        ans$Codes <- trimws(ans$Codes)
        ans <- data.frame(num = trimws(num),
                          abbr = trimws(abbr),
                          industry = trimws(industry),
                          industry_group = substr(ans$Codes, 10, 100000),
                          start = substr(ans$Codes, 1, 4),
                          end   = substr(ans$Codes, 6, 9),
                          stringsAsFactors = FALSE)
        return(ans)
    } else if (dataset == "me_breakpoints") {
        data <- read.table(text = txt, skip = 1,
                           sep = ",",
                           header = FALSE,
                           stringsAsFactors = FALSE,
                           strip.white = TRUE,
                           fill = TRUE)
        tmp <- datetimeutils::end_of_month(
                                  as.Date(paste0(data[[1]], "01"), "%Y%m%d"))
        data <- data[!is.na(tmp), , drop = FALSE]
        data <- data[, -1L]
        colnames(data) <- c("companies", paste0("Q", seq(5,100, by = 5)))
        row.names(data) <- as.character( tmp[!is.na(tmp)] )
        data[, -1] <- data[, -1]*1000000
        return(data)
    } else if (dataset == "6_portfolios_2x3_csv.zip") {
        if (tolower(weighting) == "value")
            i <- grep("Average Value Weighted Returns -- Monthly", txt)
        else if (tolower(weighting) == "equal")
            i <- grep("Average Equal Weighted Returns -- Monthly", txt)

        j <- grep("^ *$", txt)
        j <- min( j[j > i] )-1
        i <- i+1
        ans <- txt[i:j]
        cnames <- c("small.low",
                    "small.neutral",
                    "small.high",
                    "big.low",
                    "big.neutral",
                    "big.high")
    } else if (tolower(dataset) == "f-f_research_data_factors_csv.zip") {

        i <- grep("Mkt-RF", txt)
        j <- grep("^ *$", txt[-c(1:10)]) + 9
        if (frequency == "monthly") {
            ans <- txt[i[1]:j[1]]
        } else if (frequency == "annual")
            ans <- txt[i[2]:j[2]]
        else
            stop("frequency not supported")

        
    } else if (tolower(dataset) == "f-f_research_data_factors_daily_csv.zip") {

        frequency <- "daily"
        i <- grep("Mkt-RF", txt)
        j <- grep("^ *$", txt[-c(1:10)]) + 9
        ans <- txt[i:j]
        
    } else if (tolower(dataset) == "portfolios_formed_on_be-me_csv.zip") {
        if (frequency == "monthly") {
            i <- if (tolower(weighting) == "equal")
                     grep("Equal Weight Returns -- Monthly", txt)
                 else if (tolower(weighting) == "value")
                     grep("Value Weight Returns -- Monthly", txt)
                 else
                     stop("weighting must be 'equal' or 'value'")
        } else if (frequency == "annual") {
            i <- if (tolower(weighting) == "equal")
                     grep("Equal Weight Returns -- Annual", txt)
                 else if (tolower(weighting) == "value")
                     grep("Value Weight Returns -- Annual", txt)
                 else
                     stop("weighting must be 'equal' or 'value'")            
        } else
            stop("frequency not supported")
        j <- grep("^ *$", txt)
        j <- j[min(which(j > i))]

        ans <- txt[(i+1):(j-1)]

        
    } else if (tolower(dataset) != "f-f_research_data_factors_csv.zip") {
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

        if (grepl("industry_portfolios", dataset, ignore.case = TRUE) &&
            frequency != "daily") {
            i <- grep("number of firms", txt, ignore.case = TRUE) + 1
            j <- grep("^$", txt)
            j <- j[min(which(j > i))] - 1
            info1 <- read.table(text = txt[i:j], header = TRUE,
                                stringsAsFactors = FALSE, sep = ",",
                                check.names = FALSE,
                                colClasses = "numeric")
            
            i <- grep("average firm size", txt, ignore.case = TRUE) + 1
            j <- grep("^$", txt)
            j <- j[min(which(j > i))] - 1
            info2 <- read.table(text = txt[i:j], header = TRUE,
                                stringsAsFactors = FALSE, sep = ",",
                                check.names = FALSE,
                                colClasses = "numeric")

        }
        

        
    } else {
        warning("dataset not supported")
        
        i <- grep("Mkt-RF", txt)
        j <- grep("^ *$", txt[-c(1:10)]) + 9
        ans <- txt[i:j]
    }

    ans <- read.table(text = ans, header = TRUE,
                      stringsAsFactors = FALSE, sep = ",",
                      check.names = FALSE,
                      colClasses = "numeric")
    for (cc in seq_len(ncol(ans)))
        ans[[cc]][ ans[[cc]] < -99 ] <- NA

    if (!requireNamespace("datetimeutils"))
        stop("package ", sQuote("datetimeutils"), " is required")

    if (frequency == "monthly")
        timestamp <- datetimeutils::end_of_month(
                     as.Date(paste0(ans[[1]], "01"), format = "%Y%m%d"))
    else if (frequency == "daily")
        timestamp <- as.Date(as.character(ans[[1L]]), format = "%Y%m%d")
    else if (frequency == "annual")
        timestamp <- ans[[1]]
        
    ans <- ans[, -1L] ## drop timestamp
    ans <- ans/100

    if (price.series) {
        r0 <- numeric(ncol(ans))
        r0[is.na(ans[1L, ])] <- NA
        ans <- rbind(r0, ans)
        if (frequency == "monthly")
            timestamp <- c(datetimeutils::end_of_previous_month(timestamp[1L]),
                           timestamp)
        else if (frequency == "daily")
            timestamp <- c(datetimeutils::previous_businessday(timestamp[1L]),
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
    if (!is.null(cnames))
        colnames(ans) <- cnames
    row.names(ans) <- as.character(timestamp)

    if (grepl("industry_portfolios", dataset, ignore.case = TRUE) &&
        frequency != "daily") {
        attr(ans, "number.of.firms") <- info1
        attr(ans, "average.firm.size") <- info2
    }
    ans
}
