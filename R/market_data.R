## -*- truncate-lines: t; -*-

Shiller <- function(dest.dir,
                    url = "http://www.econ.yale.edu/~shiller/data/ie_data.xls") {

    f.name <- paste0(format(Sys.Date(), "%Y%m%d_"),
                     "ie_data.xls")
    f.path <- file.path(normalizePath(dest.dir), f.name)

    if (!file.exists(f.path))
        dl.result <- download.file(url, destfile = f.path)
    else
        dl.result <- 0

    if (dl.result != 0L) {
        warning("download failed with code ", dl.result, "; see ?download.file")
        return(invisible(NULL))
    }

    if (!requireNamespace("readxl", quietly = TRUE))
        stop("file downloaded, but package ",
             sQuote("readxl"), " is not available")
    if (!requireNamespace("datetimeutils", quietly = TRUE))
        stop("file downloaded, but package ",
             sQuote("datetimeutils"), " is not available")

    data <- suppressMessages(suppressWarnings(
        readxl::read_xls(f.path, sheet = "Data")))
    data <- as.data.frame(data)
    data <- data[-(1:6), ]
    data <- data[, 1:22]
    data <- data[, -c(6, 14, 16)] ## drop column 'Date Fraction' and empty column

    colnames(data) <- c("Date",
                        "Price",
                        "Dividend",
                        "Earnings",
                        "CPI",
                        "Long Rate",
                        "Real Price",
                        "Real Dividend",
                        "Real Total Return Price",
                        "Real Earnings",
                        "Real TR Scaled Earnings",
                        "CAPE",
                        "TR CAPE",
                        "Excess CAPE Yield",
                        "Monthly Total Bond Returns",
                        "Real Total Bond Returns",
                        "Ten Year Annualized Stock Real Return",
                        "Ten Year Annualized Bonds Real Return",
                        "Real 10 Year Excess Annualized Returns"
                        )

    data <- data[!is.na(data[["Date"]]), ]
    tmp <- seq(as.Date("1871-01-01"), by = "1 month",
               length.out = nrow(data))
    data[["Date"]] <- datetimeutils::end_of_month(tmp)

    for (i in 2:ncol(data)) ## there will be NAs => warnings
        data[[i]] <- suppressWarnings(as.numeric(data[[i]]))
    data
}


French <- function(dest.dir,
                   dataset = "F-F_Research_Data_Factors_CSV.zip",
                   weighting = "value",
                   frequency = "monthly",
                   price.series = FALSE,
                   na.rm = FALSE,
                   adjust.frequency = TRUE) {

    .prepare_timestamp <- function(x, freq) {
        if (freq == "monthly")
            timestamp <- datetimeutils::end_of_month(
                                            as.Date(paste0(x, "01"),
                                                    format = "%Y%m%d"))
        else if (freq == "daily")
            timestamp <- as.Date(as.character(x), format = "%Y%m%d")
        else if (freq == "annual")
            timestamp <- x
        else
            stop("unknown frequency")
        timestamp
    }


    if (match.call() == "French()") {

        files <- c(
            "10_Portfolios_Prior_12_2_CSV.zip",
            "10_Portfolios_Prior_12_2_Daily_CSV.zip",
            "49_Industry_Portfolios_CSV.zip",
            "49_Industry_Portfolios_daily_CSV.zip",
            "6_portfolios_2x3_CSV.zip",
            "6_portfolios_2x3_daily_CSV.zip",
            "F-F_Momentum_Factor_CSV.zip",
            "F-F_Momentum_Factor_daily_CSV.zip",
            "F-F_Research_Data_Factors_daily_CSV.zip",
            "ME_Breakpoints_CSV.zip",

            ## univariate sorts
            "Portfolios_Formed_on_ME_CSV.zip",

            ## bivariate sorts
            "Portfolios_Formed_on_BE-ME_CSV.zip",
            "Portfolios_Formed_on_NI_CSV.zip",
            "Portfolios_Formed_on_RESVAR_CSV.zip",
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

    weighting <- tolower(weighting)
    if (!weighting %in% c("equal", "value"))
        stop("weighting must be ", sQuote("equal"),
             " or ",               sQuote("value"))

    cnames <- character()
    attr.list <- list()
    read.ans <- TRUE

    dataset <- basename(dataset)
    if (dataset == "variance")
        url <- "Portfolios_Formed_on_VAR_CSV.zip"
    else if (dataset == "industry49" && frequency == "monthly")
        url <- "49_Industry_Portfolios_CSV.zip"
    else if (dataset == "industry49" && frequency == "daily")
        url <- "49_Industry_Portfolios_daily_CSV.zip"
    else if (dataset == "ff3" && frequency == "daily")
        url <- "F-F_Research_Data_Factors_daily_CSV.zip"
    else if (dataset == "me_breakpoints") {
        url <- "ME_Breakpoints_CSV.zip"
        dataset <- "me_breakpoints_csv.zip"
    } else if (dataset %in% c("market", "rf") &&
             frequency == "daily")
        url <- "F-F_Research_Data_Factors_daily_CSV.zip"
    else if (dataset %in% c("market", "rf"))
        url <- "F-F_Research_Data_Factors_CSV.zip"
    else
        url <- dataset

    if (adjust.frequency        &&
        grepl("daily", dataset) &&
        frequency != "daily") {
        message("Frequency set to daily.\n(Use ",
                sQuote("adjust.frequency = FALSE"),
                " to prevent this.)")
        frequency <- "daily"
    }

    if (grepl("TXT.zip$", dataset)) {
        warning("expected file ending in 'CSV.zip'")
        dataset <- sub("TXT.zip$", "CSV.zip", dataset)
    }

    .ftp <- "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/"

    f.name <- paste0(format(Sys.Date(), "%Y%m%d_"), url)
    f.path <- file.path(normalizePath(dest.dir), f.name)

    if (!file.exists(f.path))
        dl.result <- download.file(paste0(.ftp, url), f.path)
    else
        dl.result <- 0

    if (dl.result != 0L) {
        warning("download failed with code ", dl.result, "; see ?download.file")
        return(invisible(NULL))
    }

    tmp2 <- unzip(f.path)
    txt <- readLines(tmp2)
    file.remove(tmp2)
    dataset <- tolower(dataset)


    ## file-specific handling: either directly return(),
    ## or prepare
    ##         ans - subset (lines) of txt to parse,
    ##               with first column the dates;
    ##               headers should be included
    ##      cnames - column names to use
    ##   attr.list - named list of information to attach

    if (grepl("siccodes", dataset)) {
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

    } else if (tolower(dataset) == "market") {

        cnames <- "Market"
        read.ans <- FALSE

        if (frequency == "daily") {

            i <- grep("Mkt-RF", txt)
            j <- grep("^ *$", txt)[2L] - 1
            ans <- txt[i:j]

        } else {

            i <- grep("Mkt-RF", txt)
            j <- grep("^ *$", txt)[-1] - 1
            if (frequency == "monthly") {
                ans <- txt[i[1]:j[1]]
            } else if (frequency == "annual")
                ans <- txt[i[2]:j[2]]

        }

        ans <- read.table(text = ans,
                          header = TRUE,
                          stringsAsFactors = FALSE,
                          sep = ",",
                          check.names = FALSE,
                          colClasses = "numeric")

        for (cc in seq_len(ncol(ans)))
            ans[[cc]][ ans[[cc]] < -99 ] <- NA

        timestamp <- .prepare_timestamp(ans[[1]], frequency)

        ans <- ans[, -1L, drop = FALSE] ## drop timestamp
        ans <- ans/100
        ans <- ans[, "Mkt-RF", drop = FALSE] +
               ans[, "RF",     drop = FALSE]

    } else if (tolower(dataset) == "rf") {

        cnames <- "rf"
        read.ans <- FALSE

        if (frequency == "daily") {

            i <- grep("Mkt-RF", txt)
            j <- grep("^ *$", txt)[2L] - 1
            ans <- txt[i:j]

        } else {

            i <- grep("Mkt-RF", txt)
            j <- grep("^ *$", txt)[-1] - 1
            if (frequency == "monthly") {
                ans <- txt[i[1]:j[1]]
            } else if (frequency == "annual")
                ans <- txt[i[2]:j[2]]

        }

        ans <- read.table(text = ans,
                          header = TRUE,
                          stringsAsFactors = FALSE,
                          sep = ",",
                          check.names = FALSE,
                          colClasses = "numeric")

        for (cc in seq_len(ncol(ans)))
            ans[[cc]][ ans[[cc]] < -99 ] <- NA

        timestamp <- .prepare_timestamp(ans[[1]], frequency)

        ans <- ans[, -1L, drop = FALSE] ## drop timestamp
        ans <- ans/100
        ans <- ans[, "RF", drop = FALSE]

    } else if (dataset == "me_breakpoints_csv.zip") {

        if (!requireNamespace("datetimeutils", quietly = TRUE))
            stop("file downloaded, but package ",
                 sQuote("datetimeutils"), " is not available")

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
        data[, -1L] <- data[, -1L]*1000000
        return(data)

    } else if (dataset == "6_portfolios_2x3_csv.zip") {

        if (weighting == "value")
            i <- grep("Average Value Weighted Returns -- Monthly", txt)
        else if (weighting == "equal")
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
        attr.list <- list(
            original.headers = strsplit(txt[i], ",")[[1L]][-1L])

    } else if (dataset == "6_portfolios_2x3_daily_csv.zip") {

        frequency <- "daily"
        i <- if (weighting == "equal")
                 grep("Equal Weighted Returns", txt)
             else if (weighting == "value")
                 grep("Value Weighted Returns", txt)

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
        attr.list <- list(
            original.headers = strsplit(txt[i], ",")[[1L]][-1L])

    } else if (dataset == "portfolios_formed_on_me_csv.zip") {

        i <- if (weighting == "equal")
                 grep(paste0("Equal Weight(ed)? Returns.*", frequency),
                      txt, ignore.case = TRUE)
             else if (weighting == "value")
                 grep(paste0("Value Weight(ed)? Returns.*", frequency),
                      txt, ignore.case = TRUE)
        j <- grep("^ *$", txt)
        j <- min( j[j > i] )-1
        i <- i+1
        ans <- txt[i:j]
        ## cnames <- c("small.low",
        ##             "small.neutral",
        ##             "small.high",
        ##             "big.low",
        ##             "big.neutral",
        ##             "big.high")

        i <- grep("number of firms", txt, ignore.case = TRUE) + 1
        j <- grep("^$", txt)
        j <- j[min(which(j > i))] - 1
        info1 <- read.table(text = txt[i:j], header = TRUE,
                            stringsAsFactors = FALSE, sep = ",",
                            check.names = FALSE,
                            colClasses = "numeric")
        row.names(info1) <- as.character(info1[[1L]])
        info1 <- info1[, -1L]

        i <- grep("average firm size", txt, ignore.case = TRUE) + 1
        j <- grep("^$", txt)
        j <- j[min(which(j > i))] - 1
        info2 <- read.table(text = txt[i:j], header = TRUE,
                            stringsAsFactors = FALSE, sep = ",",
                            check.names = FALSE,
                            colClasses = "numeric")
        row.names(info2) <- as.character(info2[[1L]])
        info2 <- info2[, -1L]

        attr.list <- list(
            number.of.firms   = info1,
            average.firm.size = info2)


    } else if (dataset == "10_portfolios_prior_12_2_csv.zip") {

        if (weighting == "value")
            i <- grep("Average Value Weighted Returns -- Monthly", txt)
        else if (weighting == "equal")
            i <- grep("Average Equal Weighted Returns -- Monthly", txt)

        j <- grep("^ *$", txt)
        j <- min( j[j > i] )-1
        i <- i+1
        ans <- txt[i:j]

    } else if (dataset == "10_portfolios_prior_12_2_daily_csv.zip") {

        frequency <- "daily"
        i <- if (weighting == "equal")
                 grep("Equal Weighted Returns", txt)
             else if (weighting == "value")
                 grep("Value Weighted Returns", txt)

        j <- grep("^ *$", txt)
        j <- suppressWarnings(min( j[j > i] )-1)
        if (is.infinite(j))
            j <- length(txt)
        i <- i+1
        ans <- txt[i:j]

    } else if (dataset == "f-f_momentum_factor_daily_csv.zip") {

        frequency <- "daily"
        i <- grep(",Mom", txt)
        j <- grep("^ *$", txt)
        j <- min( j[j > i] )-1
        ans <- txt[i:j]

    } else if (tolower(dataset) == "f-f_momentum_factor_csv.zip") {

        i <- grep(",Mom", txt)
        i <- i[ c("monthly" = 1, "annual" = 2)[frequency] ]
        j <- grep("^[, ]*$", txt)
        j <- min( j[j > i] ) - 1
        ans <- txt[i:j]
        cnames <- "Mom"

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

    ## } else if (tolower(dataset) == "portfolios_formed_on_be-me_csv.zip") {
    ##     if (frequency == "monthly") {
    ##         i <- if (weighting == "equal")
    ##                  grep("Equal Weight Returns -- Monthly", txt)
    ##              else if (weighting == "value")
    ##                  grep("Value Weight Returns -- Monthly", txt)
    ##              else
    ##                  stop("weighting must be 'equal' or 'value'")
    ##     } else if (frequency == "annual") {
    ##         i <- if (weighting == "equal")
    ##                  grep("Equal Weight Returns -- Annual", txt)
    ##              else if (weighting == "value")
    ##                  grep("Value Weight Returns -- Annual", txt)
    ##              else
    ##                  stop("weighting must be 'equal' or 'value'")
    ##     } else
    ##         stop("frequency not supported")
    ##     j <- grep("^ *$", txt)
    ##     j <- j[min(which(j > i))]

    ##     ans <- txt[(i+1):(j-1)]


    } else if (tolower(dataset) %in%
               c("portfolios_formed_on_be-me_csv.zip",
                 "portfolios_formed_on_ni_csv.zip",
                 "portfolios_formed_on_resvar.csv",
                 "portfolios_formed_on_var_csv.zip")) {

        if (frequency == "monthly") {
            i <- if (weighting == "equal")
                     grep("Equal Weight.?.? Returns -- Monthly", txt)
                 else if (weighting == "value")
                     grep("Value Weight.?.? Returns -- Monthly", txt)
                 else
                     stop("weighting must be 'equal' or 'value'")
        } else if (frequency == "annual") {
            i <- if (weighting == "equal")
                     grep("Equal Weight.?.? Returns -- Annual", txt)
                 else if (weighting == "value")
                     grep("Value Weight.?.? Returns -- Annual", txt)
                 else
                     stop("weighting must be 'equal' or 'value'")
        } else
            stop("frequency not supported")
        j <- grep("^ *$", txt)
        j <- j[min(which(j > i))]

        ans <- txt[(i+1):(j-1)]

        if (tolower(dataset) == "portfolios_formed_on_ni_csv.zip") {
            i <- grep("number of firms", txt, ignore.case = TRUE) + 1
            j <- grep("^$", txt)
            j <- j[min(which(j > i))] - 1
            info1 <- read.table(text = txt[i:j], header = TRUE,
                                stringsAsFactors = FALSE, sep = ",",
                                check.names = FALSE,
                                colClasses = "numeric")
            row.names(info1) <- as.character(info1[[1L]])
            info1 <- info1[, -1L]

            i <- grep("average firm size", txt, ignore.case = TRUE) + 1
            j <- grep("^$", txt)
            j <- j[min(which(j > i))] - 1
            info2 <- read.table(text = txt[i:j], header = TRUE,
                                stringsAsFactors = FALSE, sep = ",",
                                check.names = FALSE,
                                colClasses = "numeric")
            row.names(info2) <- as.character(info2[[1L]])
            info2 <- info2[, -1L]

            i <- grep("Average of NI", txt, ignore.case = TRUE) + 1
            j <- grep("^$", txt)
            j <- j[min(which(j > i))] - 1
            info3 <- read.table(text = txt[i:j], header = TRUE,
                                stringsAsFactors = FALSE, sep = ",",
                                check.names = FALSE,
                                colClasses = "numeric")
            row.names(info3) <- as.character(info3[[1L]])
            info3 <- info3[, -1L]/100

            attr.list <- list(
                number.of.firms   = info1,
                average.firm.size = info2,
                value.weighted.average.ni = info3)
        }
    } else  {

        ## default
        message("Dataset not explicitly supported: trying default => check data carefully.")

        if (grepl("daily", dataset) && frequency != "daily")
            warning("daily dataset but frequency not set to daily")

        if (frequency == "annual") {
            i <- if (weighting == "equal")
                     grep("Equal Weight(ed)? Returns.*Annual", txt, ignore.case = TRUE)
                 else if (weighting == "value")
                     grep("Value Weight(ed)? Returns.*Annual", txt, ignore.case = TRUE)
                 else
                     stop("weighting must be 'equal' or 'value'")
        } else if (frequency == "monthly") {
            i <- if (weighting == "equal")
                     grep("Equal Weight(ed)? (Average)? *Returns.*Month", txt, ignore.case = TRUE)
                 else if (weighting == "value")
                     grep("Value Weight(ed)? (Average)? *Returns.*Month", txt, ignore.case = TRUE)
                 else
                     stop("weighting must be 'equal' or 'value'")
        } else if (frequency == "daily") {
            i <- if (weighting == "equal")
                     grep("Equal Weight(ed)? Returns.*Daily", txt, ignore.case = TRUE)
                 else if (weighting == "value")
                     grep("Value Weight(ed)? Returns.*Daily", txt, ignore.case = TRUE)
                 else
                     stop("weighting must be 'equal' or 'value'")
        }
        i <- i[[1]]
        j <- grep("^$", txt)
        if (i > max(j)) {
            j <- length(txt) + 1
        } else
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
            row.names(info1) <- as.character(info1[[1L]])
            info1 <- info1[, -1L]

            i <- grep("average firm size", txt, ignore.case = TRUE) + 1
            j <- grep("^$", txt)
            j <- j[min(which(j > i))] - 1
            info2 <- read.table(text = txt[i:j], header = TRUE,
                                stringsAsFactors = FALSE, sep = ",",
                                check.names = FALSE,
                                colClasses = "numeric")
            row.names(info2) <- as.character(info2[[1L]])
            info2 <- info2[, -1L]

            i <- grep("Sum of BE.* Sum of ME", txt, ignore.case = TRUE) + 1
            j <- grep("^$", txt)
            j <- j[min(which(j > i))] - 1
            info3 <- read.table(text = txt[i:j], header = TRUE,
                                stringsAsFactors = FALSE, sep = ",",
                                check.names = FALSE,
                                colClasses = "numeric")
            row.names(info3) <- as.character(info3[[1L]])
            info3 <- info3[, -1L]

            attr.list <- list(
                number.of.firms   = info1,
                average.firm.size = info2,
                sumBE.sumME       = info3)

        }
    }

    if (!requireNamespace("datetimeutils", quietly = TRUE))
        stop("file downloaded, but package ",
             sQuote("datetimeutils"), " is not available")

    if (read.ans) {
        ans <- read.table(text = ans, header = TRUE,
                          stringsAsFactors = FALSE, sep = ",",
                          check.names = FALSE,
                          colClasses = "numeric")
        for (cc in seq_len(ncol(ans)))
            ans[[cc]][ ans[[cc]] < -99 ] <- NA

        timestamp <- .prepare_timestamp(ans[[1L]], frequency)

        ans <- ans[, -1L, drop = FALSE] ## drop timestamp
        ans <- ans/100
    }

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
        else if (frequency == "annual")
            timestamp <- c(timestamp[1L] - 1, timestamp)

        for (cc in seq_len(ncol(ans))) {
            if (na.rm && any(is.na(ans[[cc]]))) {
                na <- is.na(ans[[cc]])
                first_num <- min(which(!na))
                if (!is.finite(first_num))  ## only NA values
                    next
                ans[[cc]][ na ] <- 0
                ans[[cc]] <- cumprod(1 + ans[[cc]])
                if (first_num > 1)
                    ans[[cc]][seq_len(first_num-1)] <- NA
            } else
                ans[[cc]] <- cumprod(1 + ans[[cc]])
        }
    }


    row.names(ans) <- as.character(timestamp)

    if (length(cnames))
        colnames(ans) <- cnames

    if (length(attr.list))
        for (i in seq_along(attr.list))
            attr(ans, names(attr.list)[i]) <- attr.list[[i]]

    ans
}
