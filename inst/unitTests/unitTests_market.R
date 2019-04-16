test.French <- function() {

    library("NMOF")
    library("RUnit")

    archive.dir <- "~/Downloads/French"
    if (!dir.exists(archive.dir))
        dir.create(archive.dir)


    French("~/Downloads/French", "market",
           frequency = "daily",
           price.series = TRUE)
    
    French("~/Downloads/French", "market",
           frequency = "monthly",
           price.series = TRUE)
    
    French("~/Downloads/French", "market",
           frequency = "annual",
           price.series = TRUE)
    
    French("~/Downloads/French", "rf",
           frequency = "daily",
           price.series = TRUE)
    
    French("~/Downloads/French", "rf",
           frequency = "monthly",
           price.series = TRUE)
    
    French("~/Downloads/French", "rf",
           frequency = "annual",
           price.series = TRUE)



    
    dataset <- "6_Portfolios_2x3_CSV.zip"

    data <- French(archive.dir, dataset, price.series = TRUE)
    checkTrue(all(data[1, ] == 1))
    checkEquals(row.names(data)[1], "1926-06-30")
    checkEquals(row.names(data)[2], "1926-07-31")

    data <- French(archive.dir, dataset, price.series = FALSE)
    checkEquals(row.names(data)[1], "1926-07-31")
    checkEquals(row.names(data)[2], "1926-08-31")



    dataset <- "6_Portfolios_2x3_daily_CSV.zip"

    data <- French(archive.dir, dataset,
                   price.series = TRUE)
    checkTrue(all(data[1, ] == 1))
    checkEquals(row.names(data)[1], "1926-06-30")
    checkEquals(row.names(data)[2], "1926-07-01")







    data <- French(archive.dir, "F-F_Momentum_Factor_daily_CSV.zip",
                   price.series = TRUE, frequency = "daily")
    data <- French(archive.dir, "F-F_Momentum_Factor_CSV.zip",
                   price.series = TRUE)

    data <- French(archive.dir, "6_Portfolios_ME_Prior_12_2_CSV.zip",
                   price.series = TRUE)
    plot(data[,1])
    data <- French(archive.dir, "6_Portfolios_ME_Prior_12_2_Daily_CSV.zip",
                   price.series = TRUE, frequency = "daily")
    plot(as.Date(row.names(data)), data[,1])



    files <- French()
    checkTrue(is.character(files))
    checkTrue(length(files) > 1)

    data <- French(archive.dir, "49_Industry_Portfolios_daily_CSV.zip",
                   frequency = "daily", weighting = "value")

    P <- French("~/Downloads/French",  ## path where to store raw file
                dataset = "48_Industry_Portfolios_daily_CSV.zip",
                weighting = "value",
                frequency = "daily",
                price.series = TRUE,
                na.rm = TRUE)





}
