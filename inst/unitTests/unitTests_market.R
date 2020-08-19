test.French <- function() {

    ## library("NMOF")
    ## library("RUnit")

    archive.dir <- "~/Downloads/French"
    if (!dir.exists(archive.dir))
        dir.create(archive.dir)


    ## market
    data <- French(archive.dir,
                   "market",
                   frequency = "daily",
                   price.series = FALSE)
    checkEquals(row.names(data)[1], "1926-07-01")
    checkEquals(row.names(data)[2], "1926-07-02")

    data <- French(archive.dir,
                   "market",
                   frequency = "daily",
                   price.series = TRUE)
    checkEquals(row.names(data)[1], "1926-06-30")


    data <- French(archive.dir, "market",
                   frequency = "monthly",
                   price.series = FALSE)
    checkEquals(row.names(data)[1], "1926-07-31")
    checkTrue(min(data) > -1)
    checkTrue(max(data) < 2)

    data <- French(archive.dir, "market",
                   frequency = "monthly",
                   price.series = TRUE)
    checkEquals(row.names(data)[1], "1926-06-30")

    data <- French(archive.dir, "market",
                   frequency = "annual",
                   price.series = FALSE)
    checkEquals(row.names(data)[1], "1927")

    data <- French(archive.dir, "market",
                   frequency = "annual",
                   price.series = TRUE)
    checkEquals(row.names(data)[1], "1926")


    ## rf
    data <- French(archive.dir, "rf",
                   frequency = "daily",
                   price.series = FALSE)
    checkTrue(min(data) > -0.05/250)
    checkTrue(max(data) < 0.25/250)


    data <- French(archive.dir, "rf",
                   frequency = "daily",
                   price.series = TRUE)

    data <- French(archive.dir, "rf",
                   frequency = "monthly",
                   price.series = TRUE)
    data <- French(archive.dir, "rf",
                   frequency = "monthly",
                   price.series = FALSE)
    checkTrue(min(data) > -0.05/12)
    checkTrue(max(data) < 0.25/12)

    data <- French(archive.dir, "rf",
                   frequency = "annual",
                   price.series = TRUE)

    data <- French(archive.dir, "rf",
                   frequency = "annual",
                   price.series = FALSE)
    checkTrue(min(data) > -0.05)
    checkTrue(max(data) < 0.25)



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
                   price.series = FALSE, frequency = "daily")
    checkEquals(row.names(data)[1], "1926-11-03")
    checkEquals(row.names(data)[2], "1926-11-04")

    data <- French(archive.dir, "F-F_Momentum_Factor_CSV.zip",
                   price.series = FALSE)
    checkEquals(row.names(data)[1], "1927-01-31")

    ## Size and momentum
    data <- French(archive.dir, "6_Portfolios_ME_Prior_12_2_CSV.zip",
                   price.series = TRUE)

    data <- French(archive.dir, "6_Portfolios_ME_Prior_12_2_Daily_CSV.zip",
                   price.series = TRUE, frequency = "daily")



    files <- French()
    checkTrue(is.character(files))
    checkTrue(length(files) > 1)

    data <- French(archive.dir,
                   dataset = "49_Industry_Portfolios_daily_CSV.zip",
                   frequency = "daily",
                   weighting = "value")

    data <- French(archive.dir,
                   dataset = "48_Industry_Portfolios_daily_CSV.zip",
                   frequency = "daily",
                   weighting = "value",
                   price.series = TRUE,
                   na.rm = TRUE)

    ## Momentum
    data <- French(archive.dir,
                   "10_Portfolios_Prior_12_2_CSV.zip",
                   frequency = "monthly", weighting = "value")
    data <- French(archive.dir,
                   "10_Portfolios_Prior_12_2_CSV.zip",
                   frequency = "monthly", weighting = "equal")

    data <- French(archive.dir,
                   "10_Portfolios_Prior_12_2_Daily_CSV.zip",
                   frequency = "daily", weighting = "value")
    data <- French(archive.dir,
                   "10_Portfolios_Prior_12_2_Daily_CSV.zip",
                   frequency = "daily", weighting = "equal")


    ## NI and more
    data <- French(archive.dir,
                   "Portfolios_Formed_on_NI_CSV.zip",
                   frequency = "monthly", weighting = "value",
                   price.series = TRUE)

    data <- French(archive.dir,
                   "Portfolios_Formed_on_RESVAR_CSV.zip",
                   frequency = "monthly", weighting = "value")

    data <- French(archive.dir,
                   "Portfolios_Formed_on_VAR_CSV.zip",
                   frequency = "monthly", weighting = "value")

    data <- French(archive.dir,
                   "Portfolios_Formed_on_VAR_CSV.zip",
                   frequency = "annual", weighting = "value")

    data <- French(archive.dir,
                   "Portfolios_Formed_on_BE-ME_CSV.zip",
                   frequency = "annual", weighting = "value")

    data <- French(archive.dir,
                   "Portfolios_Formed_on_BE-ME_CSV.zip",
                   frequency = "annual", weighting = "value",
                   price.series = TRUE)


    for (d in French())
        data <- French(archive.dir, d)

    ignore <- French("~/Downloads/French",
                     dataset = "ME_Breakpoints_CSV.zip")
    ignore <- French("~/Downloads/French",
                     dataset = "6_Portfolios_2x3_CSV.zip")
    ignore <- French("~/Downloads/French",
                     dataset = "6_Portfolios_ME_OP_2x3_CSV.zip")
    ignore <- French("~/Downloads/French",
                     dataset = "6_Portfolios_ME_INV_2x3_CSV.zip")
    ignore <- French("~/Downloads/French",
                     dataset = "6_Portfolios_ME_CFP_2x3_CSV.zip")
    ignore <- French("~/Downloads/French",
                     dataset = "6_Portfolios_ME_DP_2x3_CSV.zip")

}

test.Shiller <- function() {

    ## library("NMOF")
    ## library("RUnit")

    archive.dir <- "~/Downloads/Shiller"
    if (!dir.exists(archive.dir))
        dir.create(archive.dir)

    Shiller(archive.dir)
}
