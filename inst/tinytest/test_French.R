if (Sys.getenv("ES_PACKAGE_TESTING_73179826243954") == "true") {

    archive.dir <- "~/Downloads/French"
    if (!dir.exists(archive.dir))
        dir.create(archive.dir)

    dataset <- "Portfolios_Formed_on_ME_Daily_CSV.zip"
    data <- French(archive.dir, dataset, price.series = TRUE)


    ##
    for (d in French())
        data <- French(archive.dir, d)


    ## Sector Definitions
    codes <- French("~/Downloads/French",
                    "Siccodes49.zip")
    expect_equal(colnames(codes),
                 c("num", "abbr", "industry", "industry_group", "start", "end"))


    codes <- French("~/Downloads/French",
                    "Siccodes12.zip")
    expect_equal(colnames(codes),
                 c("num", "abbr", "industry", "industry_group", "start", "end"))

}
